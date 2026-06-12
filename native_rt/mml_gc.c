/* mml_gc.c: MPS-backed moving/generational collector — object format + arena.
 *
 * See mml_gc.h for the model. The object format (scan/skip/fwd/isfwd/pad) is driven
 * by the MiniML header tags (runtime.h), so the heap is scanned precisely while
 * stacks are scanned ambiguously (conservatively) by MPS. Validated standalone by
 * native_rt/mml_gc_test.c before being wired into the runtime.
 */
#include "mml_gc.h"
#include "runtime.h"      /* MML_HDR_*, MML_MAKE_HDR, MML_IS_INT, MML_HDR_TAG/SIZE */

#include "mps.h"
#include "mpsavm.h"
#include "mpscamc.h"

#include <stdio.h>
#include <stdlib.h>
#include <mach-o/getsect.h>
#include <mach-o/dyld.h>

typedef mps_word_t word;

#define HDR_SZ 8                                   /* header bytes; client = base + 8 */
#define BASE(client) ((word *)((char *)(client) - HDR_SZ))
#define CLIENT(base) ((word *)((char *)(base) + HDR_SZ))
#define ALIGN_UP(n)  (((n) + 7) & ~(size_t)7)
#define TAG(h)  ((int)((h) & 0xFF))
#define SIZE(h) ((word)((word)(h) >> 16))

/* GC-internal markers, reusing the two header-tag bytes free above POLYVAR (0x0B).
   Never collide with a real MiniML tag (0x00..0x0B). */
#define MML_HDR_FWD 0x0C   /* forwarded: header size = original byte size; client[0] = new base */
#define MML_HDR_PAD 0x0D   /* padding:   header size = byte size */

/* --- object size in BYTES (header + data), from the object base --- */
static size_t mml_obj_bytes(word *base) {
    word h = base[0];
    word *c = CLIENT(base);
    switch (TAG(h)) {
    case MML_HDR_CONS: case MML_HDR_PAIR:
    case MML_HDR_VARIANT: case MML_HDR_POLYVAR: return HDR_SZ + 16;     /* 2 data words */
    case MML_HDR_REF: case MML_HDR_FLOAT:       return HDR_SZ + 8;      /* 1 data word  */
    case MML_HDR_TUPLE: case MML_HDR_RECORD:
    case MML_HDR_CLOSURE:                       return HDR_SZ + SIZE(h) * 8;
    case MML_HDR_ARRAY:  return HDR_SZ + (1 + (size_t)c[0]) * 8;        /* c[0] = raw length */
    case MML_HDR_STRING: return ALIGN_UP(HDR_SZ + 8 + (size_t)(c[0] >> 1) + 1); /* c[0]=len<<1 */
    case MML_HDR_FWD: case MML_HDR_PAD: return (size_t)SIZE(h);
    default:
        fprintf(stderr, "mml_gc: bad object tag 0x%x at %p\n", TAG(h), (void *)base);
        abort();
    }
}

static mps_addr_t mml_fmt_skip(mps_addr_t base) {
    return (mps_addr_t)((char *)base + mml_obj_bytes((word *)base));
}

/* Fix one reference slot holding a CLIENT pointer (or tagged-int / 0). Converts
   client->base for MPS_FIX, writes the relocated base->client back. */
#define FIX_REF(ss, slotp)                                       \
    do {                                                         \
        word _v = *(slotp);                                      \
        if (!MML_IS_INT(_v) && _v != 0) {                        \
            mps_addr_t _b = (mps_addr_t)BASE(_v);                \
            if (MPS_FIX1(ss, _b)) {                              \
                mps_res_t _r = MPS_FIX2(ss, &_b);                \
                if (_r != MPS_RES_OK) return _r;                 \
                *(slotp) = (word)CLIENT(_b);                     \
            }                                                    \
        }                                                        \
    } while (0)

static mps_res_t mml_fmt_scan(mps_ss_t ss, mps_addr_t base, mps_addr_t limit) {
    MPS_SCAN_BEGIN(ss) {
        word *b = base;
        while ((mps_addr_t)b < limit) {
            word h = b[0];
            word *c = CLIENT(b);
            size_t bytes = mml_obj_bytes(b);
            switch (TAG(h)) {
            case MML_HDR_CONS: case MML_HDR_PAIR:
                FIX_REF(ss, &c[0]); FIX_REF(ss, &c[1]); break;
            case MML_HDR_VARIANT: case MML_HDR_POLYVAR:
                FIX_REF(ss, &c[1]); break;                       /* c[0] = raw ctor tag */
            case MML_HDR_REF:
                FIX_REF(ss, &c[0]); break;
            case MML_HDR_TUPLE: case MML_HDR_RECORD: {
                word n = SIZE(h); for (word i = 0; i < n; i++) FIX_REF(ss, &c[i]); break;
            }
            case MML_HDR_CLOSURE: {                              /* slots 0..2 = fn/arity/applied */
                word n = SIZE(h); for (word i = 3; i < n; i++) FIX_REF(ss, &c[i]); break;
            }
            case MML_HDR_ARRAY: {                                /* c[0] = raw length */
                word n = (word)c[0]; for (word i = 0; i < n; i++) FIX_REF(ss, &c[1 + i]); break;
            }
            case MML_HDR_STRING: case MML_HDR_FLOAT: case MML_HDR_PAD:
                break;                                           /* atomic / no references */
            default:
                fprintf(stderr, "mml_gc: scan bad tag 0x%x\n", TAG(h)); abort();
            }
            b = (word *)((char *)b + bytes);
        }
    } MPS_SCAN_END(ss);
    return MPS_RES_OK;
}

static void mml_fmt_fwd(mps_addr_t old, mps_addr_t new_) {
    word *b = old;
    size_t bytes = mml_obj_bytes(b);             /* compute BEFORE overwriting the tag */
    b[0] = MML_MAKE_HDR(MML_HDR_FWD, (int64_t)bytes);
    CLIENT(b)[0] = (word)new_;                   /* new BASE in the first data word */
}

static mps_addr_t mml_fmt_isfwd(mps_addr_t base) {
    word *b = base;
    return TAG(b[0]) == MML_HDR_FWD ? (mps_addr_t)CLIENT(b)[0] : NULL;
}

static void mml_fmt_pad(mps_addr_t base, size_t size) {
    word *b = base;
    b[0] = MML_MAKE_HDR(MML_HDR_PAD, (int64_t)size);   /* size >= 8, fits a word */
}

/* --- arena / pool / allocation point --- */

static mps_arena_t mml_arena;
static mps_pool_t  mml_pool;
static mps_ap_t    mml_ap;
static mps_fmt_t   mml_fmt;
static mps_thr_t   mml_thread;
static mps_root_t  mml_stack_root;

/* Stress mode (MML_GC_STRESS=N): force a full collection every N allocations, so
   the moving/forwarding path is exercised even by tiny programs that would never
   fill a generation. For correctness testing only — extremely slow. */
static long mml_stress_n = 0;
static long mml_stress_ctr = 0;

static void die(const char *what, mps_res_t res) {
    fprintf(stderr, "mml_gc: %s failed (res %d)\n", what, (int)res);
    exit(1);
}

void mml_gc_init(void *stack_cold) {
    mps_res_t res;
    /* MPS requires the cold stack marker word-aligned. Round UP (away from the hot
       end) so the scanned range never shrinks below a real root. */
    stack_cold = (void *)(((uintptr_t)stack_cold + 7) & ~(uintptr_t)7);
    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_ARENA_SIZE, (size_t)256 * 1024 * 1024);
        res = mps_arena_create_k(&mml_arena, mps_arena_class_vm(), args);
    } MPS_ARGS_END(args);
    if (res != MPS_RES_OK) die("arena_create", res);

    if ((res = mps_thread_reg(&mml_thread, mml_arena)) != MPS_RES_OK) die("thread_reg", res);
    res = mps_root_create_thread_scanned(&mml_stack_root, mml_arena, mps_rank_ambig(), 0,
                                         mml_thread, mps_scan_area, NULL, stack_cold);
    if (res != MPS_RES_OK) die("stack root", res);

    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FMT_ALIGN, 8);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_SCAN,  mml_fmt_scan);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_SKIP,  mml_fmt_skip);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_FWD,   mml_fmt_fwd);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_ISFWD, mml_fmt_isfwd);
        MPS_ARGS_ADD(args, MPS_KEY_FMT_PAD,   mml_fmt_pad);
        res = mps_fmt_create_k(&mml_fmt, mml_arena, args);
    } MPS_ARGS_END(args);
    if (res != MPS_RES_OK) die("fmt_create", res);

    /* A large nursery is the key macOS tuning: AMC's generational write barrier uses
       memory protection (Mach exceptions here), so every first write to a promoted
       old-generation object faults — expensive. A big gen-0 means most objects live
       and die in the nursery without ever being promoted/protected, slashing the
       barrier-fault and collection overhead. (capacity in KB; mortality = fraction
       expected to die.) */
    static mps_gen_param_s gens[] = {
        { 128u * 1024, 0.99 },   /* gen-0 nursery: 128 MB, ~all dies young */
        { 256u * 1024, 0.80 },   /* gen-1 */
    };
    mps_chain_t chain;
    if ((res = mps_chain_create(&chain, mml_arena, 2, gens)) != MPS_RES_OK) die("chain", res);

    MPS_ARGS_BEGIN(args) {
        MPS_ARGS_ADD(args, MPS_KEY_FORMAT, mml_fmt);
        MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
        res = mps_pool_create_k(&mml_pool, mml_arena, mps_class_amc(), args);
    } MPS_ARGS_END(args);
    if (res != MPS_RES_OK) die("pool_create", res);

    if ((res = mps_ap_create_k(&mml_ap, mml_pool, mps_args_none)) != MPS_RES_OK) die("ap_create", res);

    /* Register MiniML's static globals as ambiguous roots. Top-level bindings and
       typeclass-dictionary slots are LLVM globals (@mml_g_*) holding mml_values; the
       codegen places them all in the "__DATA,__mmlgc" section so we can register
       exactly that region. Boehm scans static data automatically, but MPS does not —
       without this a global whose target is moved is left dangling. We must NOT
       register the whole __DATA segment: it also holds MPS's own static handles
       (mml_arena, mml_pool, …), and scanning those tangles the collector (it hangs). */
    {
        const struct mach_header_64 *mh =
            (const struct mach_header_64 *)_dyld_get_image_header(0);
        unsigned long sz = 0;
        uint8_t *p = getsectiondata(mh, "__DATA", "__mmlgc", &sz);
        if (p && sz) mml_gc_add_area_root(p, p + sz);
    }

    {
        const char *s = getenv("MML_GC_STRESS");
        if (s && *s) mml_stress_n = atol(s);
    }
}

void *mml_gc_alloc(int64_t nbytes, int64_t header) {
    if (mml_stress_n && ++mml_stress_ctr >= mml_stress_n) {
        mml_stress_ctr = 0;
        mps_arena_collect(mml_arena);
        mps_arena_release(mml_arena);
    }
    size_t bytes = ALIGN_UP(HDR_SZ + (size_t)nbytes);
    mps_addr_t p;
    do {
        mps_res_t res = mps_reserve(&p, mml_ap, bytes);
        if (res != MPS_RES_OK) die("reserve", res);
        word *b = p;
        b[0] = (word)header;
        for (size_t i = 1; i < bytes / 8; i++) b[i] = 0;   /* zero data words */
    } while (!mps_commit(mml_ap, p, bytes));
    return CLIENT(p);
}

void *mml_gc_add_area_root(void *base, void *limit) {
    mps_root_t root;
    mps_res_t res = mps_root_create_area(&root, mml_arena, mps_rank_ambig(), 0,
                                         base, limit, mps_scan_area, NULL);
    if (res != MPS_RES_OK) die("area root", res);
    return (void *)root;
}

void mml_gc_remove_area_root(void *handle) {
    mps_root_destroy((mps_root_t)handle);
}

void mml_gc_collect(void) {
    mps_arena_collect(mml_arena);
    mps_arena_release(mml_arena);
}
