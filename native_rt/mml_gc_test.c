/* mml_gc_test.c: standalone stress test for the MPS object format in mml_gc.c.
 *
 * Builds a 300k-node graph through the moving AMC pool — each node is a
 * tuple -> variant -> cons(FLOAT, REF(STRING)) plus a CLOSURE with captures —
 * drops all but the spine head, forces repeated collections, and verifies the
 * structure and the descending FLOAT leaf values survived forwarding. Exercises
 * CONS/TUPLE/VARIANT/CLOSURE/REF/FLOAT/STRING, the client<->base fix conversion,
 * forwarding/padding, half-init zeroing, and (critically) MPS pinning of
 * ambiguous interior (client) pointers held in C locals + a global.
 *
 * Build: see the `test-mml-gc` Makefile target. Exit 0 = PASS.
 */
#include "mml_gc.h"
#include "runtime.h"
#include <stdio.h>
#include <string.h>

typedef int64_t word;
#define CLIENTBASE(v) (((word *)(intptr_t)(v))[-1])     /* header at client[-1] */
#define HTAG(v) ((int)(CLIENTBASE(v) & 0xFF))
#define TAG_INT(n) ((((word)(n)) << 1) | 1)

static word *cons(word hd, word tl)      { word *c = mml_gc_alloc(16, MML_MAKE_HDR(MML_HDR_CONS, 0));    c[0]=hd; c[1]=tl; return c; }
static word *tuple2(word a, word b)      { word *c = mml_gc_alloc(16, MML_MAKE_HDR(MML_HDR_TUPLE, 2));   c[0]=a; c[1]=b; return c; }
static word *variant(word ct, word pl)   { word *c = mml_gc_alloc(16, MML_MAKE_HDR(MML_HDR_VARIANT, 0)); c[0]=ct; c[1]=pl; return c; }
static word *mref(word v)                { word *c = mml_gc_alloc(8,  MML_MAKE_HDR(MML_HDR_REF, 0));      c[0]=v; return c; }
static word *fbox(double d)              { word *c = mml_gc_alloc(8,  MML_MAKE_HDR(MML_HDR_FLOAT, 0));    *(double *)&c[0]=d; return c; }
static word *closure2(word a, word b)    { word *c = mml_gc_alloc(40, MML_MAKE_HDR(MML_HDR_CLOSURE, 5));  c[0]=0xC0DE; c[1]=2; c[2]=0; c[3]=a; c[4]=b; return c; }
static word *str(const char *s) {
    size_t len = strlen(s);
    word *c = mml_gc_alloc((int64_t)(8 + len + 1), MML_MAKE_HDR(MML_HDR_STRING, 0));
    c[0] = ((word)len) << 1;            /* len<<1, written immediately (no safepoint before) */
    memcpy(&c[1], s, len + 1);
    return c;
}

static word *g_spine = NULL;            /* ambiguous global root holding a client pointer */

int main(void) {
    void *cold = &cold;
    mml_gc_init(cold);
    mml_gc_add_area_root(&g_spine, (char *)&g_spine + sizeof(g_spine));

    const int N = 300000;
    for (int i = 0; i < N; i++) {
        word *s  = str("leaf");
        word *f  = fbox((double)i);
        word *r  = mref((word)str("inner"));
        word *cn = cons((word)f, (word)r);
        word *v  = variant(TAG_INT(i & 7), (word)cn);
        word *cl = closure2((word)v, (word)s);
        word *t  = tuple2((word)v, (word)cl);
        g_spine  = cons((word)t, (word)g_spine);
        if ((i & 0x3FFF) == 0) mml_gc_collect();
    }
    mml_gc_collect();

    long count = 0; int ok = 1; word *p = g_spine;
    for (long expect = N - 1; p; expect--) {
        if (HTAG(p) != MML_HDR_CONS) { ok = 0; break; }
        word *t = (word *)p[0];
        if (HTAG(t) != MML_HDR_TUPLE)   { ok = 0; fprintf(stderr, "not tuple\n");   break; }
        word *v = (word *)t[0];
        if (HTAG(v) != MML_HDR_VARIANT) { ok = 0; fprintf(stderr, "not variant\n"); break; }
        word *cn = (word *)v[1];
        if (HTAG(cn) != MML_HDR_CONS)   { ok = 0; fprintf(stderr, "not cons\n");    break; }
        word *f = (word *)cn[0];
        if (HTAG(f) != MML_HDR_FLOAT)   { ok = 0; fprintf(stderr, "not float\n");   break; }
        if ((long)(*(double *)&f[0]) != expect) { ok = 0; fprintf(stderr, "float mismatch at %ld\n", count); break; }
        count++; p = (word *)p[1];
    }
    printf("spine length %ld (expect %d), leaves %s\n", count, N, (ok && count == N) ? "OK" : "BAD");
    printf("mml_gc MPS format validation: %s\n", (ok && count == N) ? "PASS" : "FAIL");
    return (ok && count == N) ? 0 : 1;
}
