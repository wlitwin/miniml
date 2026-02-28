#ifndef MML_RUNTIME_H
#define MML_RUNTIME_H

#include <stdint.h>
#include <gc/gc.h>

/* ---- Value representation ----
 *
 * All MiniML values are represented as a single i64 (int64_t) using
 * tagged pointers:
 *
 *   Integers:  (n << 1) | 1    -- low bit set = immediate integer (63-bit)
 *   Pointers:  aligned ptr     -- low bit clear = heap object pointer
 *   Unit:      tagged 0 = 1
 *
 * This matches the OCaml tagging convention.
 */

typedef int64_t mml_value;

#define MML_IS_INT(v)    ((v) & 1)
#define MML_INT_VAL(v)   ((int64_t)(v) >> 1)
#define MML_TAG_INT(n)   (((int64_t)(n) << 1) | 1)

#define MML_UNIT         MML_TAG_INT(0)
#define MML_TRUE         MML_TAG_INT(1)   /* 3 */
#define MML_FALSE        MML_TAG_INT(0)   /* 1 */

/* ---- String representation ----
 * Strings are heap-allocated as: [length (int64_t)] [data bytes] ['\0']
 * mml_value points to the start (the length field).
 * length is raw (not tagged), trailing NUL is NOT counted in length.
 */
#define MML_STR_LEN(v)   (*(int64_t *)(intptr_t)(v) >> 1)
#define MML_STR_DATA(v)  ((const char *)((int64_t *)(intptr_t)(v) + 1))

/* ---- Array representation ----
 * Arrays are flat mutable buffers: [length (int64_t)] [elem_0] ... [elem_{n-1}]
 * mml_value points to the start (the length field).
 * length is a raw int64 (not tagged).
 * Empty array (#[]) is represented as MML_UNIT (tagged 1) — no allocation.
 */
#define MML_ARR_LEN(v)   (*(int64_t *)(intptr_t)(v))
#define MML_ARR_DATA(v)  ((mml_value *)((int64_t *)(intptr_t)(v) + 1))

/* ---- Header word (at ptr[-1] for all heap objects) ----
 * Every heap-allocated MML value has a header word stored at ptr[-1].
 * The allocator reserves 8 extra bytes, writes the header, and returns
 * a pointer past it, so all field indices remain unchanged.
 *
 * Header format:
 *   Bits 63-16: size (element count for tuples/records; 0 for fixed-layout types)
 *   Bits 15-8:  reserved
 *   Bits  7-0:  type tag
 */
#define MML_HEADER(v)     (((int64_t *)(intptr_t)(v))[-1])
#define MML_HDR_TAG(v)    ((int)(MML_HEADER(v) & 0xFF))
#define MML_HDR_SIZE(v)   ((int64_t)(MML_HEADER(v) >> 16))
#define MML_MAKE_HDR(tag, size) (((int64_t)(size) << 16) | (tag))

#define MML_HDR_CONS      0x00
#define MML_HDR_TUPLE     0x01
#define MML_HDR_RECORD    0x02
#define MML_HDR_CLOSURE   0x03
#define MML_HDR_VARIANT   0x04
#define MML_HDR_ARRAY     0x05
#define MML_HDR_STRING    0x06
#define MML_HDR_FLOAT     0x07
/* 0x08 was MML_HDR_AVL, now unused */
#define MML_HDR_PAIR      0x09
#define MML_HDR_REF       0x0A
#define MML_HDR_POLYVAR   0x0B

/* Result type tags — set by codegen in @mml_result_type */
#define MML_RESULT_INT    0
#define MML_RESULT_BOOL   1
#define MML_RESULT_STRING 2
#define MML_RESULT_FLOAT  3
#define MML_RESULT_UNIT   4
#define MML_RESULT_COMPOUND 5
#define MML_RESULT_BYTE   6
#define MML_RESULT_RUNE   7

/* Runtime functions callable from generated LLVM IR */
mml_value mml_print_int(mml_value v);
mml_value mml_print_bool(mml_value v);
mml_value mml_print_string(mml_value v);
mml_value mml_print_float(mml_value v);
mml_value mml_print_unit(mml_value v);
mml_value mml_print_value(mml_value v);

mml_value mml_string_concat(mml_value a, mml_value b);
mml_value mml_string_eq(mml_value a, mml_value b);
mml_value mml_string_compare(mml_value a, mml_value b);
mml_value mml_string_length(mml_value s);
mml_value mml_string_of_int(mml_value v);

mml_value mml_box_float(double d);
double    mml_unbox_float(mml_value v);
mml_value mml_float_of_int(mml_value v);
mml_value mml_int_of_float(mml_value v);

void __attribute__((noreturn)) mml_panic(const char *msg);
void mml_panic_mml(mml_value s);
void mml_check_div_zero(int64_t divisor);

/* String allocation helpers */
mml_value mml_string_alloc(int64_t len);
mml_value mml_string_from_cstr(const char *s);
mml_value mml_string_from_buf(const char *buf, int64_t len);

/* Heap allocation: reserves header word at ptr[-1], returns pointer past it */
void *mml_alloc(int64_t nbytes, int64_t header);

/* Format functions (print without newline, for compound result printing) */
void mml_fmt_int(mml_value v);
void mml_fmt_bool(mml_value v);
void mml_fmt_string(mml_value v);
void mml_fmt_float(mml_value v);
void mml_fmt_str(const char *s);
void mml_fmt_str_raw(mml_value v);
void mml_fmt_byte(mml_value v);
void mml_fmt_rune(mml_value v);
void mml_fmt_newline(void);

/* Physical equality */
mml_value mml_phys_equal(mml_value a, mml_value b);

/* String.of_byte */
mml_value mml_string_of_byte(mml_value v);

/* Show support (string_of_*) */
mml_value mml_string_of_float(mml_value v);
mml_value mml_string_of_bool(mml_value v);
mml_value mml_show_unit(mml_value v);
mml_value mml_identity(mml_value v);

/* List operations */
mml_value mml_list_rev(mml_value lst);
mml_value mml_list_length(mml_value lst);
mml_value mml_list_map(mml_value fn, mml_value lst);
mml_value mml_list_fold(mml_value fn, mml_value acc, mml_value lst);

mml_value mml_list_filter(mml_value fn, mml_value lst);
mml_value mml_list_find(mml_value fn, mml_value lst);
mml_value mml_list_find_map(mml_value fn, mml_value lst);
mml_value mml_list_exists(mml_value fn, mml_value lst);
mml_value mml_list_forall(mml_value fn, mml_value lst);
mml_value mml_list_iter(mml_value fn, mml_value lst);
mml_value mml_list_mapi(mml_value fn, mml_value lst);
mml_value mml_list_concat(mml_value a, mml_value b);
mml_value mml_list_flatten(mml_value lst);
mml_value mml_list_sort(mml_value fn, mml_value lst);
mml_value mml_list_assoc_opt(mml_value key, mml_value lst);
mml_value mml_list_fold_right(mml_value fn, mml_value lst, mml_value acc);
mml_value mml_list_init(mml_value n, mml_value fn);
mml_value mml_list_map2(mml_value fn, mml_value lst1, mml_value lst2);
mml_value mml_list_iter2(mml_value fn, mml_value lst1, mml_value lst2);

/* Breakable fold (for break-in-fold support) */
mml_value mml_list_fold_breakable(mml_value fn, mml_value acc, mml_value lst);
void mml_fold_break(mml_value val);
int64_t mml_check_fold_broken(void);
mml_value mml_consume_fold_break(void);

/* Early return (for return inside for-in loops) */
void mml_set_early_return(mml_value val);
int64_t mml_check_early_return(void);
mml_value mml_get_early_return(void);

/* Index operations */
mml_value mml_list_nth(mml_value idx, mml_value lst);
mml_value mml_string_get_byte(mml_value idx, mml_value str);

/* Array operations (flat mutable buffers) */
mml_value mml_array_length(mml_value arr);
mml_value mml_array_get(mml_value idx, mml_value arr);
mml_value mml_array_set(mml_value arr, mml_value idx, mml_value val);
mml_value mml_array_make(mml_value n, mml_value val);
mml_value mml_array_of_list(mml_value lst);
mml_value mml_array_to_list(mml_value arr);
mml_value mml_array_fold(mml_value fn, mml_value acc, mml_value arr);
mml_value mml_array_fold_breakable(mml_value fn, mml_value acc, mml_value arr);
mml_value mml_array_eq(mml_value a, mml_value b);
mml_value mml_array_copy(mml_value arr);
mml_value mml_array_init(mml_value n, mml_value fn);
mml_value mml_array_map(mml_value fn, mml_value arr);
mml_value mml_array_mapi(mml_value fn, mml_value arr);
mml_value mml_array_iter(mml_value fn, mml_value arr);

/* Association list helpers for map pattern matching */
mml_value mml_assoc_has(mml_value key, mml_value alist);
mml_value mml_assoc_get(mml_value key, mml_value alist);

/* Rune/String extra operations */
mml_value mml_rune_to_string(mml_value cp);
mml_value mml_string_rune_length(mml_value s);
mml_value mml_string_of_bytes(mml_value lst);
mml_value mml_string_of_runes(mml_value lst);
mml_value mml_string_sub(mml_value s, mml_value start, mml_value len);
mml_value mml_string_contains(mml_value s, mml_value sub);
mml_value mml_string_to_list(mml_value s);

/* String.get_rune, String.to_bytes, String.to_runes */
mml_value mml_string_get_rune(mml_value idx, mml_value s);
mml_value mml_string_to_bytes(mml_value s);
mml_value mml_string_to_runes(mml_value s);

/* Generic show (runtime heuristic) */
mml_value mml_show_value(mml_value v);
mml_value mml_show_byte(mml_value v);
mml_value mml_show_rune(mml_value v);

/* Show for poly variants */
mml_value mml_show_polyvariant(mml_value names_list, mml_value show_payload_fn, mml_value value);

/* Show for records */
mml_value mml_show_record(mml_value fields_list, mml_value record);
mml_value mml_show_tuple(mml_value show_fns_list, mml_value tuple);
mml_value mml_show_variant(mml_value ctors_list, mml_value value);

/* Show for compound types */
mml_value mml_show_list(mml_value show_elem_fn, mml_value list);
mml_value mml_show_array(mml_value show_elem_fn, mml_value array);

/* Structural equality */
mml_value mml_structural_eq(mml_value a, mml_value b);
mml_value mml_list_eq(mml_value a, mml_value b);
mml_value mml_tuple_eq(mml_value a, mml_value b, mml_value n);
mml_value mml_record_eq(mml_value a, mml_value b);

/* Math builtins */
mml_value mml_math_pow(mml_value a, mml_value b);
mml_value mml_math_sqrt(mml_value a);
mml_value mml_math_floor(mml_value a);
mml_value mml_math_ceil(mml_value a);
mml_value mml_math_round(mml_value a);

/* Byte builtins */
mml_value mml_byte_of_int(mml_value v);
mml_value mml_byte_to_int(mml_value v);
mml_value mml_byte_to_string(mml_value v);

/* Format builtins */
mml_value mml_fmt_float_str(mml_value prec, mml_value val);
mml_value mml_fmt_hex(mml_value val);
mml_value mml_fmt_hex_upper(mml_value val);
mml_value mml_fmt_oct(mml_value val);
mml_value mml_fmt_bin(mml_value val);
mml_value mml_fmt_zero_pad(mml_value width, mml_value str);
mml_value mml_fmt_pad_left(mml_value width, mml_value str);
mml_value mml_fmt_pad_right(mml_value width, mml_value str);

/* String extra builtins */
mml_value mml_string_split(mml_value str, mml_value delim);
mml_value mml_string_trim(mml_value str);
mml_value mml_string_starts_with(mml_value str, mml_value prefix);
mml_value mml_string_replace(mml_value str, mml_value old_s, mml_value new_s);
mml_value mml_string_to_int(mml_value str);
mml_value mml_string_to_float(mml_value str);
mml_value mml_string_uppercase(mml_value str);
mml_value mml_string_lowercase(mml_value str);
mml_value mml_string_to_byte_array(mml_value str);
mml_value mml_string_of_byte_array(mml_value arr);
mml_value mml_string_make(mml_value n, mml_value ch);
mml_value mml_string_index_opt(mml_value str, mml_value sub);
mml_value mml_string_rindex_opt(mml_value str, mml_value sub);
mml_value mml_string_concat_list(mml_value sep, mml_value lst);
mml_value mml_string_compare_str(mml_value a, mml_value b);
mml_value mml_array_sub(mml_value arr, mml_value start, mml_value len);

/* Compound result printing (generated by codegen, declared here for main) */
extern void mml_format_result(mml_value v);

/* ---- Closure application ----
 *
 * Closure layout: [fn_ptr, arity, num_applied, captures_or_saved_args...]
 *   - Fresh (num_applied=0): slots 3+ are captured free variables
 *   - PAP (num_applied>0):   slot 3 = original closure ptr, slots 4+ = saved args
 *
 * fn_ptr always takes (void *env, arg0, arg1, ..., arg_{arity-1}).
 * env points to the original (root) closure, not the PAP.
 */

/* Generic apply: handles exact, under, and over-application */
mml_value mml_apply(mml_value closure, mml_value *args, int64_t n);

/* Dispatch call through fn pointer with arity-based switch */
mml_value mml_call_fn(void *fn, void *env, mml_value *args, int64_t n);

/* Convenience wrappers (avoid stack arrays in LLVM IR) */
mml_value mml_apply1(mml_value cls, mml_value a0);
mml_value mml_apply2(mml_value cls, mml_value a0, mml_value a1);
mml_value mml_apply3(mml_value cls, mml_value a0, mml_value a1, mml_value a2);
mml_value mml_apply4(mml_value cls, mml_value a0, mml_value a1, mml_value a2, mml_value a3);
mml_value mml_apply5(mml_value cls, mml_value a0, mml_value a1, mml_value a2, mml_value a3, mml_value a4);
mml_value mml_apply6(mml_value cls, mml_value a0, mml_value a1, mml_value a2, mml_value a3, mml_value a4, mml_value a5);

/* ---- Algebraic effect handler stack ---- */

#include <setjmp.h>
#include "context.h"

#define MML_HANDLER_PROVIDE  0
#define MML_HANDLER_TRY      1
#define MML_HANDLER_FULL     2

typedef struct mml_op_entry {
    const char* name;           /* NUL-terminated operation name */
    int kind;                   /* MML_HANDLER_PROVIDE / TRY / FULL */
    int64_t fn_ptr;             /* handler arm function pointer as i64 */
    int64_t env_ptr;            /* capture env array pointer as i64, or 0 */
} mml_op_entry;

typedef struct mml_handler {
    struct mml_handler* parent;
    int num_ops;
    int64_t return_fn;          /* return arm function pointer as i64 */
    int64_t return_env;         /* return arm env pointer as i64, or 0 */
    jmp_buf try_jmp;            /* setjmp buffer for try-handler unwind */
    int64_t try_result;         /* result value set before longjmp */
    int has_try_arms;           /* 1 if any arm is TRY kind */
    mml_op_entry ops[];         /* flexible array of operations */
} mml_handler;

extern mml_handler* mml_current_handler;

mml_handler* mml_alloc_handler(int64_t num_ops);
void mml_push_handler(mml_handler* h);
void mml_pop_handler(void);
void mml_handler_set_op(mml_handler* h, int64_t index, const char* name,
                        int64_t kind, int64_t fn_ptr, int64_t env_ptr);
void mml_handler_set_return(mml_handler* h, int64_t fn_ptr, int64_t env_ptr);
int64_t mml_handler_setjmp(mml_handler* h);
int64_t mml_handler_get_try_result(mml_handler* h);
int64_t mml_perform_op(const char* op_name, int64_t arg);

/* Run handler body with setjmp for try handlers */
int64_t mml_run_try_handler(int64_t handler_i64,
                            int64_t body_fn_i64, int64_t body_env_i64,
                            int64_t return_fn_i64, int64_t return_env_i64);

/* ---- Fiber infrastructure for full effect handlers (THOp) ---- */

#define MML_FIBER_STACK_SIZE (256 * 1024)  /* 256 KB per fiber */

/* Fiber states */
#define MML_FIBER_RUNNING   0
#define MML_FIBER_YIELDED   1  /* suspended at perform */
#define MML_FIBER_COMPLETED 2

typedef struct mml_fiber {
    mml_context ctx;          /* fiber context */
    mml_context *parent_ctx;  /* parent context to return to */
    void *stack;              /* heap-allocated stack */
    int64_t result;           /* body result or resumed value */
    const char *op_name;      /* pending perform op name */
    int64_t op_arg;           /* pending perform argument */
    int state;                /* MML_FIBER_RUNNING/YIELDED/COMPLETED */
} mml_fiber;

typedef struct mml_continuation {
    mml_fiber *fiber;         /* the suspended fiber */
    mml_handler *handler;     /* handler to reinstall on resume */
    int64_t return_fn;        /* return arm function pointer (or 0 for identity) */
    int64_t return_env;       /* return arm env pointer */
    int used;                 /* one-shot enforcement */
} mml_continuation;

/* Current fiber (NULL when running on main stack) */
extern mml_fiber *mml_current_fiber;

/* Run a full handler body on a fiber, dispatching effects to handler arms.
 * Returns the final result (after return arm). */
int64_t mml_run_full_handler(int64_t handler_i64,
                             int64_t body_fn_i64, int64_t body_env_i64,
                             int64_t return_fn_i64, int64_t return_env_i64);

/* Resume a continuation with a value */
int64_t mml_resume_continuation(int64_t k_i64, int64_t val);

/* Copy a continuation (for multi-shot) */
int64_t mml_copy_continuation(int64_t k_i64);

#endif /* MML_RUNTIME_H */
