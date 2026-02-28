#ifndef MML_CONTEXT_H
#define MML_CONTEXT_H

#include <stdint.h>
#include <stddef.h>

#if defined(__x86_64__)
/* x86_64 callee-saved: RBX, RBP, R12-R15, RSP, RIP */
typedef struct mml_context {
    uint64_t rbx;       /* offset  0 */
    uint64_t rbp;       /* offset  8 */
    uint64_t r12;       /* offset 16 */
    uint64_t r13;       /* offset 24 */
    uint64_t r14;       /* offset 32 */
    uint64_t r15;       /* offset 40 */
    uint64_t rsp;       /* offset 48 */
    uint64_t rip;       /* offset 56 */
} mml_context;

#define MML_CTX_SP_OFFSET 48
#define MML_CTX_BP_OFFSET  8

#elif defined(__aarch64__) || defined(__arm64__)
/* ARM64 callee-saved: X19-X28, X29(FP), X30(LR), SP, D8-D15 */
typedef struct mml_context {
    uint64_t x19;       /* offset   0 */
    uint64_t x20;       /* offset   8 */
    uint64_t x21;       /* offset  16 */
    uint64_t x22;       /* offset  24 */
    uint64_t x23;       /* offset  32 */
    uint64_t x24;       /* offset  40 */
    uint64_t x25;       /* offset  48 */
    uint64_t x26;       /* offset  56 */
    uint64_t x27;       /* offset  64 */
    uint64_t x28;       /* offset  72 */
    uint64_t x29;       /* offset  80 -- frame pointer */
    uint64_t x30;       /* offset  88 -- link register (return address) */
    uint64_t sp;        /* offset  96 */
    uint64_t d8;        /* offset 104 */
    uint64_t d9;        /* offset 112 */
    uint64_t d10;       /* offset 120 */
    uint64_t d11;       /* offset 128 */
    uint64_t d12;       /* offset 136 */
    uint64_t d13;       /* offset 144 */
    uint64_t d14;       /* offset 152 */
    uint64_t d15;       /* offset 160 */
} mml_context;

#define MML_CTX_SP_OFFSET 96
#define MML_CTX_BP_OFFSET 80

#else
#error "Unsupported architecture for MML context switching"
#endif

/* Save current context to `from`, restore context from `to`.
 * Implemented in assembly (context_x86_64.S / context_arm64.S). */
void mml_swap_context(mml_context *from, mml_context *to);

/* Initialize `ctx` so that swapping to it starts executing `fn(arg)`
 * on the given stack. `stack_base` is the low address, `stack_size` is
 * the byte count. The function `fn` should never return. */
void mml_make_context(mml_context *ctx, void *stack_base, size_t stack_size,
                      void (*fn)(void *), void *arg);

#endif /* MML_CONTEXT_H */
