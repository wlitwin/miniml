/* x86_64 context switching for MiniML native backend.
 *
 * mml_context layout (see context.h):
 *   rbx: offset  0
 *   rbp: offset  8
 *   r12: offset 16
 *   r13: offset 24
 *   r14: offset 32
 *   r15: offset 40
 *   rsp: offset 48
 *   rip: offset 56
 */

/* ---- mml_swap_context(mml_context *from [rdi], mml_context *to [rsi]) ---- */
.globl _mml_swap_context
.globl mml_swap_context
_mml_swap_context:
mml_swap_context:
    /* Save callee-saved registers into *from (rdi) */
    movq %rbx,  0(%rdi)
    movq %rbp,  8(%rdi)
    movq %r12, 16(%rdi)
    movq %r13, 24(%rdi)
    movq %r14, 32(%rdi)
    movq %r15, 40(%rdi)
    /* Save stack pointer (RSP before our return address was pushed by call) */
    leaq 8(%rsp), %rax        /* RSP + 8 = caller's RSP */
    movq %rax, 48(%rdi)       /* save SP */
    /* Save return address (= caller's RIP, at top of stack from call) */
    movq (%rsp), %rax
    movq %rax, 56(%rdi)       /* save IP */

    /* Restore callee-saved registers from *to (rsi) */
    movq  0(%rsi), %rbx
    movq  8(%rsi), %rbp
    movq 16(%rsi), %r12
    movq 24(%rsi), %r13
    movq 32(%rsi), %r14
    movq 40(%rsi), %r15
    movq 48(%rsi), %rsp       /* restore SP */
    /* Jump to saved IP */
    jmpq *56(%rsi)

/* ---- mml_make_context(ctx [rdi], stack_base [rsi], stack_size [rdx],
                          fn [rcx], arg [r8]) ---- */
.globl _mml_make_context
.globl mml_make_context
_mml_make_context:
mml_make_context:
    /* Compute top of stack (stack grows down) */
    addq %rdx, %rsi           /* rsi = stack_base + stack_size = top */
    andq $-16, %rsi           /* 16-byte align */
    /* Push a fake return address (0) so fn() returning would fault cleanly */
    subq $8, %rsi
    movq $0, (%rsi)
    /* Set context fields */
    movq %rsi, 48(%rdi)       /* rsp = top of stack */
    leaq _mml_context_entry(%rip), %rax
    movq %rax, 56(%rdi)       /* rip = trampoline */
    movq %rcx, 0(%rdi)        /* rbx = fn (callee-saved, survives swap) */
    movq %r8,  8(%rdi)        /* rbp = arg (callee-saved, survives swap) */
    /* Zero remaining callee-saved to avoid garbage */
    movq $0, 16(%rdi)         /* r12 */
    movq $0, 24(%rdi)         /* r13 */
    movq $0, 32(%rdi)         /* r14 */
    movq $0, 40(%rdi)         /* r15 */
    ret

/* ---- Trampoline: entered when swapping into a newly made context ----
 * rbx = fn, rbp = arg (set by make_context, preserved across swap) */
_mml_context_entry:
mml_context_entry:
    movq %rbp, %rdi           /* arg -> first parameter */
    callq *%rbx               /* call fn(arg) */
    /* fn should never return; if it does, abort */
    ud2
