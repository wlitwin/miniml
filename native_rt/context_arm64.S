/* ARM64 context switching for MiniML native backend.
 *
 * mml_context layout (see context.h):
 *   x19-x28: offsets 0-72    (callee-saved GPRs)
 *   x29(FP): offset 80
 *   x30(LR): offset 88
 *   SP:      offset 96
 *   d8-d15:  offsets 104-160  (callee-saved FP regs)
 */

/* ---- mml_swap_context(mml_context *from [x0], mml_context *to [x1]) ---- */
.globl _mml_swap_context
.globl mml_swap_context
.align 2
_mml_swap_context:
mml_swap_context:
    /* Save callee-saved GPRs to *from (x0) */
    stp x19, x20, [x0, #0]
    stp x21, x22, [x0, #16]
    stp x23, x24, [x0, #32]
    stp x25, x26, [x0, #48]
    stp x27, x28, [x0, #64]
    stp x29, x30, [x0, #80]
    mov x2, sp
    str x2, [x0, #96]
    /* Save callee-saved FP regs */
    stp d8,  d9,  [x0, #104]
    stp d10, d11, [x0, #120]
    stp d12, d13, [x0, #136]
    stp d14, d15, [x0, #152]

    /* Restore callee-saved GPRs from *to (x1) */
    ldp x19, x20, [x1, #0]
    ldp x21, x22, [x1, #16]
    ldp x23, x24, [x1, #32]
    ldp x25, x26, [x1, #48]
    ldp x27, x28, [x1, #64]
    ldp x29, x30, [x1, #80]
    ldr x2, [x1, #96]
    mov sp, x2
    /* Restore callee-saved FP regs */
    ldp d8,  d9,  [x1, #104]
    ldp d10, d11, [x1, #120]
    ldp d12, d13, [x1, #136]
    ldp d14, d15, [x1, #152]
    /* Return to restored x30 (LR) */
    ret

/* ---- mml_make_context(ctx [x0], stack_base [x1], stack_size [x2],
                          fn [x3], arg [x4]) ---- */
.globl _mml_make_context
.globl mml_make_context
.align 2
_mml_make_context:
mml_make_context:
    /* Compute top of stack, 16-byte aligned */
    add x1, x1, x2           /* x1 = stack_base + stack_size */
    and x1, x1, #~0xF        /* 16-byte align */
    /* Set SP */
    str x1, [x0, #96]        /* ctx->sp */
    /* Set LR to trampoline */
    adrp x5, _mml_context_entry@PAGE
    add x5, x5, _mml_context_entry@PAGEOFF
    str x5, [x0, #88]        /* ctx->x30 (LR) = trampoline */
    /* Stash fn and arg in callee-saved registers */
    str x3, [x0, #0]         /* ctx->x19 = fn */
    str x4, [x0, #8]         /* ctx->x20 = arg */
    /* Set FP to 0 (frame chain terminator) */
    str xzr, [x0, #80]       /* ctx->x29 = 0 */
    /* Zero other callee-saved regs */
    stp xzr, xzr, [x0, #16]  /* x21, x22 */
    stp xzr, xzr, [x0, #32]  /* x23, x24 */
    stp xzr, xzr, [x0, #48]  /* x25, x26 */
    stp xzr, xzr, [x0, #64]  /* x27, x28 */
    stp xzr, xzr, [x0, #104] /* d8, d9 */
    stp xzr, xzr, [x0, #120] /* d10, d11 */
    stp xzr, xzr, [x0, #136] /* d12, d13 */
    stp xzr, xzr, [x0, #152] /* d14, d15 */
    ret

/* ---- Trampoline: entered when swapping into a newly made context ----
 * x19 = fn, x20 = arg (set by make_context, preserved across swap) */
.align 2
_mml_context_entry:
mml_context_entry:
    mov x0, x20               /* arg -> first parameter */
    blr x19                   /* call fn(arg) */
    brk #1                    /* fn must not return; trap if it does */
