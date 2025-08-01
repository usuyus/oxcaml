/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 2013 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

/* Asm part of the runtime system, ARM processor, 64-bit mode */
/* Must be preprocessed by cpp */

#include "caml/m.h"

/* Special registers */

#define DOMAIN_STATE_PTR x28
#define TRAP_PTR x26
#define ALLOC_PTR x27
#define ADDITIONAL_ARG x8
#define TMP x16
#define TMP2 x17

#define C_ARG_1 x0
#define C_ARG_2 x1
#define C_ARG_3 x2
#define C_ARG_4 x3

/* Support for CFI directives */

#if defined(ASM_CFI_SUPPORTED)
#define CFI_STARTPROC .cfi_startproc
#define CFI_ENDPROC .cfi_endproc
#define CFI_ADJUST(n) .cfi_adjust_cfa_offset n
#define CFI_REGISTER(r1,r2) .cfi_register r1,r2
#define CFI_OFFSET(r,n) .cfi_offset r,n
#else
#define CFI_STARTPROC
#define CFI_ENDPROC
#define CFI_ADJUST(n)
#define CFI_REGISTER(r1,r2)
#define CFI_OFFSET(r,n)
#endif

        .set    domain_curr_field, 0
#if defined(SYS_macosx)
#define DOMAIN_STATE(c_type, name) DOMAIN_STATE c_type, name
        .macro DOMAIN_STATE c_type, name
        .equ    domain_field_caml_\name, domain_curr_field
        .set    domain_curr_field, domain_curr_field + 1
        .endm
#else
#define DOMAIN_STATE(c_type, name) \
        .equ    domain_field_caml_##name, domain_curr_field ; \
        .set    domain_curr_field, domain_curr_field + 1
#endif
#include "../runtime4/caml/domain_state.tbl"
#undef DOMAIN_STATE

#define Caml_state(var) [DOMAIN_STATE_PTR, 8*domain_field_caml_##var]

/* Globals and labels */
#if defined(SYS_macosx)
#define G(sym) _##sym
#define L(lbl) L##lbl
#else
#define G(sym) sym
#define L(lbl) .L##lbl
#endif

#if defined(SYS_macosx)

#define ADDRGLOBAL(reg,symb) ADDRGLOBAL reg, symb
        .macro ADDRGLOBAL reg, symb
        adrp        TMP2, G(\symb)@GOTPAGE
        ldr         \reg, [TMP2, G(\symb)@GOTPAGEOFF]
        .endm
#elif defined(__PIC__)
#define ADDRGLOBAL(reg,symb) \
        adrp    TMP2, :got:G(symb); \
        ldr     reg, [TMP2, #:got_lo12:G(symb)]
#else

#define ADDRGLOBAL(reg,symb) \
        adrp    reg, G(symb); \
        add     reg, reg, #:lo12:G(symb)

#endif

#if defined(FUNCTION_SECTIONS)
#define TEXT_SECTION(name) .section .text.caml.##name,"ax",%progbits
#else
#define TEXT_SECTION(name)
#endif

#if defined(FUNCTION_SECTIONS)
        TEXT_SECTION(caml_hot__code_begin)
        .globl  G(caml_hot__code_begin)
G(caml_hot__code_begin):

        TEXT_SECTION(caml_hot__code_end)
        .globl  G(caml_hot__code_end)
G(caml_hot__code_end):
#endif

#if defined(SYS_macosx)

#define FUNCTION(name) FUNCTION name
        .macro FUNCTION name
        TEXT_SECTION(G(\name))
        .align 2
        .globl G(\name)
G(\name):
        .endm
#define END_FUNCTION(name)

#define OBJECT(name) OBJECT name
        .macro OBJECT name
        .data
        .align  3
        .globl  G(\name)
G(\name):
        .endm
#define END_OBJECT(name)

#else

#define FUNCTION(name) \
        TEXT_SECTION(name); \
        .align  2; \
        .globl  G(name); \
        .type   G(name), %function; \
G(name):
#define END_FUNCTION(name) \
        .size   G(name), .-G(name)

#define OBJECT(name) \
        .data; \
        .align  3; \
        .globl  G(name); \
        .type   G(name), %object; \
G(name):
#define END_OBJECT(name) \
        .size   G(name), .-G(name)
#endif

/* Allocation functions and GC interface */
        TEXT_SECTION(caml_system__code_begin)
        .globl  G(caml_system__code_begin)
G(caml_system__code_begin):

FUNCTION(caml_call_gc)
        CFI_STARTPROC
L(caml_call_gc):
    /* Record return address */
        str     x30, Caml_state(last_return_address)
    /* Record lowest stack address */
        mov     TMP, sp
        str     TMP, Caml_state(bottom_of_stack)
    /* Set up stack space, saving return address and frame pointer */
    /* (2 regs RA/GP, 24 allocatable int regs)*8 + (32 caller-save float regs)*16 */
        sub     sp, sp, #720
        CFI_ADJUST(720)
        CFI_OFFSET(29, 0)
        CFI_OFFSET(30, -8)
        stp     x29, x30, [sp]
        add     x29, sp, #0
    /* Save allocatable integer registers on the stack, in the order
       given in proc.ml */
        stp     x0, x1, [sp, 16]
        stp     x2, x3, [sp, 32]
        stp     x4, x5, [sp, 48]
        stp     x6, x7, [sp, 64]
        stp     x8, x9, [sp, 80]
        stp     x10, x11, [sp, 96]
        stp     x12, x13, [sp, 112]
        stp     x14, x15, [sp, 128]
        stp     x19, x20, [sp, 144]
        stp     x21, x22, [sp, 160]
        stp     x23, x24, [sp, 176]
        str     x25, [sp, 192]
     /* Save caller-save floating-point registers on the stack
        (callee-saves are preserved by caml_garbage_collection) */
        stp     q0, q1, [sp, 208]
        stp     q2, q3, [sp, 240]
        stp     q4, q5, [sp, 272]
        stp     q6, q7, [sp, 304]
        stp     q8, q9, [sp, 336]
        stp     q10, q11, [sp, 368]
        stp     q12, q13, [sp, 400]
        stp     q14, q15, [sp, 432]
        stp     q16, q17, [sp, 464]
        stp     q18, q19, [sp, 496]
        stp     q20, q21, [sp, 528]
        stp     q22, q23, [sp, 560]
        stp     q24, q25, [sp, 592]
        stp     q26, q27, [sp, 624]
        stp     q28, q29, [sp, 656]
        stp     q30, q31, [sp, 688]
    /* Store pointer to saved integer registers in Caml_state->gc_regs */
        add     TMP, sp, #16
        str     TMP, Caml_state(gc_regs)
    /* Save current allocation pointer for debugging purposes */
        str     ALLOC_PTR, Caml_state(young_ptr)
    /* Save trap pointer in case an exception is raised during GC */
        str     TRAP_PTR, Caml_state(exn_handler)
    /* Call the garbage collector */
        bl      G(caml_garbage_collection)
    /* Restore registers */
        ldp     x0, x1, [sp, 16]
        ldp     x2, x3, [sp, 32]
        ldp     x4, x5, [sp, 48]
        ldp     x6, x7, [sp, 64]
        ldp     x8, x9, [sp, 80]
        ldp     x10, x11, [sp, 96]
        ldp     x12, x13, [sp, 112]
        ldp     x14, x15, [sp, 128]
        ldp     x19, x20, [sp, 144]
        ldp     x21, x22, [sp, 160]
        ldp     x23, x24, [sp, 176]
        ldr     x25, [sp, 192]
        ldp     q0, q1, [sp, 208]
        ldp     q2, q3, [sp, 240]
        ldp     q4, q5, [sp, 272]
        ldp     q6, q7, [sp, 304]
        ldp     q8, q9, [sp, 336]
        ldp     q10, q11, [sp, 368]
        ldp     q12, q13, [sp, 400]
        ldp     q14, q15, [sp, 432]
        ldp     q16, q17, [sp, 464]
        ldp     q18, q19, [sp, 496]
        ldp     q20, q21, [sp, 528]
        ldp     q22, q23, [sp, 560]
        ldp     q24, q25, [sp, 592]
        ldp     q26, q27, [sp, 624]
        ldp     q28, q29, [sp, 656]
        ldp     q30, q31, [sp, 688]
    /* Reload new allocation pointer */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
    /* Free stack space and return to caller */
        ldp     x29, x30, [sp]
        add     sp, sp, #720
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_call_gc)

FUNCTION(caml_alloc1)
        CFI_STARTPROC
        ldr     TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, #16
        cmp     ALLOC_PTR, TMP
        b.lo    L(caml_call_gc)
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_alloc1)

FUNCTION(caml_alloc2)
        CFI_STARTPROC
        ldr     TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, #24
        cmp     ALLOC_PTR, TMP
        b.lo    L(caml_call_gc)
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_alloc2)

FUNCTION(caml_alloc3)
        CFI_STARTPROC
        ldr     TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, #32
        cmp     ALLOC_PTR, TMP
        b.lo    L(caml_call_gc)
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_alloc3)

FUNCTION(caml_allocN)
        CFI_STARTPROC
        ldr     TMP, Caml_state(young_limit)
        sub     ALLOC_PTR, ALLOC_PTR, ADDITIONAL_ARG
        cmp     ALLOC_PTR, TMP
        b.lo    L(caml_call_gc)
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_allocN)

FUNCTION(caml_call_local_realloc)
L(caml_call_local_realloc):
        CFI_STARTPROC
    /* Set up stack space, saving return address and frame pointer */
    /* Store return address and frame pointer */
    /* (2 RA/GP, 24 allocatable int regs)*8 + (32 caller-saved float regs)*16 */
        sub     sp, sp, #720
        CFI_ADJUST(720)
        CFI_OFFSET(29,0)
        CFI_OFFSET(30,-8)
        stp     x29, x30, [sp]
        add     x29, sp, #0

    /* Save allocatable integer registers on the stack, using order in proc.ml */
        stp     x0, x1, [sp, 16]
        stp     x2, x3, [sp, 32]
        stp     x4, x5, [sp, 48]
        stp     x6, x7, [sp, 64]
        stp     x8, x9, [sp, 80]
        stp     x10, x11, [sp, 96]
        stp     x12, x13, [sp, 112]
        stp     x14, x15, [sp, 128]
        stp     x19, x20, [sp, 144]
        stp     x21, x22, [sp, 160]
        stp     x23, x24, [sp, 176]
        str     x25, [sp, 192]

    /* Save caller saved floating-point registers on the stack */
        stp     q0, q1, [sp, 208]
        stp     q2, q3, [sp, 240]
        stp     q4, q5, [sp, 272]
        stp     q6, q7, [sp, 304]
        stp     q8, q9, [sp, 336]
        stp     q10, q11, [sp, 368]
        stp     q12, q13, [sp, 400]
        stp     q14, q15, [sp, 432]
        stp     q16, q17, [sp, 464]
        stp     q18, q19, [sp, 496]
        stp     q20, q21, [sp, 528]
        stp     q22, q23, [sp, 560]
        stp     q24, q25, [sp, 592]
        stp     q26, q27, [sp, 624]
        stp     q28, q29, [sp, 656]
        stp     q30, q31, [sp, 688]

    /* Store pointer to saved integer registers in Caml_state->gc_regs */
        add     TMP, sp, #16
        str     TMP, Caml_state(gc_regs)

    /* Save current allocation pointer for debugging purposes */
        str     ALLOC_PTR, Caml_state(young_ptr)

    /* Call the realloc function */
        bl      G(caml_local_realloc)

    /* Restore registers */
        ldp     x0, x1, [sp, 16]
        ldp     x2, x3, [sp, 32]
        ldp     x4, x5, [sp, 48]
        ldp     x6, x7, [sp, 64]
        ldp     x8, x9, [sp, 80]
        ldp     x10, x11, [sp, 96]
        ldp     x12, x13, [sp, 112]
        ldp     x14, x15, [sp, 128]
        ldp     x19, x20, [sp, 144]
        ldp     x21, x22, [sp, 160]
        ldp     x23, x24, [sp, 176]
        ldr     x25, [sp, 192]
        ldp     q0, q1, [sp, 208]
        ldp     q2, q3, [sp, 240]
        ldp     q4, q5, [sp, 272]
        ldp     q6, q7, [sp, 304]
        ldp     q8, q9, [sp, 336]
        ldp     q10, q11, [sp, 368]
        ldp     q12, q13, [sp, 400]
        ldp     q14, q15, [sp, 432]
        ldp     q16, q17, [sp, 464]
        ldp     q18, q19, [sp, 496]
        ldp     q20, q21, [sp, 528]
        ldp     q22, q23, [sp, 560]
        ldp     q24, q25, [sp, 592]
        ldp     q26, q27, [sp, 624]
        ldp     q28, q29, [sp, 656]
        ldp     q30, q31, [sp, 688]

    /* Reload new allocation pointer */
        ldr     ALLOC_PTR, Caml_state(young_ptr)

    /* Free stack space and return to caller */
        ldp     x29, x30, [sp]
        add     sp, sp, #720
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_call_local_realloc)

/* Call a C function from OCaml */
/* Function to call is in ADDITIONAL_ARG */

FUNCTION(caml_c_call)
        CFI_STARTPROC
    /* Preserve return address in callee-save register x19 */
        mov     x19, x30
        CFI_REGISTER(30, 19)
    /* Record lowest stack address and return address */
        str     x30, Caml_state(last_return_address)
        add     TMP, sp, #0
        str     TMP, Caml_state(bottom_of_stack)
    /* Make the exception handler alloc ptr available to the C code */
        str     ALLOC_PTR, Caml_state(young_ptr)
        str     TRAP_PTR, Caml_state(exn_handler)
    /* Call the function */
        blr     ADDITIONAL_ARG
    /* Reload alloc ptr  */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
    /* Return */
        ret     x19
        CFI_ENDPROC
        END_FUNCTION(caml_c_call)

/* Start the OCaml program */

FUNCTION(caml_start_program)
        CFI_STARTPROC
        mov     TMP, C_ARG_1
        ADDRGLOBAL(TMP2, caml_program)

/* Code shared with caml_callback* */
/* Address of domain state is in TMP */
/* Address of OCaml code to call is in TMP2 */
/* Arguments to the OCaml code are in x0...x7 */

L(jump_to_caml):
    /* Set up stack frame and save callee-save registers */
        CFI_OFFSET(29, -224)
        CFI_OFFSET(30, -216)
        stp     x29, x30, [sp, -224]!
        CFI_ADJUST(224)
        add     x29, sp, #0
        stp     x19, x20, [sp, 16]
        stp     x21, x22, [sp, 32]
        stp     x23, x24, [sp, 48]
        stp     x25, x26, [sp, 64]
        stp     x27, x28, [sp, 80]
        stp     q8, q9, [sp, 96]
        stp     q10, q11, [sp, 128]
        stp     q12, q13, [sp, 160]
        stp     q14, q15, [sp, 192]
    /* Load domain state pointer from argument */
        mov     DOMAIN_STATE_PTR, TMP
    /* Setup a callback link on the stack */
        ldr     x8, Caml_state(bottom_of_stack)
        ldr     x9, Caml_state(last_return_address)
        ldr     x10, Caml_state(gc_regs)
        ldr     x11, Caml_state(async_exception_pointer)
        stp     x8, x9, [sp, -32]!     /* 16-byte alignment */
        CFI_ADJUST(32)
        str     x10, [sp, 16]
        str     x11, [sp, 24]
    /* Setup a trap frame to catch exceptions escaping the OCaml code */
        ldr     x8, Caml_state(exn_handler)
        adr     x9, L(trap_handler)
        stp     x8, x9, [sp, -16]!
        CFI_ADJUST(16)
        add     TRAP_PTR, sp, #0
    /* Store the async exception pointer */
        str     TRAP_PTR, Caml_state(async_exception_pointer)
    /* Reload allocation pointer */
        ldr     ALLOC_PTR, Caml_state(young_ptr)
    /* Call the OCaml code */
        blr     TMP2
L(caml_retaddr):
    /* Pop the trap frame, restoring caml_exn_handler */
        ldr     x8, [sp], 16
        CFI_ADJUST(-16)
        str     x8, Caml_state(exn_handler)
    /* Pop the callback link, restoring the global variables */
L(return_result):
        ldr     x11, [sp, 24]
        ldr     x10, [sp, 16]
        ldp     x8, x9, [sp], 32
        CFI_ADJUST(-32)
        str     x8, Caml_state(bottom_of_stack)
        str     x9, Caml_state(last_return_address)
        str     x10, Caml_state(gc_regs)
        str     x11, Caml_state(async_exception_pointer)
    /* Update allocation pointer */
        str     ALLOC_PTR, Caml_state(young_ptr)
    /* Reload callee-save registers and return address */
        ldp     x19, x20, [sp, 16]
        ldp     x21, x22, [sp, 32]
        ldp     x23, x24, [sp, 48]
        ldp     x25, x26, [sp, 64]
        ldp     x27, x28, [sp, 80]
        ldp     q8, q9, [sp, 96]
        ldp     q10, q11, [sp, 128]
        ldp     q12, q13, [sp, 160]
        ldp     q14, q15, [sp, 192]
        ldp     x29, x30, [sp], 224
        CFI_ADJUST(-224)
    /* Return to C caller */
        ret
        CFI_ENDPROC
        END_FUNCTION(caml_start_program)

/* The trap handler */

        .align  2
L(trap_handler):
        CFI_STARTPROC
    /* Save exception pointer */
        str     TRAP_PTR, Caml_state(exn_handler)
    /* Encode exception bucket as an exception result */
        orr     x0, x0, #2
    /* Return it */
        b       L(return_result)
        CFI_ENDPROC

/* Raise an exception from OCaml */

FUNCTION(caml_raise_exn)
        CFI_STARTPROC
    /* Test if backtrace is active */
        ldr     TMP, Caml_state(backtrace_active)
        cbnz    TMP, 2f
1:  /* Cut stack at current trap handler */
        mov     sp, TRAP_PTR
    /* Pop previous handler and jump to it */
        ldr     TMP, [sp, 8]
        ldr     TRAP_PTR, [sp], 16
        br      TMP
2:  /* Preserve exception bucket in callee-save register x19 */
        mov     x19, x0
    /* Stash the backtrace */
                               /* arg1: exn bucket, already in x0 */
        mov     x1, x30        /* arg2: pc of raise */
        add     x2, sp, #0     /* arg3: sp of raise */
        mov     x3, TRAP_PTR   /* arg4: sp of handler */
        bl      G(caml_stash_backtrace)
    /* Restore exception bucket and raise */
        mov     x0, x19
        b       1b
        CFI_ENDPROC
        END_FUNCTION(caml_raise_exn)

/* Raise an exception from C */

FUNCTION(caml_raise_exception)
        CFI_STARTPROC
    /* Load the domain state ptr */
        mov     DOMAIN_STATE_PTR, C_ARG_1
    /* Load the exception bucket */
        mov     x0, C_ARG_2
    /* Reload trap ptr and alloc ptr */
        ldr     TRAP_PTR, Caml_state(exn_handler)
        ldr     ALLOC_PTR, Caml_state(young_ptr)
    /* Test if backtrace is active */
        ldr     TMP, Caml_state(backtrace_active)
        cbnz    TMP, 2f
1:  /* Cut stack at current trap handler */
        mov     sp, TRAP_PTR
    /* Pop previous handler and jump to it */
        ldr     TMP, [sp, 8]
        ldr     TRAP_PTR, [sp], 16
        br      TMP
2:  /* Preserve exception bucket in callee-save register x19 */
        mov     x19, x0
    /* Stash the backtrace */
                                                      /* arg1: exn bucket */
        ldr     x1, Caml_state(last_return_address)   /* arg2: pc of raise */
        ldr     x2, Caml_state(bottom_of_stack)       /* arg3: sp of raise */
        mov     x3, TRAP_PTR   /* arg4: sp of handler */
        bl      G(caml_stash_backtrace)
    /* Restore exception bucket and raise */
        mov     x0, x19
        b       1b
        CFI_ENDPROC
        END_FUNCTION(caml_raise_exception)

/* Raise a Stack_overflow exception on return from segv_handler()
   (in runtime/signals_nat.c).  On entry, the stack is full, so we
   cannot record a backtrace.
   No CFI information here since this function disrupts the stack
   backtrace anyway.
   Since we have returned from the signal handler, the DOMAIN_STATE_PTR,
   TRAP_PTR and ALLOC_PTR registers should have the same values
   they had in the faulting OCaml code, so don't try to reload them. */

FUNCTION(caml_stack_overflow)
    /* Load the exception bucket */
        ADDRGLOBAL(x0, caml_exn_Stack_overflow)
    /* Cut stack at current async exn trap handler */
        ldr     TMP, Caml_state(async_exception_pointer)
        mov     sp, TMP
    /* Pop handler and jump to it */
        ldr     TMP, [sp, 8]
        ldr     TRAP_PTR, [sp], 16
        br      TMP
        END_FUNCTION(caml_stack_overflow)

/* Callback from C to OCaml */

FUNCTION(caml_callback_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = first arg) */
        mov     TMP, x0
        ldr     x0, [x2]        /* x0 = first arg */
                                /* x1 = closure environment */
        ldr     TMP2, [x1]       /* code pointer */
        b       L(jump_to_caml)
        CFI_ENDPROC
        END_FUNCTION(caml_callback_asm)

FUNCTION(caml_callback2_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = arg1, [x2,8] = arg2) */
        mov     TMP, x0
        mov     TMP2, x1
        ldp     x0, x1, [x2, 0] /* x0 = first arg, x1 = second arg */
        mov     x2, TMP2         /* x2 = closure environment */
        ADDRGLOBAL(TMP2, caml_apply2L)
        b       L(jump_to_caml)
        CFI_ENDPROC
        END_FUNCTION(caml_callback2_asm)

FUNCTION(caml_callback3_asm)
        CFI_STARTPROC
    /* Initial shuffling of arguments */
    /* (x0 = Caml_state, x1 = closure, [x2] = arg1, [x2,8] = arg2,
        [x2,16] = arg3) */
        mov     TMP, x0
        mov     x3, x1          /* x3 = closure environment */
        ldp     x0, x1, [x2, 0] /* x0 = first arg, x1 = second arg */
        ldr     x2, [x2, 16]    /* x2 = third arg */
        ADDRGLOBAL(TMP2, caml_apply3L)
        b       L(jump_to_caml)
        CFI_ENDPROC
        END_FUNCTION(caml_callback3_asm)

FUNCTION(caml_ml_array_bound_error)
        CFI_STARTPROC
    /* Load address of [caml_array_bound_error] in ADDITIONAL_ARG */
        ADDRGLOBAL(ADDITIONAL_ARG, caml_array_bound_error)
    /* Call that function */
        b       G(caml_c_call)
        CFI_ENDPROC
        END_FUNCTION(caml_ml_array_bound_error)

         TEXT_SECTION(caml_system__code_end)
        .globl  G(caml_system__code_end)
G(caml_system__code_end):

/* GC roots for callback */

OBJECT(caml_system__frametable)
        .quad   1               /* one descriptor */
        .4byte  L(caml_retaddr) - . /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  3
        END_OBJECT(caml_system__frametable)

#if !defined(SYS_macosx)
/* Mark stack as non-executable */
        .section .note.GNU-stack,"",%progbits
#endif
