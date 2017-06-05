/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1998 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id: arm.S 9252 2009-05-04 13:46:46Z xleroy $ */

/* Asm part of the runtime system, ARM processor */

/* Linux/BSD with ELF binaries and Solaris do not prefix identifiers with _.
   Linux/BSD with a.out binaries and NextStep do.
   Copied from asmrun/i386.S */

#if defined(SYS_solaris)
#define CONCAT(a,b) a/**/b
#else
#define CONCAT(a,b) a##b
#endif

#if defined(SYS_linux_elf) || defined(SYS_bsd_elf) \
 || defined(SYS_solaris) || defined(SYS_beos) || defined(SYS_gnu)
#define G(x) x
#define LBL(x) CONCAT(.L,x)
#else
#define G(x) CONCAT(_,x)
#define LBL(x) CONCAT(L,x)
#endif

trap_ptr        .req    r11
alloc_ptr       .req    r8
alloc_limit     .req    r10

        .text

/* Allocation functions and GC interface */

        .globl G(caml_call_gc)
G(caml_call_gc):
    /* Record return address and desired size */
    /* Can use alloc_limit as a temporary since it will be reloaded by
       invoke_gc */
        ldr     alloc_limit, LBL(caml_last_return_address)
        str     lr, [alloc_limit, #0]
        ldr     alloc_limit, LBL(caml_requested_size)
        str     r12, [alloc_limit, #0]
    /* Branch to shared GC code */
        bl      LBL(invoke_gc)
    /* Finish allocation */
	ldr	r12, LBL(caml_requested_size)
	ldr	r12, [r12, #0]
	sub	alloc_ptr, alloc_ptr, r12
        bx      lr

        .globl G(caml_alloc1)
G(caml_alloc1):
        sub     alloc_ptr, alloc_ptr, #8
        cmp     alloc_ptr, alloc_limit
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r12, LBL(caml_last_return_address)
        str     lr, [r12, #0]
    /* Invoke GC */
        bl      LBL(invoke_gc)
    /* Try again */
        b       G(caml_alloc1)

        .globl G(caml_alloc2)
G(caml_alloc2):
        sub     alloc_ptr, alloc_ptr, #12
        cmp     alloc_ptr, alloc_limit
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r12, LBL(caml_last_return_address)
        str     lr, [r12, #0]
    /* Invoke GC */
        bl      LBL(invoke_gc)
    /* Try again */
        b       G(caml_alloc2)

        .globl G(caml_alloc3)
G(caml_alloc3):
        sub     alloc_ptr, alloc_ptr, #16
        cmp     alloc_ptr, alloc_limit
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address */
        ldr     r12, LBL(caml_last_return_address)
        str     lr, [r12, #0]
    /* Invoke GC */
        bl      LBL(invoke_gc)
    /* Try again */
        b       G(caml_alloc3)

        .globl G(caml_allocN)
G(caml_allocN):
        sub     alloc_ptr, alloc_ptr, r12
        cmp     alloc_ptr, alloc_limit
        movcs   pc, lr                /* Return if alloc_ptr >= alloc_limit */
    /* Record return address and desired size */
    /* Can use alloc_limit as a temporary since it will be reloaded by
       invoke_gc */
        ldr     alloc_limit, LBL(caml_last_return_address)
        str     lr, [alloc_limit, #0]
        ldr     alloc_limit, LBL(caml_requested_size)
        str     r12, [alloc_limit, #0]
    /* Invoke GC */
        bl      LBL(invoke_gc)
    /* Try again */
	ldr	r12, LBL(caml_requested_size)
	ldr	r12, [r12, #0]
        b       G(caml_allocN)

/* Shared code to invoke the GC */
LBL(invoke_gc):
    /* Record lowest stack address */
        ldr     r12, LBL(caml_bottom_of_stack)
        str     sp, [r12, #0]
    /* Save integer registers and return address on stack */
        stmfd   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r12,lr}
    /* Store pointer to saved integer registers in caml_gc_regs */
        ldr     r12, LBL(caml_gc_regs)
        str     sp, [r12, #0]
    /* Save current allocation pointer for debugging purposes */
        ldr     r12, LBL(caml_young_ptr)
        str     alloc_ptr, [r12, #0]
    /* Save trap pointer in case an exception is raised during GC */
        ldr     r12, LBL(caml_exception_pointer)
        str     trap_ptr, [r12, #0]
    /* Call the garbage collector */
        bl      G(caml_garbage_collection)
    /* Restore the registers from the stack */
        ldmfd   sp!, {r0,r1,r2,r3,r4,r5,r6,r7,r12}
    /* Reload return address */
        ldr     r12, LBL(caml_last_return_address)
        ldr     lr, [r12, #0]
    /* Reload new allocation pointer and allocation limit */
        ldr     r12, LBL(caml_young_ptr)
        ldr     alloc_ptr, [r12, #0]
        ldr     r12, LBL(caml_young_limit)
        ldr     alloc_limit, [r12, #0]
    /* Return to caller */
        ldr     r12, [sp], #4
        bx      r12

/* Call a C function from Caml */
/* Function to call is in r12 */

        .globl G(caml_c_call)
G(caml_c_call):
    /* Preserve return address in callee-save register r4 */
        mov     r4, lr
    /* Record lowest stack address and return address */
        ldr     r5, LBL(caml_last_return_address)
        ldr     r6, LBL(caml_bottom_of_stack)
        str     lr, [r5, #0]
        str     sp, [r6, #0]
    /* Make the exception handler and alloc ptr available to the C code */
        ldr     r6, LBL(caml_young_ptr)
        ldr     r7, LBL(caml_exception_pointer)
        str     alloc_ptr, [r6, #0]
        str     trap_ptr, [r7, #0]
    /* Call the function */
        mov     lr, pc
        bx      r12
    /* Reload alloc ptr and alloc limit */
        ldr     r5, LBL(caml_young_limit)
        ldr     alloc_ptr, [r6, #0]    /* r6 still points to caml_young_ptr */
        ldr     alloc_limit, [r5, #0]
    /* Return */
        bx      r4

/* Start the Caml program */

        .globl G(caml_start_program)
G(caml_start_program):
        ldr     r12, LBL(caml_program)

/* Code shared with caml_callback* */
/* Address of Caml code to call is in r12 */
/* Arguments to the Caml code are in r0...r3 */

LBL(jump_to_caml):
    /* Save return address and callee-save registers */
        stmfd   sp!, {r4,r5,r6,r7,r8,r10,r11,lr} /* 8-alignment */
    /* Setup a callback link on the stack */
        sub     sp, sp, #4*4                    /* 8-alignment */
        ldr     r4, LBL(caml_bottom_of_stack)
        ldr     r4, [r4, #0]
        str     r4, [sp, #0]
        ldr     r4, LBL(caml_last_return_address)
        ldr     r4, [r4, #0]
        str     r4, [sp, #4]
        ldr     r4, LBL(caml_gc_regs)
        ldr     r4, [r4, #0]
        str     r4, [sp, #8]
    /* Setup a trap frame to catch exceptions escaping the Caml code */
        sub     sp, sp, #4*2
        ldr     r4, LBL(caml_exception_pointer)
        ldr     r4, [r4, #0]
        str     r4, [sp, #0]
        ldr     r4, LBL(Ltrap_handler)
        str     r4, [sp, #4]
        mov     trap_ptr, sp
    /* Reload allocation pointers */
        ldr     r4, LBL(caml_young_ptr)
        ldr     alloc_ptr, [r4, #0]
        ldr     r4, LBL(caml_young_limit)
        ldr     alloc_limit, [r4, #0]
    /* Call the Caml code */
        mov     lr, pc
        bx      r12
LBL(caml_retaddr):
    /* Pop the trap frame, restoring caml_exception_pointer */
        ldr     r4, LBL(caml_exception_pointer)
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        add     sp, sp, #2 * 4
    /* Pop the callback link, restoring the global variables */
LBL(return_result):
        ldr     r4, LBL(caml_bottom_of_stack)
        ldr     r5, [sp, #0]
        str     r5, [r4, #0]
        ldr     r4, LBL(caml_last_return_address)
        ldr     r5, [sp, #4]
        str     r5, [r4, #0]
        ldr     r4, LBL(caml_gc_regs)
        ldr     r5, [sp, #8]
        str     r5, [r4, #0]
        add     sp, sp, #4*4
    /* Update allocation pointer */
        ldr     r4, LBL(caml_young_ptr)
        str     alloc_ptr, [r4, #0]
    /* Reload callee-save registers and return */
        ldmfd   sp!, {r4,r5,r6,r7,r8,r10,r11,lr}
	bx	lr

    /* The trap handler */
LBL(trap_handler):
    /* Save exception pointer */
        ldr     r4, LBL(caml_exception_pointer)
        str     trap_ptr, [r4, #0]
    /* Encode exception bucket as an exception result */
        orr     r0, r0, #2
    /* Return it */
        b       LBL(return_result)

/* Raise an exception from C */

        .globl G(caml_raise_exception)
G(caml_raise_exception):
    /* Reload Caml allocation pointers */
        ldr     r12, LBL(caml_young_ptr)
        ldr     alloc_ptr, [r12, #0]
        ldr     r12, LBL(caml_young_limit)
        ldr     alloc_limit, [r12, #0]
    /* Cut stack at current trap handler */
        ldr     r12, LBL(caml_exception_pointer)
        ldr     sp, [r12, #0]
    /* Pop previous handler and addr of trap, and jump to it */
        ldmfd   sp!, {trap_ptr, pc}

/* Callback from C to Caml */

        .globl G(caml_callback_exn)
G(caml_callback_exn):
    /* Initial shuffling of arguments (r0 = closure, r1 = first arg) */
        mov     r12, r0
        mov     r0, r1            /* r0 = first arg */
        mov     r1, r12           /* r1 = closure environment */
        ldr     r12, [r12, #0]    /* code pointer */
        b       LBL(jump_to_caml)

        .globl G(caml_callback2_exn)
G(caml_callback2_exn):
    /* Initial shuffling of arguments (r0 = closure, r1 = arg1, r2 = arg2) */
        mov     r12, r0
        mov     r0, r1           /* r0 = first arg */
        mov     r1, r2           /* r1 = second arg */
        mov     r2, r12          /* r2 = closure environment */
        ldr     r12, LBL(caml_apply2)
        b       LBL(jump_to_caml)

        .globl G(caml_callback3_exn)
G(caml_callback3_exn):
    /* Initial shuffling of arguments */
    /* (r0 = closure, r1 = arg1, r2 = arg2, r3 = arg3) */
        mov     r12, r0
        mov     r0, r1          /* r0 = first arg */
        mov     r1, r2          /* r1 = second arg */
        mov     r2, r3          /* r2 = third arg */
        mov     r3, r12         /* r3 = closure environment */
        ldr     r12, LBL(caml_apply3)
        b       LBL(jump_to_caml)

        .globl G(caml_ml_array_bound_error)
G(caml_ml_array_bound_error):
    /* Load address of [caml_array_bound_error] in r12 */
        ldr     r12, LBL(caml_array_bound_error)
    /* Call that function */
        b       G(caml_c_call)

/* Global references */

LBL(caml_last_return_address):     .word G(caml_last_return_address)
LBL(caml_bottom_of_stack):         .word G(caml_bottom_of_stack)
LBL(caml_gc_regs):                 .word G(caml_gc_regs)
LBL(caml_young_ptr):               .word G(caml_young_ptr)
LBL(caml_young_limit):             .word G(caml_young_limit)
LBL(caml_exception_pointer):       .word G(caml_exception_pointer)
LBL(caml_program):                 .word G(caml_program)
LBL(Ltrap_handler):                .word LBL(trap_handler)
LBL(caml_apply2):                  .word G(caml_apply2)
LBL(caml_apply3):                  .word G(caml_apply3)
LBL(caml_array_bound_error):       .word G(caml_array_bound_error)
LBL(caml_requested_size):          .word G(caml_requested_size)

	.data
G(caml_requested_size):
	.word	0

/* GC roots for callback */

        .data
        .globl G(caml_system__frametable)
G(caml_system__frametable):
        .word   1               /* one descriptor */
        .word   LBL(caml_retaddr)  /* return address into callback */
        .short  -1              /* negative frame size => use callback link */
        .short  0               /* no roots */
        .align  2
