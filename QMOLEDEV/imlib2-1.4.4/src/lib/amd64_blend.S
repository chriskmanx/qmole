#include <config.h>
#include "asm.h"

#ifdef DO_AMD64_ASM

/*\ 
|*| AMD64 SSE2 assembly blending routines for Imlib2
|*| Written by John Slaten <zartheenumerator@comcast.net>
|*| Based on MMX routines written by Willem Monsuwe <willem@stack.nl>
\*/

/*\ Some useful masks \*/
.data
	.align 16
m0X000000: .byte   0,   0,   0,   0,   0,   0, 255,   0
           .byte   0,   0,   0,   0,   0,   0, 255,   0
m10000000: .byte   0,   0,   0,   0,   0,   0,   0,   1
           .byte   0,   0,   0,   0,   0,   0,   0,   1
m00XXXXXX: .byte 255, 255, 255, 255, 255, 255,   0,   0
           .byte 255, 255, 255, 255, 255, 255,   0,   0
mVX000000: .byte   0,   0,   0,   0,   0,   0, 255, 127
           .byte   0,   0,   0,   0,   0,   0, 255, 127
mV0000000: .byte   0,   0,   0,   0,   0,   0,   0, 128
           .byte   0,   0,   0,   0,   0,   0,   0, 128
mX000X000: .byte   0,   0,   0,   0,   0,   0, 255, 255
           .byte   0,   0,   0,   0,   0,   0, 255, 255
m0XXX0XXX0XXX0XXX: .byte 255, 255, 255,   0, 255, 255, 255,   0
                   .byte 255, 255, 255,   0, 255, 255, 255,   0
m0XXX0XXX00000000: .byte 255, 255, 255,   0, 255, 255, 255,   0
                   .byte   0,   0,   0,   0,   0,   0,   0,   0
m0XXX000000000000: .byte 255, 255, 255,   0,   0,   0,   0,   0
                   .byte   0,   0,   0,   0,   0,   0,   0,   0
mX000X000X000X000: .byte   0,   0,   0, 255,   0,   0,   0, 255
                   .byte   0,   0,   0, 255,   0,   0,   0, 255
mX000X00000000000: .byte   0,   0,   0, 255,   0,   0,   0, 255
                   .byte   0,   0,   0, 255,   0,   0,   0, 255
mX000000000000000: .byte   0,   0,   0, 255,   0,   0,   0, 255
                   .byte   0,   0,   0, 255,   0,   0,   0, 255
m1000100010001000: .byte   0,   0,   0,   1,   0,   0,   0,   1
                   .byte   0,   0,   0,   1,   0,   0,   0,   1
m000V0V0V000V0V0V: .byte 127,   0, 127,   0, 127,   0,   0,   0
                   .byte 127,   0, 127,   0, 127,   0,   0,   0
mI0000000I0000000: .byte   0,   0,   0,   0,   0,   0,   0,  64
                   .byte   0,   0,   0,   0,   0,   0,   0,  64
m0VVV0VVV0VVV0VVV: .byte 127, 127, 127,   0, 127, 127, 127,   0
                   .byte 127, 127, 127,   0, 127, 127, 127,   0
c1: .word 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1

/*\ All functions have the same calling convention:
|*|  __imlib_amd64_<op>_rgba_to_rgb[A](void *src, int sw, void *dst, int dw,
|*|			               int w, int h, ImlibColorModifier *cm)
|*| AMD64 GCC passes paramters by register, so no aliases exist in this version.
\*/

.text
	.align 16
FN_(imlib_amd64_blend_rgba_to_rgb)
FN_(imlib_amd64_blend_rgba_to_rgba)
FN_(imlib_amd64_copy_rgba_to_rgb)
FN_(imlib_amd64_copy_rgba_to_rgba)

FN_(imlib_amd64_copy_rgb_to_rgba)
FN_(imlib_amd64_add_blend_rgba_to_rgb)
FN_(imlib_amd64_add_blend_rgba_to_rgba)
FN_(imlib_amd64_add_copy_rgba_to_rgb)
FN_(imlib_amd64_add_copy_rgba_to_rgba)
FN_(imlib_amd64_add_copy_rgb_to_rgba)

FN_(imlib_amd64_subtract_blend_rgba_to_rgb)
FN_(imlib_amd64_subtract_blend_rgba_to_rgba)
FN_(imlib_amd64_subtract_copy_rgba_to_rgb)
FN_(imlib_amd64_subtract_copy_rgba_to_rgba)
FN_(imlib_amd64_subtract_copy_rgb_to_rgba)

FN_(imlib_amd64_reshade_blend_rgba_to_rgb)
FN_(imlib_amd64_reshade_blend_rgba_to_rgba)
FN_(imlib_amd64_reshade_copy_rgba_to_rgb)
FN_(imlib_amd64_reshade_copy_rgba_to_rgba)
FN_(imlib_amd64_reshade_copy_rgb_to_rgba)

.extern pow_lut
	
/*\ SSE register use:
|*| %xmm1 = Source value
|*| %xmm2 = Destination value
|*| %xmm3 = Alpha value
|*| %xmm4 = 0
|*| %xmm5-%xmm7 = masks
\*/

/*\ Variables:
|*| %rsi = src
|*| %rdi = dst
|*| %r8d = w
|*| %r9d = h
|*| %r10d = sw
|*| %r11d = dw
\*/
	




#define ENTER		\
	pushq %rbp	; \
	movq %rsp, %rbp	; \
	pushq %rbx	; \
	pushq %r13	; \
	pushq %r14	; \
	movq %rsi, %r10	; \
	movq %rcx, %r11	; \
	movq %rdi, %rsi	; \
	movq %rdx, %rdi	; \
	movq 16(%rbp), %r14 ; \
			; \
	/* param sanity check */ ; \
	testq %r8, %r8	; \
	jz 9f		; \
	testq %r9, %r9	; \
	jz 9f
	
#define LEAVE		\
	popq %r14	; \
	popq %r13	; \
	popq %rbx	; \
	movq %rbp, %rsp	; \
	popq %rbp	; \
	ret


PR_(imlib_amd64_blend_rgba_to_rgb):
	ENTER

	pxor %xmm4, %xmm4
	movdqu c1(%rip), %xmm5
	movdqu m00XXXXXX(%rip), %xmm6

	/* Move right to left across each line, */ 
	/* processing in two pixel chunks */ 
	leaq (%rsi, %r8, 4), %rsi	
	leaq (%rdi, %r8, 4), %rdi	
					
	/* Last instruction is %rcx = 0 */ 
	subq $4, %rsi			
	subq $4, %rdi			
					
	negq %r8			
0:					
	movq %r8, %rcx			
					
	incq %rcx			

	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	jz 2f /* one pixel line */	
1:
	/* main loop, unrolled to work on 64 byte chunks */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	js 1b				
	jnz 3f				
2:
	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
3:					
	leaq (%rsi, %r10, 4), %rsi	
	leaq (%rdi, %r11, 4), %rdi	
	decq %r9			
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_blend_rgba_to_rgb)
PR_(imlib_amd64_blend_rgba_to_rgba):
	ENTER

	pxor %xmm4, %xmm4
	movdqu c1(%rip), %xmm5
	xorq %rax, %rax
	movdqu mX000X000X000X000(%rip), %xmm6
	movq pow_lut@GOTPCREL(%rip), %r13

	/* Move right to left across each line, */ 
	/* processing in two pixel chunks */ 
	leaq (%rsi, %r8, 4), %rsi	
	leaq (%rdi, %r8, 4), %rdi	
					
	/* Last instruction is %rcx = 0 */ 
	subq $4, %rsi			
	subq $4, %rdi			
					
	negq %r8			
0:					
	movq %r8, %rcx			
					
	incq %rcx			

	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	jz 2f /* one pixel line */	
1:
	/* main loop, unrolled to work on 64 byte chunks */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	js 1b				
	jnz 3f				
2:
	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* Load one pixel as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %eax, %xmm3 
	/* override source alpha to 255 */
	por %xmm6, %xmm1

	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* unpack source and dest */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * ((s - d) + 0.5)) */
	psubw %xmm2, %xmm1
	psllw $1, %xmm1
	paddw %xmm5, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* repack new pixels */
	packuswb %xmm4, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
3:					
	leaq (%rsi, %r10, 4), %rsi	
	leaq (%rdi, %r11, 4), %rdi	
	decq %r9			
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_blend_rgba_to_rgba)
PR_(imlib_amd64_copy_rgba_to_rgb):
	ENTER

	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm5
	movdqu mX000X000X000X000(%rip), %xmm6

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = (s & 0x00ffffff) | (d & 0xff000000) */
	pand %xmm5, %xmm1
	pand %xmm6, %xmm2
	por %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_copy_rgba_to_rgb)
PR_(imlib_amd64_copy_rgba_to_rgba):
	ENTER


	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd %xmm1, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd %xmm1, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_copy_rgba_to_rgba)
PR_(imlib_amd64_copy_rgb_to_rgba):
	ENTER

	movdqu mX000X000X000X000(%rip), %xmm5

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movd %xmm1, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movdqa %xmm1, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	/* d = s | 0xff000000 */
	por %xmm5, %xmm1
	movd %xmm1, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_copy_rgb_to_rgba)
PR_(imlib_amd64_add_blend_rgba_to_rgb):
	ENTER

	pxor %xmm4, %xmm4
	movdqu m00XXXXXX(%rip), %xmm6

	/* Move right to left across each line, */ 
	/* processing in two pixel chunks */ 
	leaq (%rsi, %r8, 4), %rsi	
	leaq (%rdi, %r8, 4), %rdi	
					
	/* Last instruction is %rcx = 0 */ 
	subq $4, %rsi			
	subq $4, %rdi			
					
	negq %r8			
0:					
	movq %r8, %rcx			
					
	incq %rcx			

	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	jz 2f /* one pixel line */	
1:
	/* main loop, unrolled to work on 64 byte chunks */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	js 1b				
	jnz 3f				
2:
	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (a * s) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
3:					
	leaq (%rsi, %r10, 4), %rsi	
	leaq (%rdi, %r11, 4), %rdi	
	decq %r9			
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_add_blend_rgba_to_rgb)

PR_(imlib_amd64_add_blend_rgba_to_rgba):
	ENTER

	pxor %xmm4, %xmm4
	movdqu c1(%rip), %xmm5
	xorq %rax, %rax
	movdqu mX000X000X000X000(%rip), %xmm6
	movq pow_lut@GOTPCREL(%rip), %r13

	/* Move right to left across each line, */ 
	/* processing in two pixel chunks */ 
	leaq (%rsi, %r8, 4), %rsi	
	leaq (%rdi, %r8, 4), %rdi	
					
	/* Last instruction is %rcx = 0 */ 
	subq $4, %rsi			
	subq $4, %rdi			
					
	negq %r8			
0:					
	movq %r8, %rcx			
					
	incq %rcx			

	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	jz 2f /* one pixel line */	
1:
	/* main loop, unrolled to work on 64 byte chunks */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	js 1b				
	jnz 3f				
2:
	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* Load one pixel as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %eax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	por %xmm6, %xmm1
	pand %xmm6, %xmm0
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (s * ca) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
3:					
	leaq (%rsi, %r10, 4), %rsi	
	leaq (%rdi, %r11, 4), %rdi	
	decq %r9			
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_add_blend_rgba_to_rgba)

PR_(imlib_amd64_add_copy_rgba_to_rgb):
	ENTER

	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm5

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = d + (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	paddusb %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_add_copy_rgba_to_rgb)

PR_(imlib_amd64_add_copy_rgba_to_rgba):
	ENTER

	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm5

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = (d & 0x00ffffff) + s */
	pand %xmm5, %xmm2
	paddusb %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_add_copy_rgba_to_rgba)

PR_(imlib_amd64_add_copy_rgb_to_rgba):
	ENTER

	movdqu mX000X000X000X000(%rip), %xmm5

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = (d + s) | 0xff000000 */	
	paddusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_add_copy_rgb_to_rgba)

PR_(imlib_amd64_subtract_blend_rgba_to_rgb):
	ENTER

	pxor %xmm4, %xmm4
	movdqu m00XXXXXX(%rip), %xmm6

	/* Move right to left across each line, */ 
	/* processing in two pixel chunks */ 
	leaq (%rsi, %r8, 4), %rsi	
	leaq (%rdi, %r8, 4), %rdi	
					
	/* Last instruction is %rcx = 0 */ 
	subq $4, %rsi			
	subq $4, %rdi			
					
	negq %r8			
0:					
	movq %r8, %rcx			
					
	incq %rcx			

	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	jz 2f /* one pixel line */	
1:
	/* main loop, unrolled to work on 64 byte chunks */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	js 1b				
	jnz 3f				
2:
	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* Get alpha from source and unpack to words
	 * Result ranges is [0, 0x7fff], and is mapped to
	 *  point values in [0.0, 1.0) by using the high word
	 *  of the 32 bit multiplication result.
	 * Because we want the unsigned value, we shift right one 
	 *  here and also shift left the other factors to compensate.
	 */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero out the alpha channel of the source to leave the
	 * destination alpha unchanged.
	 */
	pand %xmm6, %xmm3

	/* Unpack src and dst to words */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - (s * a) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	psubsw %xmm1, %xmm2

	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
3:					
	leaq (%rsi, %r10, 4), %rsi	
	leaq (%rdi, %r11, 4), %rdi	
	decq %r9			
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_subtract_blend_rgba_to_rgb)

PR_(imlib_amd64_subtract_blend_rgba_to_rgba):
	ENTER

	movq pow_lut@GOTPCREL(%rip), %r13
	pxor %xmm4, %xmm4
	movdqu c1(%rip), %xmm5
	movdqu mX000X000X000X000(%rip), %xmm6
	movdqu mX000X000(%rip), %xmm7
	xorq %rax, %rax

	/* Move right to left across each line, */ 
	/* processing in two pixel chunks */ 
	leaq (%rsi, %r8, 4), %rsi	
	leaq (%rdi, %r8, 4), %rdi	
					
	/* Last instruction is %rcx = 0 */ 
	subq $4, %rsi			
	subq $4, %rdi			
					
	negq %r8			
0:					
	movq %r8, %rcx			
					
	incq %rcx			

	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	jz 2f /* one pixel line */	
1:
	/* main loop, unrolled to work on 64 byte chunks */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Load two pixels as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %rax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	js 1b				
	jnz 3f				
2:
	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* Load one pixel as 00, 00, src alpha, combined alpha 
	 * Combined alpha is derived from the pow_lut table in blend.c 
	 */
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	movd %eax, %xmm3 
	/* unpack alpha to src alpha, combined alpha x 3 */
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	/* src alpha = 255 - dst alpha */
	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	/* unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d - ((s * a) ^ 0xff000000) */
	psllw $1, %xmm1
	pmulhw %xmm3, %xmm1
	pxor %xmm7, %xmm1
	psubsw %xmm1, %xmm2
	
	/* pack new pixels */
	packuswb %xmm4, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
3:					
	leaq (%rsi, %r10, 4), %rsi	
	leaq (%rdi, %r11, 4), %rdi	
	decq %r9			
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_subtract_blend_rgba_to_rgba)

PR_(imlib_amd64_subtract_copy_rgba_to_rgb):
	ENTER

	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm5

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = d - (s & 0x00ffffff) */
	pand %xmm5, %xmm1
	psubusb %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_subtract_copy_rgba_to_rgb)

PR_(imlib_amd64_subtract_copy_rgba_to_rgba):
	ENTER

	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm5
	movdqu mX000X000X000X000(%rip), %xmm6

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = d - s, d alpha = s alpha */
	psubusb %xmm1, %xmm2
	pand %xmm6, %xmm1
	pand %xmm5, %xmm2
	por %xmm1, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_subtract_copy_rgba_to_rgba)

PR_(imlib_amd64_subtract_copy_rgb_to_rgba):
	ENTER

	movdqu mX000X000X000X000(%rip), %xmm5

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* d = (d - s) | 0xff000000 */	
	psubusb %xmm1, %xmm2
	por %xmm5, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_subtract_copy_rgb_to_rgba)

PR_(imlib_amd64_reshade_blend_rgba_to_rgb):
	ENTER

	pxor %xmm4, %xmm4
	movdqu m000V0V0V000V0V0V(%rip), %xmm6
	movdqu m00XXXXXX(%rip), %xmm7

	/* Move right to left across each line, */ 
	/* processing in two pixel chunks */ 
	leaq (%rsi, %r8, 4), %rsi	
	leaq (%rdi, %r8, 4), %rdi	
					
	/* Last instruction is %rcx = 0 */ 
	subq $4, %rsi			
	subq $4, %rdi			
					
	negq %r8			
0:					
	movq %r8, %rcx			
					
	incq %rcx			

	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	jz 2f /* one pixel line */	
1:
	/* main loop, unrolled to work on 64 byte chunks */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	js 1b				
	jnz 3f				
2:
	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* Unpack alpha */
	movq %xmm1, %xmm3
	punpcklbw %xmm3, %xmm3
	pshufhw $0xFF, %xmm3, %xmm3
	pshuflw $0xFF, %xmm3, %xmm3 
	psrlw $1, %xmm3

	/* Zero blending alpha */
	pand %xmm7, %xmm3

	/* Unpack src and dst */
	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	/* d = d + (2 * a * (s - 127)) */
	psubw %xmm6, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2

	/* Repack new pixels */
	packuswb %xmm4, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
3:					
	leaq (%rsi, %r10, 4), %rsi	
	leaq (%rdi, %r11, 4), %rdi	
	decq %r9			
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_reshade_blend_rgba_to_rgb)

PR_(imlib_amd64_reshade_blend_rgba_to_rgba):
	ENTER

	movq pow_lut@GOTPCREL(%rip), %r13
	pxor %xmm4, %xmm4
	movdqu c1(%rip), %xmm5
	movdqu mX000X000X000X000(%rip), %xmm6
	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm7
	movdqu m000V0V0V000V0V0V(%rip), %xmm8
	xorq %rax, %rax

	/* Move right to left across each line, */ 
	/* processing in two pixel chunks */ 
	leaq (%rsi, %r8, 4), %rsi	
	leaq (%rdi, %r8, 4), %rdi	
					
	/* Last instruction is %rcx = 0 */ 
	subq $4, %rsi			
	subq $4, %rdi			
					
	negq %r8			
0:					
	movq %r8, %rcx			
					
	incq %rcx			

	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	jz 2f /* one pixel line */	
1:
	/* main loop, unrolled to work on 64 byte chunks */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %rax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %rax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %rax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %rax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %rax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %rax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %rax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	jz 2f
	jns 3f				

	movq (%rsi, %rcx, 4), %xmm1
	movq (%rdi, %rcx, 4), %xmm2
	movzbq 7(%rdi, %rcx, 4), %rdx
	movb 7(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	shlq $32, %rax
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %rax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movq %xmm2, (%rdi, %rcx, 4)

	incq %rcx			
	incq %rcx			
	js 1b				
	jnz 3f				
2:
	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	movzbq 3(%rdi, %rcx, 4), %rdx
	movb 3(%rsi, %rcx, 4), %dh
	movb (%r13, %rdx), %al
	movb %dh, %ah
	shrb $1, %ah
	movd %eax, %xmm3 
	punpcklbw %xmm3, %xmm3
	pshufhw $0x40, %xmm3, %xmm3
	pshuflw $0x40, %xmm3, %xmm3	
	psrlw $1, %xmm3

	movdqa %xmm2, %xmm0
	pand %xmm6, %xmm0
	por %xmm6, %xmm1
	psubusb %xmm0, %xmm1

	punpcklbw %xmm4, %xmm1
	punpcklbw %xmm4, %xmm2

	psubw %xmm8, %xmm1
	psllw $2, %xmm1
	pmulhw %xmm3, %xmm1
	paddsw %xmm1, %xmm2
	
	packuswb %xmm4, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
3:					
	leaq (%rsi, %r10, 4), %rsi	
	leaq (%rdi, %r11, 4), %rdi	
	decq %r9			
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_reshade_blend_rgba_to_rgba)

PR_(imlib_amd64_reshade_copy_rgba_to_rgb):
	ENTER

	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm5
	movdqu m0VVV0VVV0VVV0VVV(%rip), %xmm6

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* To take advantage of saturation and be able to do 8 bytes
	 *  at a time, we divide reshading into two separate steps:
	 *  adding values above 128, and subtracting values below 128
	 * These values go into %mm1 and %mm3 respectively
	 * - %xmm1 becomes (2 * (s - 127))
	 * - %xmm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* dest alpha should not be changed in this func */
	pand %xmm5, %xmm1
	pand %xmm5, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_reshade_copy_rgba_to_rgb)

PR_(imlib_amd64_reshade_copy_rgba_to_rgba):
	ENTER

	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm5
	movdqu m0VVV0VVV0VVV0VVV(%rip), %xmm6
	movdqu mX000X000X000X000(%rip), %xmm7

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	/* This time, dest alpha = src alpha, so src alpha is saved in %xmm0 */
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	movdqa %xmm1, %xmm0
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2
	
	/* d alpha = s alpha */
	pand %xmm5, %xmm2
	pand %xmm7, %xmm0
	por %xmm0, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_reshade_copy_rgba_to_rgba)

PR_(imlib_amd64_reshade_copy_rgb_to_rgba):
	ENTER

	movdqu m0XXX0XXX0XXX0XXX(%rip), %xmm5
	movdqu m0VVV0VVV0VVV0VVV(%rip), %xmm6
	movdqu mX000X000X000X000(%rip), %xmm7

	leaq (%rsi, %r8, 4), %rsi
	leaq (%rdi, %r8, 4), %rdi

	subq $12, %rsi
	subq $12, %rdi

	negq %r8
0:
	movq %r8, %rcx

	/* if < 4 pixels left, goto end */
	addq $3, %rcx
	jns 4f
1:
	/* 16 byte align dst ptr */
	leaq (%rdi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jz 1f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jz 4f
	jmp 1b
1:
	/* prefetch a couple cache lines ahead */
	prefetchnta (%rsi, %rcx, 4)
	prefetcht0 (%rdi, %rcx, 4)
	prefetchnta 64(%rsi, %rcx, 4)
	prefetcht0 64(%rdi, %rcx, 4)

	/* test if 16 byte aligned src ptr */
	leaq (%rsi, %rcx, 4), %rdx
	test $0x0f, %rdx
	jnz 3f
2:
	/* main loop, unrolled to work on 64 byte chunks */
	/* aligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqa (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 2b
	jmp 4f
3:
	/* main loop, unrolled to work on 64 byte chunks */
	/* unaligned src, aligned dst */
	prefetchnta 128(%rsi, %rcx, 4)
	prefetcht0 128(%rdi, %rcx, 4)

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	jns 4f

	movdqu (%rsi, %rcx, 4), %xmm1
	movdqa (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movdqa %xmm2, (%rdi, %rcx, 4)
	addq $4, %rcx
	js 3b
4:
	/* finish loop */
	cmp $2, %rcx
	jg 5f

	movd (%rsi, %rcx, 4), %xmm1
	movd (%rdi, %rcx, 4), %xmm2
	movdqa %xmm1, %xmm3
	psubusb %xmm6, %xmm1
	paddusb %xmm1, %xmm1
	paddusb %xmm6, %xmm3
	pxor %xmm5, %xmm3
	paddusb %xmm3, %xmm3

	/* d = d + s1 - s2, unsigned saturation */
	paddusb %xmm1, %xmm2
	psubusb %xmm3, %xmm2

	/* d alpha = 0xff */
	por %xmm7, %xmm2
	movd %xmm2, (%rdi, %rcx, 4)
	incq %rcx
	jmp 4b
5:
	/* finish line */
	leaq (%rsi, %r10, 4), %rsi
	leaq (%rdi, %r11, 4), %rdi
	decq %r9
	jnz 0b

9:
	LEAVE
SIZE(imlib_amd64_reshade_copy_rgb_to_rgba)

#endif

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif
