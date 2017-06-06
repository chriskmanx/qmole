/*
 * Copyright (C) 1997-2009, Michael Jennings
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies of the Software, its documentation and marketing & publicity
 * materials, and acknowledgment shall be given in the documentation, materials
 * and software packages that this Software was used.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   
 *  THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER  
 *  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN   
 *  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Ported from <willem@stack.nl> Willem Monsuwe's original x86/MMX assembly 
 * code by Tres Melton in 2005 and 2006.  Anything copyrightable by me is
 * assigned to the Eterm project and its founder/maintainer: Michael Jennings.
 *
 *  Much inspiration was drawn from the original x86 MMX port written by
 *	Willem Monsuwe <willem@stack.nl> in pure x86/MMX Assembly.  The MMX
 *	instructions are taken almost verbatim but the memory and parameter
 *	accessing had to be completely reworked for the x86_64 ABI and to 
 *	ensure they worked with various gcc options.  Further the code was
 *	extended to take advantage of the 128 bit xmm registers in SSE2.
 *
 *  The imlib2 code in Enlightenment also has a lot to teach on the subject.
 *
 *  Manuals used in this port:
 *      The Gnu Assembler
 *              http://www.gnu.org/software/binutils/manual/gas-2.9.1/html_mono/as.html
 *      AMD64 Architecture Programmer's Manual Volume 1: Application Programming
 *              http://www.amd.com/us-en/assets/content_type/white_papers_and_tech_docs/24592.pdf
 *      AMD64 Architecture Programmer's Manual Volume 2: System Programming
 *              http://www.amd.com/us-en/assets/content_type/white_papers_and_tech_docs/24593.pdf
 *      AMD64 Architecture Programmer's Manual Volume 3: General-Purpose and System Instructions
 *              http://www.amd.com/us-en/assets/content_type/white_papers_and_tech_docs/24594.pdf
 *      AMD64 Architecture Programmer's Manual Volume 4: 128-Bit Media Instructions
 *              http://www.amd.com/us-en/assets/content_type/white_papers_and_tech_docs/26568.pdf
 *      AMD64 Architecture Programmer's Manual Volume 5: 64-Bit Media and x87 Floating-Point Instructions
 *              http://www.amd.com/us-en/assets/content_type/white_papers_and_tech_docs/26569.pdf
 *	AMD64 Application Binary Interface (v. 0.95)
 *		http://www.x86-64.org/documentation/documentation/abi-0.95.pdf
 *
 *  The 32 bit color modification algorithm is simple but so optimized 
 *    (even the C version) that it is almost unreadable.  
 *    Therefore the pseudo code is:
 *
 *	for each color of each pixel
 *		new_color = color * modifier
 *		if ( new_color > max_color_value )
 *			new_color = max_color_value
 *		end if
 *	end for
 *
 *  The AMD64 ABI is at version 0.95 and might change in the future.  Further it has changed a
 *	number of times in the past (although mostly in 2002-2003) as evidenced by the mailing
 *	list on http://www.x86-64.org.  The GCC, Glibc, and Linux kernel have changed as well
 *	during this time to keep up.  The standard C definition states that function parameters
 *	are to be passed on the stack but that can be very inefficient compared to passing them
 *	in registers so gcc tries to use registers.  This is very different than on the register
 *	starved i386 architecture (AMD64 adds 8 general purpose registers: %r8-%r15, and 
 *	MMX/SSE2/F87 adds 16 64/128/80bit registers: %xmm0-%xmm15. The x86_64 version of GCC uses
 *	registers as efficiently as possible and as a result exactly which registers are used
 *	for which parameters has evolved.  Since all of these tools change simultaneously inline 
 *	assembly code in C functions is the only way to ensure that this code will continue to 
 *	function through a (however unlikely) change.  If pure assembly were to be used as the
 *	original MMX author, Willem Monsuwe, did and the ABI changed then this code would cease
 *	to function properly.  
 *
 *  In Conclusion:
 *	Using C functions and inline assembly code should alleviate all of the concerns as the
 *	C compiler will ensure that the parameters get to the function in a gauranteed manner
 *	and the inline assembly explicitly loads them into the desired registers for the assembly
 *	code.  This might seem like alot of overhead but great care has been taken to adhere to
 *	the x86_64 ABI so that gcc/gas/ld will not perform any unneeded operations even when no
 *	optimizations have been enabled (-O[123]).
 */

#include "config.h"

#ifdef HAVE_SSE2

void shade_ximage_15_sse2( volatile void *data, volatile int bpl, volatile int w, volatile int h, volatile int rm, volatile int gm, volatile int bm )
{
  __asm__ __volatile__ (
	".align 16                      \n\t"   /* SIMD instructions should be aligned on 16 byte (128 bit) boundraries for performance reasons.*/
	"leaq -14(%%rsi, %%rbx, 2), %%rsi\n\t"	/* Load the stack index register with a pointer to data + ( width * bytes/pixel ) -6		*/
	"negq %%rbx			\n\t"	/* Negate the width to that we can increment the counter					*/
	"jz 10f				\n\t"	/* Jump to end if the line count is zero							*/
	"movd %[red_mod], %%xmm5	\n\t"	/* Load the color modifiers into mmx registers							*/
	"movd %[green_mod], %%xmm6	\n\t"	/* " "												*/
	"movd %[blue_mod], %%xmm7	\n\t"	/* " "												*/
	"punpcklwd %%xmm5, %%xmm5	\n\t"	/* Unpack and Interleave low words.  From A64_128bit_Media_Programming (p. 380)			*/
	"punpcklwd %%xmm6, %%xmm6	\n\t"	/* Duplicate the bottom 16 bits into the next 16 bits (both operands are the same)		*/
	"punpcklwd %%xmm7, %%xmm7	\n\t"
	"punpckldq %%xmm5, %%xmm5	\n\t"	/* Unpack and Interleave low double words.  From A64_128bit_Media_Programming (p. 376)		*/
	"punpckldq %%xmm6, %%xmm6	\n\t"	/* Duplicate the bottom 32 bits into the next 32 bits (both operands are the same)		*/
	"punpckldq %%xmm7, %%xmm7	\n\t"
	"punpcklqdq %%xmm5, %%xmm5	\n\t"	/* Unpack and Interleave low quad words.  From A64_128bit_Media_Programming (p. 378)		*/
	"punpcklqdq %%xmm6, %%xmm6	\n\t"	/* Duplicate the bottom 64 bits into the next 64 bits (both operands are the same)		*/
	"punpcklqdq %%xmm7, %%xmm7	\n\t"
	"or %[red_mod], %[green_mod]	\n\t"	/* This, and the following 4 instructions, check to see if all three colormodifiers are		*/
	"or %[blue_mod], %[green_mod]	\n\t"	/* less than 256.  If any of the modifiers are > 256 then they will have the 9th, or higher,	*/
	"sar $8, %[green_mod]		\n\t"	/* bit set.  Then we shift off eight bits, leaving something set if a modifier > 256. 		*/
	"movq %%rax, %[blue_mod]	\n\t"	/* Use the register named blue_mod to now store bytes_per_line.					*/
	"xor %[red_mod], %[red_mod]	\n\t"	/* zero red so we don't have to load an immediate value for the following compare.		*/
	"cmp %[red_mod], %[green_mod]	\n\t"	/* Compare the left over bits to zero								*/
	"jg 5f				\n\t"	/* If one of the colors (might) need saturated then jump to the secondary set of loops.		*/
	"1:				\n\t"	/* Start of the outer loop (lines).								*/
	"movq %%rbx, %%rcx		\n\t"	/* Move the width into the count register							*/
	"addq $7, %%rcx			\n\t"	
	"jns 3f				\n\t"
	"2:				\n\t"	/* Start of the inner loop (pixels 8 at a time --> 8 * 16 = 128bits/xmm register )		*/
	"movdqu (%%rsi, %%rcx, 2), %%xmm0\n\t"	/* Load the 16 bits of the pixel (5 bits for red, 6 bits for green, 5 bits for blue)		*/
	"movdqa %%xmm0, %%xmm1		\n\t"	/* Create a copy of the pixel for the green color						*/
	"movdqa %%xmm0, %%xmm2		\n\t"	/* Create a copy of the pixel for the blue color						*/
	"psrlw $5, %%xmm1		\n\t"	/* Packed Shift Right Logical Words								*/
						/* From A64_128bit_Media_Programming (p. 347)							*/
						/* Shifts the blue off of the green color							*/
	"psrlw $10, %%xmm0		\n\t"	/* Shifts the blue & green off of the red color							*/
	"psllw $11, %%xmm2		\n\t"	/* Packed Shift Left Logical Words								*/
						/* From A64_128bit_Media_Programming (p. 330)							*/
						/* Shifts the red & green off of the blue color							*/
	"psllw $11, %%xmm1		\n\t"	/* Shifts the red off of the green color							*/
	"psllw $8, %%xmm0		\n\t"	/* Shifts the red color into position								*/
	"psrlw $3, %%xmm1		\n\t"	/* Shifts the green color into position								*/
	"psrlw $3, %%xmm2		\n\t"	/* Shifts the blue color into position								*/
	"pmulhw %%xmm5, %%xmm0		\n\t"	/* color *= modifier										*/
	"pmulhw %%xmm6, %%xmm1		\n\t"
	"pmulhw %%xmm7, %%xmm2		\n\t"
	"psllw $10, %%xmm0		\n\t"	/* Shift red back into its original position							*/
	"psllw $5, %%xmm1		\n\t"	/* Shift green back into its original position							*/
	"por %%xmm2, %%xmm0		\n\t"	/* Mesh the colors back together								*/
	"por %%xmm1, %%xmm0		\n\t"
	"movdqu %%xmm0, (%%rsi, %%rcx, 2)\n\t"	/* Place the shaded 8 pixels back into the image map						*/
	"addq $8, %%rcx			\n\t"	
	"js 2b				\n\t"
	"jmp 4f				\n\t"
	"3:				\n\t"	/* Deal with pixels one at a time here.								 */
	"movw (%%rsi, %%rcx, 2), %%ax	\n\t"
	"movd %%eax, %%xmm0		\n\t"
	"movq %%xmm0, %%xmm1		\n\t"
	"movq %%xmm0, %%xmm2		\n\t"
	"psrlw $5, %%xmm1		\n\t"
	"psrlw $10, %%xmm0		\n\t"
	"psllw $11, %%xmm2		\n\t"
	"psllw $11, %%xmm1		\n\t"
	"psllw $8, %%xmm0		\n\t"
	"psrlw $3, %%xmm1		\n\t"
	"psrlw $3, %%xmm2		\n\t"
	"pmulhw %%xmm5, %%xmm0		\n\t"
	"pmulhw %%xmm6, %%xmm1		\n\t"
	"pmulhw %%xmm7, %%xmm2		\n\t"
	"psllw $10, %%xmm0		\n\t"
	"psllw $5, %%xmm1		\n\t"
	"por %%xmm2, %%xmm0		\n\t"
	"por %%xmm1, %%xmm0		\n\t"
	"movd %%xmm0, %%eax		\n\t"
	"movw %%ax, (%%rsi, %%rcx, 2)	\n\t"
	"incq %%rcx			\n\t"
	"4:				\n\t"
	"cmpq $6, %%rcx			\n\t"
	"jng 3b				\n\t"
	"addq %[blue_mod], %%rsi	\n\t"	/* Blue_mod is the name of a register that now contains bytes_per_line.				*/
	"decq %%rdx			\n\t"
	"jnz 1b				\n\t"
	"jmp 10f			\n\t"	/* We're done!											*/

	"5:				\n\t"	/*  Saturation is required									*/
	"pcmpeqw %%xmm3, %%xmm3		\n\t"	/* Packed Compare Equal Words									*/
						/* From A64_128bit_Media_Programming (p. 276)							*/
						/* This sets xmm3 to 128 1's (since mm6 = mm6)							*/
	"psllw $5, %%xmm3		\n\t"	/* xmm3 = 8 copies of 1111 1111 1110 0000							*/
	"6:				\n\t"
	"movq %%rbx, %%rcx		\n\t"
	"addq $7, %%rcx			\n\t"
	"jns 8f				\n\t"
	"7:				\n\t"
	"movdqu (%%rsi, %%rcx, 2), %%xmm0\n\t"
	"movdqa %%xmm0, %%xmm1		\n\t"
	"movdqa %%xmm0, %%xmm2		\n\t"
	"psrlw $5, %%xmm1		\n\t"
	"psrlw $10, %%xmm0		\n\t"
	"psllw $11, %%xmm2		\n\t"
	"psllw $11, %%xmm1		\n\t"
	"psllw $8, %%xmm0		\n\t"
	"psrlw $3, %%xmm1		\n\t"
	"psrlw $3, %%xmm2		\n\t"
	"pmulhw %%xmm5, %%xmm0		\n\t"
	"pmulhw %%xmm6, %%xmm1		\n\t"
	"pmulhw %%xmm7, %%xmm2		\n\t"
	"paddusw %%xmm3, %%xmm0		\n\t"
	"paddusw %%xmm3, %%xmm1		\n\t"
	"paddusw %%xmm3, %%xmm2		\n\t"
	"psubw %%xmm3, %%xmm0		\n\t"	/* FIXME: This line needs added to the original asm code					*/
	"psubw %%xmm3, %%xmm1		\n\t"
	"psubw %%xmm3, %%xmm2		\n\t"
	"psllw $10, %%xmm0		\n\t"
	"psllw $5, %%xmm1		\n\t"
	"por %%xmm2, %%xmm0		\n\t"
	"por %%xmm1, %%xmm0		\n\t"
	"movdqu %%xmm0, (%%rsi, %%rcx, 2)\n\t"
	"addq $8, %%rcx			\n\t"
	"js 7b				\n\t"
	"jmp 9f				\n\t"
	"8:				\n\t"
	"movw (%%rsi, %%rcx, 2), %%ax	\n\t"
	"movd %%eax, %%xmm0		\n\t"
	"movq %%xmm0, %%xmm1		\n\t"
	"movq %%xmm0, %%xmm2		\n\t"
	"psrlw $5, %%xmm1		\n\t"
	"psrlw $10, %%xmm0		\n\t"
	"psllw $11, %%xmm2		\n\t"
	"psllw $11, %%xmm1		\n\t"
	"psllw $8, %%xmm0		\n\t"
	"psrlw $3, %%xmm1		\n\t"
	"psrlw $3, %%xmm2		\n\t"
	"pmulhw %%xmm5, %%xmm0		\n\t"
	"pmulhw %%xmm6, %%xmm1		\n\t"
	"pmulhw %%xmm7, %%xmm2		\n\t"
	"paddusw %%xmm3, %%xmm0		\n\t"
	"paddusw %%xmm3, %%xmm1		\n\t"
	"paddusw %%xmm3, %%xmm2		\n\t"
	"psubw %%xmm3, %%xmm0		\n\t"	/* FIXME: This line needs added to the original asm code					*/
	"psubw %%xmm3, %%xmm1		\n\t"
	"psubw %%xmm3, %%xmm2		\n\t"
	"psllw $10, %%xmm0		\n\t"
	"psllw $5, %%xmm1		\n\t"
	"por %%xmm2, %%xmm0		\n\t"
	"por %%xmm1, %%xmm0		\n\t"
	"movd %%xmm0, %%eax		\n\t"
	"movw %%ax, (%%rsi, %%rcx, 2)	\n\t"
	"incq %%rcx			\n\t"
	"9:				\n\t"
	"cmpq $6, %%rcx			\n\t"
	"jng 8b				\n\t"
	"addq %[blue_mod], %%rsi	\n\t"	/* Blue_mod is the name of a register that now contains bytes_per_line.				*/
	"decq %%rdx			\n\t"
	"jnz 6b				\n\t"
	"10:				\n\t"	/* This is the end.  Jump here if the line count is zero.					*/
	"emms				\n\t"	/* exit multi-media state (last asm instruction)						*/
	: 					/* outputs: none										*/
						/* inputs: (many operations cannot be performed with a mix of 32bit & 64bit operands directly)	*/
						/*	(however the compiler/assembler can preload 32bit values into 64bit registers)		*/
						/*	(that is why certain variables cannot be referenced by name -- use their register)	*/
	: [data]       "S" (data), 		/*   put the pointer data into the rsi register							*/
	  [width]      "b" (w),			/*   put the width in the %rbx register	(cannot be referenced by name)				*/
	  [height]     "d" (h),			/*   put the heigth in the %rdx register (cannot be referenced by name)				*/
	  [red_mod]    "r" ((unsigned long)(rm)),/*  put the red_modifier   in a register (referenced by name)					*/
	  [green_mod]  "r" ((unsigned long)(gm)),/*  put the green_modifier in a register (referenced by name)					*/
	  [blue_mod]   "r" ((unsigned long)(bm)),/*  put the blue_modifier  in a register (referenced by name)	Later store the bytes_line here	*/
	  [bytes_line] "a" (bpl)		/*   put the bytes_per_line in the %rax register (cannot be referenced by name)			*/
	: "memory"				/* clobbers: (memory includes all the registers)						*/
  );	/*  End of Assembly  */
}


void shade_ximage_16_sse2( volatile void *data, volatile int bpl, volatile int w, volatile int h, volatile int rm, volatile int gm, volatile int bm )
{
  __asm__ __volatile__ (
	".align 16                      \n\t"   /* SIMD instructions should be aligned on 16 byte (128 bit) boundraries for performance reasons.*/
	"leaq -14(%%rsi, %%rbx, 2), %%rsi\n\t"	/* Load the stack index register with a pointer to data + ( width * bytes/pixel ) -6		*/
	"negq %%rbx			\n\t"	/* Negate the width to that we can increment the counter					*/
	"jz 10f				\n\t"	/* Jump to end if the line count is zero							*/
	"movd %[red_mod], %%xmm5	\n\t"	/* Load the color modifiers into mmx registers							*/
	"movd %[green_mod], %%xmm6	\n\t"	/* " "												*/
	"movd %[blue_mod], %%xmm7	\n\t"	/* " "												*/
	"punpcklwd %%xmm5, %%xmm5	\n\t"	/* Unpack and Interleave low words.  From A64_128bit_Media_Programming (p. 380)			*/
	"punpcklwd %%xmm6, %%xmm6	\n\t"	/* Duplicate the bottom 16 bits into the next 16 bits (both operands are the same)		*/
	"punpcklwd %%xmm7, %%xmm7	\n\t"
	"punpckldq %%xmm5, %%xmm5	\n\t"	/* Unpack and Interleave low double words.  From A64_128bit_Media_Programming (p. 376)		*/
	"punpckldq %%xmm6, %%xmm6	\n\t"	/* Duplicate the bottom 32 bits into the next 32 bits (both operands are the same)		*/
	"punpckldq %%xmm7, %%xmm7	\n\t"
	"punpcklqdq %%xmm5, %%xmm5	\n\t"	/* Unpack and Interleave low quad words.  From A64_128bit_Media_Programming (p. 378)		*/
	"punpcklqdq %%xmm6, %%xmm6	\n\t"	/* Duplicate the bottom 64 bits into the next 64 bits (both operands are the same)		*/
	"punpcklqdq %%xmm7, %%xmm7	\n\t"
	"or %[red_mod], %[green_mod]	\n\t"	/* This, and the following 4 instructions, check to see if all three colormodifiers are		*/
	"or %[blue_mod], %[green_mod]	\n\t"	/* less than 256.  If any of the modifiers are > 256 then they will have the 9th, or higher,	*/
	"sar $8, %[green_mod]		\n\t"	/* bit set.  Then we shift off eight bits, leaving something set if a modifier > 256. 		*/
	"movq %%rax, %[blue_mod]	\n\t"	/* Use the register named blue_mod to now store bytes_per_line.		*/
	"xor %[red_mod], %[red_mod]	\n\t"	/* zero red so we don't have to load an immediate value for the following compare.		*/
	"cmp %[red_mod], %[green_mod]	\n\t"	/* Compare the left over bits to zero								*/
	"jg 5f				\n\t"	/* If one of the colors (might) need saturated then jump to the secondary set of loops.		*/
	"1:				\n\t"	/* Start of the outer loop (lines).								*/
	"movq %%rbx, %%rcx		\n\t"	/* Move the width into the count register							*/
	"addq $7, %%rcx			\n\t"	
	"jns 3f				\n\t"
	"2:				\n\t"	/* Start of the inner loop (pixels 8 at a time --> 8 * 16 = 128bits/xmm register )		*/
	"movdqu (%%rsi, %%rcx, 2), %%xmm0\n\t"	/* Load the 16 bits of the pixel (5 bits for red, 6 bits for green, 5 bits for blue)		*/
	"movdqa %%xmm0, %%xmm1		\n\t"	/* Create a copy of the pixel for the green color						*/
	"movdqa %%xmm0, %%xmm2		\n\t"	/* Create a copy of the pixel for the blue color						*/
	"psrlw $5, %%xmm1		\n\t"	/* Packed Shift Right Logical Words								*/
						/* From A64_128bit_Media_Programming (p. 347)							*/
						/* Shifts the blue off of the green color							*/
	"psrlw $11, %%xmm0		\n\t"	/* Shifts the blue & green off of the red color							*/
	"psllw $11, %%xmm2		\n\t"	/* Packed Shift Left Logical Words								*/
						/* From A64_128bit_Media_Programming (p. 330)							*/
						/* Shifts the red & green off of the blue color							*/
	"psllw $10, %%xmm1		\n\t"	/* Shifts the red off of the green color							*/
	"psllw $8, %%xmm0		\n\t"	/* Shifts the red color into position								*/
	"psrlw $2, %%xmm1		\n\t"	/* Shifts the green color into position								*/
	"psrlw $3, %%xmm2		\n\t"	/* Shifts the blue color into position								*/
	"pmulhw %%xmm5, %%xmm0		\n\t"	/* color *= modifier										*/
	"pmulhw %%xmm6, %%xmm1		\n\t"
	"pmulhw %%xmm7, %%xmm2		\n\t"
	"psllw $11, %%xmm0		\n\t"	/* Shift red back into its original position							*/
	"psllw $5, %%xmm1		\n\t"	/* Shift green back into its original position							*/
	"por %%xmm2, %%xmm0		\n\t"	/* Mesh the colors back together								*/
	"por %%xmm1, %%xmm0		\n\t"
	"movdqu %%xmm0, (%%rsi, %%rcx, 2)\n\t"	/* Place the shaded 8 pixels back into the image map						*/
	"addq $8, %%rcx			\n\t"	
	"js 2b				\n\t"
	"jmp 4f				\n\t"
	"3:				\n\t"	/* Deal with pixels one at a time here.								 */
	"movw (%%rsi, %%rcx, 2), %%ax	\n\t"
	"movd %%eax, %%xmm0		\n\t"
	"movq %%xmm0, %%xmm1		\n\t"
	"movq %%xmm0, %%xmm2		\n\t"
	"psrlw $5, %%xmm1		\n\t"
	"psrlw $11, %%xmm0		\n\t"
	"psllw $11, %%xmm2		\n\t"
	"psllw $10, %%xmm1		\n\t"
	"psllw $8, %%xmm0		\n\t"
	"psrlw $2, %%xmm1		\n\t"
	"psrlw $3, %%xmm2		\n\t"
	"pmulhw %%xmm5, %%xmm0		\n\t"
	"pmulhw %%xmm6, %%xmm1		\n\t"
	"pmulhw %%xmm7, %%xmm2		\n\t"
	"psllw $11, %%xmm0		\n\t"
	"psllw $5, %%xmm1		\n\t"
	"por %%xmm2, %%xmm0		\n\t"
	"por %%xmm1, %%xmm0		\n\t"
	"movd %%xmm0, %%eax		\n\t"
	"movw %%ax, (%%rsi, %%rcx, 2)	\n\t"
	"incq %%rcx			\n\t"
	"4:				\n\t"
	"cmpq $6, %%rcx			\n\t"
	"jng 3b				\n\t"
	"addq %[blue_mod], %%rsi	\n\t"	/* Blue_mod is the name of a register that now contains bytes_per_line.				*/
	"decq %%rdx			\n\t"
	"jnz 1b				\n\t"
	"jmp 10f			\n\t"	/* We're done!											*/

	"5:				\n\t"	/*  Saturation is required									*/
	"pcmpeqw %%xmm3, %%xmm3		\n\t"	/* Packed Compare Equal Words									*/
						/* From A64_128bit_Media_Programming (p. 276)							*/
						/* This sets xmm3 to 128 1's (since mm6 = mm6)							*/
	"movdqa %%xmm3, %%xmm4		\n\t"	/* Make copy of 128 ones									*/
	"psllw $5, %%xmm3		\n\t"	/* xmm3 = 8 copies of 1111 1111 1110 0000							*/
	"psllw $6, %%xmm4		\n\t"	/* xmm4 = 8 copies of 1111 1111 1100 0000							*/
	"6:				\n\t"
	"movq %%rbx, %%rcx		\n\t"
	"addq $7, %%rcx			\n\t"
	"jns 8f				\n\t"
	"7:				\n\t"
	"movdqu (%%rsi, %%rcx, 2), %%xmm0\n\t"
	"movdqa %%xmm0, %%xmm1		\n\t"
	"movdqa %%xmm0, %%xmm2		\n\t"
	"psrlw $5, %%xmm1		\n\t"
	"psrlw $11, %%xmm0		\n\t"
	"psllw $11, %%xmm2		\n\t"
	"psllw $10, %%xmm1		\n\t"
	"psllw $8, %%xmm0		\n\t"
	"psrlw $2, %%xmm1		\n\t"
	"psrlw $3, %%xmm2		\n\t"
	"pmulhw %%xmm5, %%xmm0		\n\t"
	"pmulhw %%xmm6, %%xmm1		\n\t"
	"pmulhw %%xmm7, %%xmm2		\n\t"
	"paddusw %%xmm3, %%xmm0		\n\t"
	"paddusw %%xmm4, %%xmm1		\n\t"
	"paddusw %%xmm3, %%xmm2		\n\t"
	"psubw %%xmm4, %%xmm1		\n\t"
	"psubw %%xmm3, %%xmm2		\n\t"
	"psllw $11, %%xmm0		\n\t"
	"psllw $5, %%xmm1		\n\t"
	"por %%xmm2, %%xmm0		\n\t"
	"por %%xmm1, %%xmm0		\n\t"
	"movdqu %%xmm0, (%%rsi, %%rcx, 2)\n\t"
	"addq $8, %%rcx			\n\t"
	"js 7b				\n\t"
	"jmp 9f				\n\t"
	"8:				\n\t"
	"movw (%%rsi, %%rcx, 2), %%ax	\n\t"
	"movd %%eax, %%xmm0		\n\t"
	"movq %%xmm0, %%xmm1		\n\t"
	"movq %%xmm0, %%xmm2		\n\t"
	"psrlw $5, %%xmm1		\n\t"
	"psrlw $11, %%xmm0		\n\t"
	"psllw $11, %%xmm2		\n\t"
	"psllw $10, %%xmm1		\n\t"
	"psllw $8, %%xmm0		\n\t"
	"psrlw $2, %%xmm1		\n\t"
	"psrlw $3, %%xmm2		\n\t"
	"		\n\t"
	"pmulhw %%xmm5, %%xmm0		\n\t"
	"pmulhw %%xmm6, %%xmm1		\n\t"
	"pmulhw %%xmm7, %%xmm2		\n\t"
	"		\n\t"
	"paddusw %%xmm3, %%xmm0		\n\t"
	"paddusw %%xmm4, %%xmm1		\n\t"
	"paddusw %%xmm3, %%xmm2		\n\t"
	"		\n\t"
	"psubw %%xmm4, %%xmm1		\n\t"
	"psubw %%xmm3, %%xmm2		\n\t"
	"		\n\t"
	"psllw $11, %%xmm0		\n\t"
	"psllw $5, %%xmm1		\n\t"
	"por %%xmm2, %%xmm0		\n\t"
	"por %%xmm1, %%xmm0		\n\t"
	"movd %%xmm0, %%eax		\n\t"
	"movw %%ax, (%%rsi, %%rcx, 2)	\n\t"
	"incq %%rcx			\n\t"
	"9:				\n\t"
	"cmpq $6, %%rcx			\n\t"
	"jng 8b				\n\t"
	"addq %[blue_mod], %%rsi	\n\t"	/* Blue_mod is the name of a register that now contains bytes_per_line.				*/
	"decq %%rdx			\n\t"
	"jnz 6b				\n\t"
	"10:				\n\t"	/* This is the end.  Jump here if the line count is zero.					*/
	"emms				\n\t"	/* exit multi-media state (last asm instruction)						*/
	: 					/* outputs: none										*/
						/* inputs: (many operations cannot be performed with a mix of 32bit & 64bit operands directly)	*/
						/*	(however the compiler/assembler can preload 32bit values into 64bit registers)		*/
						/*	(that is why certain variables cannot be referenced by name -- use their register)	*/
	: [data]       "S" (data), 		/*   put the pointer data into the rsi register							*/
	  [width]      "b" (w),			/*   put the width in the %rbx register	(cannot be referenced by name)				*/
	  [height]     "d" (h),			/*   put the heigth in the %rdx register (cannot be referenced by name)				*/
	  [red_mod]    "r" ((unsigned long)(rm)),/*  put the red_modifier   in a register (referenced by name)					*/
	  [green_mod]  "r" ((unsigned long)(gm)),/*  put the green_modifier in a register (referenced by name)					*/
	  [blue_mod]   "r" ((unsigned long)(bm)),/*  put the blue_modifier  in a register (referenced by name)	Later store the bytes_line here	*/
	  [bytes_line] "a" (bpl)		/*   put the bytes_per_line in the %rax register (cannot be referenced by name)			*/
	: "memory"				/* clobbers: (memory includes all the registers)						*/
  );	/*  End of Assembly  */
}


void shade_ximage_32_sse2( volatile void *data, volatile int bpl, volatile int w, volatile int h, volatile int rm, volatile int gm, volatile int bm )
{
  __asm__ __volatile__ (
	".align 16                      \n\t"   /* SIMD instructions should be aligned on 16 byte (128 bit) boundraries for performance reasons.*/
	"leaq -4(%%rsi, %%rbx, 4), %%rsi\n\t"	/* From A64_General_Purpose_and_System_Instructions (p. 182)					*/
						/* Intel syntax section:[base + index*scale + disp]  (used by AMD manuals)			*/
						/* AT&T  syntax section:disp(base, index, scale)     (used by gas/gcc)				*/
						/* Load Effective Address of (rsi + (rbx * size)) into rsi					*/
						/* 32 bits per pixel means a multiplier of 4.							*/
	"negq %%rbx			\n\t"	/* two's compliment negation of ebx (width) and sets the Zero Flag based on the results		*/
						/* From A64_General_Purpose_and_System_Instructions (p. 212)					*/
	"jz 10f				\n\t"	/* Jump to label 3 forward on Zero								*/
						/* Basically if width = 0 blowout								*/
						/* I don't understand why the height isn't checked (shouldn't matter, zero loop iterations)	*/
	"movd %[red_mod], %%xmm4	\n\t"	/* move red modifier into mm4 w/ zero extension to 128bits					*/
						/* RGB's are 8 bit values. regardless of them coming in in 32/64 bit they are zero extended	*/
	"psllq $16, %%xmm4		\n\t"	/* Packed Shift Left Logical Quad words (left shift mm4 16bits twice, once for each 64bit value)*/
						/* From A64_128bit_Media_Programming (p. 328)							*/
	"movd %[green_mod], %%xmm5	\n\t"	/* move green modifier into mm5 w/ zero extension to 128bits					*/
	"por %%xmm5, %%xmm4		\n\t"	/* Mesh green modifier into color modifier							*/
	"psllq $16, %%xmm4		\n\t"	/* Packed Shift Left Logical Quad words (left shift mm4 16bits twice, once for each 64bit value)*/
	"movd %[blue_mod], %%xmm5	\n\t"	/* move blue modifier (32 bits) into mm4 w/ zero extension to 128bits				*/
	"por %%xmm5, %%xmm4		\n\t"	/* Mesh blue modifier into color modifier							*/
						/* mm4 (color modifier) now contains 00 00 00 00 : 00 00 00 00 :: 00 00 00 rm : 00 gm 00 bm	*/
        "punpcklqdq %%xmm4, %%xmm4      \n\t"   /* Unpack and Interleave low quad words.  From A64_128bit_Media_Programming (p. 378)            */
           					/* Duplicate the bottom 64 bits into the next 64 bits (both operands are the same)              */
	"pcmpeqw %%xmm6, %%xmm6		\n\t"	/* Packed Compare Equal Words									*/
						/* From A64_128bit_Media_Programming (p. 276)							*/
						/* This sets mm6 to 128 1's (since mm6 = mm6)							*/
	"psllw $15, %%xmm6		\n\t"	/* Packed Shift Left Logical Words								*/
						/* From A64_128bit_Media_Programming (p. 330)							*/
						/* This sets 8 16 bit values of  1000 0000 0000 0000 in the 128 bit word			*/
	"movdqa %%xmm6, %%xmm5		\n\t"	/* Copy mm6 to mm5 (we need mm6 later)								*/
	"pmulhw %%xmm4, %%xmm5		\n\t"	/* Packed Multiply High Signed Word								*/
						/* mm4 = ( mm4 * mm5 ) >> 16  (8 times, once for each 16bit value)				*/
						/* For each color_ modifier (cm)								*/
						/*   (( cm * 80 00 ) >> 16 ) = (( cm << 15 ) >> 16 )  = cm >> 1					*/
	"1:				\n\t"	/* The start of the outer loop (lines)								*/
	"movq %%rbx, %%rcx		\n\t"	/* Load the counting register (rcx) with the width of the window to shade			*/
	"incq %%rcx			\n\t"
	"2:				\n\t"	/* The start of the inner loop (columns)							*/
	"movq (%%rsi, %%rcx, 4), %%xmm1	\n\t"	/* sets mm1 to the 32bit color in the image map (data[ rcx ])					*/
						/* 32 bit color is still 4 bytes so leave the multiplier alone it is zero extended to 128 bits	*/
						/* only move 32 bits with movd so we don't get two pixels worth of colors			*/
	"pxor %%xmm0, %%xmm0		\n\t"	/* 128bit exclusive or (sets mm0 to 0)								*/
	"punpcklbw %%xmm1, %%xmm0	\n\t"   /* Unpack and interleave low bytes								*/
						/* For each color of the pixel expand to 16 bits and shift left 8 bits				*/
						/* From A64_128bit_Media_Programming (p. 374)							*/
						/* discard high 64 bits and expand both mm0 and mm1 a byte at a time into mm0 (mm0 first)	*/
	"pxor %%xmm6, %%xmm0		\n\t"	/* This flips the sign of the 16 bit red, green, and blue colors. (mm6 ~= 1000:0000 8 times)	*/
	"pmulhw %%xmm4, %%xmm0		\n\t"	/* Package Multiply High Signed Word  (an SSE2 instruction) 128bit     mm0=color  mm4=cm	*/
						/* Each 16 bit signed int in mm4 (8) is multiplied by the same in mm0				*/
						/*    and the high 16 bits of the result replace the 16 bits used from mm0			*/
						/* For (( each 16 bit color * each 16 bit color modifier ) >> 16 )				*/
	"psubw %%xmm5, %%xmm0		\n\t"	/* Packed Subtract Words									*/
						/* From A64_128bit_Media_Programming (p. 364)							*/
						/* mm0=modified color  mm5=corrected color modifier. mm0 = ( mm0 - mm5 )			*/
						/* 16 bit corrected modified color = ( modified color - corrected color modifier )		*/
	"packuswb %%xmm0, %%xmm0	\n\t"	/* Pack with Saturation Signed Word to Unsigned Byte						*/
						/* From A64_128bit_Media_Programming (p. 246)							*/
						/* if mm0 > 255 then mm0=255 elsif mm0 < 0 mm0=0 else mm0=mm0					*/
						/* The top 64 bits are now trashed.  The remaining 64 bits are 2 pixels				*/
	"movq %%xmm0, (%%rsi, %%rcx, 4)	\n\t"	/* puts the new 32 bit color value back into the data (image map)				*/
						/* 32 bit color is still a double word so movd stays movd					*/
	"addq $2, %%rcx			\n\t"	/* Increment the count register (more pixels left)						*/
	"js 2b				\n\t"	/* Jump backwards to label 2 (restart inner loop) on negative (more pixels left)		*/
	"jmp 5f				\n\t"	/* Jump to single pixel section after pairs are exhausted					*/
	"4:				\n\t"	/* The start of the inner loop (columns)							*/
	"movd (%%rsi, %%rcx, 4), %%xmm1	\n\t"	/* sets mm1 to the 32bit color in the image map (data[ rcx ])					*/
						/* 32 bit color is still 4 bytes so leave the multiplier alone it is zero extended to 128 bits	*/
						/* only move 32 bits with movd so we don't get two pixels worth of colors			*/
	"pxor %%xmm0, %%xmm0		\n\t"	/* 128bit exclusive or (sets mm0 to 0)								*/
	"punpcklbw %%xmm1, %%xmm0	\n\t"   /* Unpack and interleave low bytes								*/
						/* For each color of the pixel expand to 16 bits and shift left 8 bits				*/
						/* From A64_128bit_Media_Programming (p. 374)							*/
						/* discard high 64 bits and expand both mm0 and mm1 a byte at a time into mm0 (mm0 first)	*/
	"pxor %%xmm6, %%xmm0		\n\t"	/* This flips the sign of the 16 bit red, green, and blue colors. (mm6 ~= 1000:0000 8 times)	*/
	"pmulhw %%xmm4, %%xmm0		\n\t"	/* Package Multiply High Signed Word  (an SSE2 instruction) 128bit     mm0=color  mm4=cm	*/
						/* Each 16 bit signed int in mm4 (8) is multiplied by the same in mm0				*/
						/*    and the high 16 bits of the result replace the 16 bits used from mm0			*/
						/* For (( each 16 bit color * each 16 bit color modifier ) >> 16 )				*/
	"psubw %%xmm5, %%xmm0		\n\t"	/* Packed Subtract Words									*/
						/* From A64_128bit_Media_Programming (p. 364)							*/
						/* mm0=modified color  mm5=corrected color modifier. mm0 = ( mm0 - mm5 )			*/
						/* 16 bit corrected modified color = ( modified color - corrected color modifier )		*/
	"packuswb %%xmm0, %%xmm0	\n\t"	/* Pack with Saturation Signed Word to Unsigned Byte						*/
						/* From A64_128bit_Media_Programming (p. 246)							*/
						/* if mm0 > 255 then mm0=255 elsif mm0 < 0 mm0=0 else mm0=mm0					*/
	"movd %%xmm0, (%%rsi, %%rcx, 4)	\n\t"	/* puts the new 32 bit color value back into the data (image map)				*/
						/* 32 bit color is still a double word so movd stays movd					*/
	"incq %%rcx			\n\t"	/* Increment the count register (more pixels left)						*/
	"5:				\n\t"	/* Jump here after all pairs of pixels are exhausted						*/
	"cmpq $0, %%rcx			\n\t"	/* Increment the count register (more pixels left)						*/
	"jng 4b				\n\t"	/* Jump backwards to label 2 (restart inner loop) on NOT zero (more pixels left)		*/

	"addq %%rax, %%rsi		\n\t"	/* Add bytes per line to the data pointer (advance the pointer to the next line)		*/
	"decq %%rdx			\n\t"	/* Decrement the dx register (row count)							*/
	"jnz 1b				\n\t"	/* Jump backwards to label 1 (restart outer loop) if not zero (more rows left)			*/
	"10:				\n\t"	/* End of function (jump here to clean up and return to caller					*/
	"emms				\n\t"	/* exit multi-media state (last asm instruction)						*/
	: 					/* outputs: none										*/
						/* inputs: (many operations cannot be performed with a mix of 32bit & 64bit operands directly)	*/
						/*	(however the compiler/assembler can preload 32bit values into 64bit registers)		*/
						/*	(that is why certain variables cannot be referenced by name -- use their register)	*/
	: [data]       "S" (data), 		/*   put the pointer data into the rsi register							*/
	  [width]      "b" (w),			/*   put the width in the %rbx register	(cannot be referenced by name)				*/
	  [height]     "d" (h),			/*   put the heigth in the %rdx register (cannot be referenced by name)				*/
	  [red_mod]    "r" (rm),		/*   put the red_modifier   in a register (referenced by name)					*/
	  [green_mod]  "r" (gm),		/*   put the green_modifier in a register (referenced by name)					*/
	  [blue_mod]   "r" (bm),		/*   put the blue_modifier  in a register (referenced by name)					*/
	  [bytes_line] "a" (bpl)		/*   put the bytes_per_line in the %rax register (cannot be referenced by name)			*/
	: "memory"				/* clobbers: (memory includes all the registers)						*/
  );	/*  End of Assembly  */
}

#endif
