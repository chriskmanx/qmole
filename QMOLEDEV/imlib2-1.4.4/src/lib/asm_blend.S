#include <config.h>
#include "asm.h"

#ifdef DO_MMX_ASM

/*\ 
|*| MMX assembly blending routines for Imlib2
|*| Written by Willem Monsuwe <willem@stack.nl>
|*|
|*| Special (hairy) constructs are only commented on first use.
\*/

/*\ All functions have the same calling convention:
|*|  __imlib_mmx_<op>_rgba_to_rgb[A](void *src, int sw, void *dst, int dw,
|*|				     int w, int h, ImlibColorModifier *cm)
\*/

#define src	8(%ebp)
#define sw	12(%ebp)
#define dst	16(%ebp)
#define dw	20(%ebp)
#define w	24(%ebp)
#define h	28(%ebp)
#define cm	32(%ebp)

.text
	.align 8
FN_(imlib_mmx_blend_rgba_to_rgb)
FN_(imlib_mmx_blend_rgba_to_rgba)
FN_(imlib_mmx_copy_rgba_to_rgb)
FN_(imlib_mmx_copy_rgba_to_rgba)
FN_(imlib_mmx_copy_rgb_to_rgba)

FN_(imlib_mmx_add_blend_rgba_to_rgb)
FN_(imlib_mmx_add_blend_rgba_to_rgba)
FN_(imlib_mmx_add_copy_rgba_to_rgb)
FN_(imlib_mmx_add_copy_rgba_to_rgba)
FN_(imlib_mmx_add_copy_rgb_to_rgba)

FN_(imlib_mmx_subtract_blend_rgba_to_rgb)
FN_(imlib_mmx_subtract_blend_rgba_to_rgba)
FN_(imlib_mmx_subtract_copy_rgba_to_rgb)
FN_(imlib_mmx_subtract_copy_rgba_to_rgba)
FN_(imlib_mmx_subtract_copy_rgb_to_rgba)

FN_(imlib_mmx_reshade_blend_rgba_to_rgb)
FN_(imlib_mmx_reshade_blend_rgba_to_rgba)
FN_(imlib_mmx_reshade_copy_rgba_to_rgb)
FN_(imlib_mmx_reshade_copy_rgba_to_rgba)
FN_(imlib_mmx_reshade_copy_rgb_to_rgba)

#include "asm_loadimmq.S"

	
/*\ MMX register use:
|*| %mm1 = Source value
|*| %mm2 = Destination value
|*| %mm3 = Alpha value
|*| %mm4 = 0
|*| %mm5-%mm6 = masks
\*/

/*\ Common code \*/
/*\ Set MMX mode, save registers, load common parameters \*/
#define ENTER				\
	pushl %ebp			;\
	movl  %esp, %ebp		;\
	pushl %ebx			;\
	pushl %ecx			;\
	pushl %edx			;\
	pushl %edi			;\
	pushl %esi			;\
	movl  h,    %edx		;\
	movl  w,    %ebx		;\
	movl  src,  %esi		;\
	movl  dst,  %edi		;\
	leal (%esi, %ebx, 4), %esi	;\
	leal (%edi, %ebx, 4), %edi	;\
	negl %ebx			;\
	jz 9f				;\
	incl  %edx			;\
	decl  %edx			;\
	jz 9f				;\
	decl  %edx			;\

#define LOOP_START		\
8:				;\
	movl %ebx, %ecx

#define LOOP_END			\
	movl sw, %ecx			;\
	leal (%esi, %ecx, 4), %esi	;\
	movl dw, %ecx			;\
	leal (%edi, %ecx, 4), %edi	;\
	decl %edx			;\
	jns 8b


/*\ Unset MMX mode, reset registers, return \*/
#define LEAVE			\
9:				;\
	emms			;\
	popl %esi		;\
	popl %edi		;\
	popl %edx		;\
	popl %ecx		;\
	popl %ebx		;\
	movl %ebp, %esp		;\
	popl %ebp		;\
	ret


PR_(imlib_mmx_blend_rgba_to_rgb):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(c1, %mm5)
	CLEANUP_IMMQ_LOADS(1)

	LOOP_START
1:
	/*\ Load source and destination \*/
	movd (%esi, %ecx, 4), %mm1
	movd (%edi, %ecx, 4), %mm2

	/*\ Get alpha from source and unpack/copy to eight bytes
	|*|  which are treated as four words.
	|*| Result ranges from [0, 0x7fff), and is mapped to
	|*|  point value in [0.0, 1.0) by using the high word
	|*|  of the 16->32 multiplications.
	|*| (Because we want the unsigned value we shift one bit,
	|*|  and also shift the other factor to compensate.)
	|*| Magic to get the fourth byte: lhh
	\*/
	movq %mm1, %mm3
	punpcklbw %mm3, %mm3
	punpckhwd %mm3, %mm3
	punpckhdq %mm3, %mm3
	psrlw $1, %mm3

	/*\ Make the alpha value that gets multiplied to the
	|*|  alpha channels 0, so the resulting alpha value is
	|*|  the destination alpha value.
	\*/
	psrlq $16, %mm3

	/*\ Unpack source and destination, bytes to words \*/
	punpcklbw %mm4, %mm1
	punpcklbw %mm4, %mm2

	/*\ d = d + (a * ((s - d) + 0.5)) \*/
	psubw %mm2, %mm1
	psllw $1, %mm1
	paddw %mm5, %mm1	/*\ Roundoff \*/
	pmulhw %mm3, %mm1
	paddw %mm1, %mm2

	/*\ Pack into lower 4 bytes and save \*/
	packuswb %mm4, %mm2
	movd %mm2, (%edi, %ecx, 4)

	incl %ecx
	js 1b

	LOOP_END
	LEAVE

SIZE(imlib_mmx_blend_rgba_to_rgb)

PR_(imlib_mmx_blend_rgba_to_rgba):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(m0X000000, %mm5)
	LOAD_IMMQ(m00XXXXXX, %mm6)
	LOAD_IMMQ(c1, %mm7)
	CLEANUP_IMMQ_LOADS(3)

	LOOP_START
1:
	/*\ Load source and destination \*/
	movd (%esi, %ecx, 4), %mm1
	movd (%edi, %ecx, 4), %mm2

	/*\ Get alpha from source and target, a = src + (255 - dest) \*/
	movq %mm2, %mm3
	pxor %mm6, %mm3
	paddusb %mm1, %mm3
	/*\ Unpack/copy to eight bytes \*/
	punpcklbw %mm3, %mm3
	punpckhwd %mm3, %mm3
	punpckhdq %mm3, %mm3
	psrlw $1, %mm3

	/*\ Unpack source and destination, bytes to words \*/
	punpcklbw %mm4, %mm1
	punpcklbw %mm4, %mm2

	/*\ Separate alpha channel \*/
	movq %mm1, %mm0
	pand %mm5, %mm0

	/*\ d = d + (a * ((s - d) + 0.5)) \*/
	psubw %mm2, %mm1
	psllw $1, %mm1
	paddw %mm7, %mm1	/*\ Roundoff \*/
	pmulhw %mm3, %mm1

	/*\ Replace alpha channel with separated out version in mm0 and add \*/
	pand %mm6, %mm1
	por %mm0, %mm1
	paddw %mm1, %mm2

	/*\ Pack into lower 4 bytes and save \*/
	packuswb %mm4, %mm2
	movd %mm2, (%edi, %ecx, 4)

	incl %ecx
	js 1b

	LOOP_END
	LEAVE

SIZE(imlib_mmx_blend_rgba_to_rgba)

PR_(imlib_mmx_copy_rgba_to_rgb):
	ENTER

	LOAD_IMMQ(m0XXX0XXX, %mm5)
	LOAD_IMMQ(mX000X000, %mm6)
	CLEANUP_IMMQ_LOADS(2)

	/*\ Two at a time: last item is at %ecx = 0 \*/
	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ Clear alpha channel of source, get alpha of destination \*/
	pand %mm5, %mm1
	pand %mm6, %mm2

	/*\ d = d | s, and save \*/
	por %mm1, %mm2
	movq %mm2, (%edi, %ecx, 4)

	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2

	pand %mm5, %mm1
	pand %mm6, %mm2

	por %mm1, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_copy_rgba_to_rgb)

PR_(imlib_mmx_copy_rgba_to_rgba):
	ENTER

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source, save destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq %mm1, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd %mm1, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_copy_rgba_to_rgba)

PR_(imlib_mmx_copy_rgb_to_rgba):
	ENTER

	LOAD_IMMQ(mX000X000, %mm5)
	CLEANUP_IMMQ_LOADS(1)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source, save destination \*/
	movq (%esi, %ecx, 4), %mm1
	por %mm5, %mm1
	movq %mm1, (%edi, %ecx, 4)

	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	por %mm5, %mm1
	movd %mm1, (%edi)
3:

	LOOP_END
	LEAVE

SIZE(imlib_mmx_copy_rgb_to_rgba)

PR_(imlib_mmx_add_blend_rgba_to_rgb):
	ENTER

	pxor %mm4, %mm4

	LOOP_START
1:
	/*\ Load source and destination \*/
	movd (%esi, %ecx, 4), %mm1
	movd (%edi, %ecx, 4), %mm2

	/*\ Get alpha from source and unpack/copy to eight bytes \*/
	movq %mm1, %mm3
	punpcklbw %mm3, %mm3
	punpckhwd %mm3, %mm3
	punpckhdq %mm3, %mm3
	psrlw $1, %mm3

	psrlq $16, %mm3

	/*\ Unpack source and destination, bytes to words \*/
	punpcklbw %mm4, %mm1
	punpcklbw %mm4, %mm2

	/*\ d = d + (a * s) \*/
	psllw $1, %mm1
	pmulhw %mm3, %mm1
	paddw %mm1, %mm2

	/*\ Pack into lower 4 bytes and save \*/
	packuswb %mm4, %mm2
	movd %mm2, (%edi, %ecx, 4)
	
	incl %ecx
	js 1b

	LOOP_END
	LEAVE

SIZE(imlib_mmx_add_blend_rgba_to_rgb)

PR_(imlib_mmx_add_blend_rgba_to_rgba):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(mVX000000, %mm5)
	LOAD_IMMQ(m00XXXXXX, %mm6)
	CLEANUP_IMMQ_LOADS(2)

	LOOP_START
1:
	/*\ Load source and destination \*/
	movd (%esi, %ecx, 4), %mm1
	movd (%edi, %ecx, 4), %mm2

	/*\ Get alpha from source and target and unpack/copy to eight bytes \*/
	movq %mm2, %mm3
	pxor %mm6, %mm3
	paddusb %mm1, %mm3
	punpcklbw %mm3, %mm3
	punpckhwd %mm3, %mm3
	punpckhdq %mm3, %mm3
	psrlw $1, %mm3

	/*\ Make the alpha value that gets multiplied to the
	|*|  alpha channels 0x7fff, so the resulting alpha value is
	|*|  the sum of the source and destination alpha values.
	\*/
	por %mm5, %mm3

	/*\ Unpack source and destination, bytes to words \*/
	punpcklbw %mm4, %mm1
	punpcklbw %mm4, %mm2

	/*\ d = d + (a * s) \*/
	psllw $1, %mm1
	pmulhw %mm3, %mm1
	paddw %mm1, %mm2

	/*\ Pack into lower 4 bytes and save \*/
	packuswb %mm4, %mm2
	movd %mm2, (%edi, %ecx, 4)
	
	incl %ecx
	js 1b

	LOOP_END
	LEAVE

SIZE(imlib_mmx_add_blend_rgba_to_rgba)

PR_(imlib_mmx_add_copy_rgba_to_rgb):
	ENTER

	LOAD_IMMQ(m0XXX0XXX, %mm5)
	CLEANUP_IMMQ_LOADS(1)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ Clear alpha channel of source \*/
	pand %mm5, %mm1

	/*\ d = d + s, unsigned saturation, and save \*/
	paddusb %mm1, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	pand %mm5, %mm1
	paddusb %mm1, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_add_copy_rgba_to_rgb)

PR_(imlib_mmx_add_copy_rgba_to_rgba):
	ENTER

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ d = d + s, unsigned saturation, and save \*/
	paddusb %mm1, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	paddusb %mm1, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_add_copy_rgba_to_rgba)

PR_(imlib_mmx_add_copy_rgb_to_rgba):
	ENTER

	LOAD_IMMQ(mX000X000, %mm5)
	CLEANUP_IMMQ_LOADS(1)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ d = d + s, unsigned saturation, and save \*/
	paddusb %mm1, %mm2

	/*\ Make result alpha 0xff \*/
	por %mm5, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	paddusb %mm1, %mm2
	por %mm5, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_add_copy_rgb_to_rgba)

PR_(imlib_mmx_subtract_blend_rgba_to_rgb):
	ENTER

	pxor %mm4, %mm4

	LOOP_START
1:
	/*\ Load source and destination \*/
	movd (%esi, %ecx, 4), %mm1
	movd (%edi, %ecx, 4), %mm2

	/*\ Get alpha from source and unpack/copy to eight bytes \*/
	movq %mm1, %mm3
	punpcklbw %mm3, %mm3
	punpckhwd %mm3, %mm3
	punpckhdq %mm3, %mm3
	psrlw $1, %mm3

	psrlq $16, %mm3

	/*\ Unpack source and destination, bytes to words \*/
	punpcklbw %mm4, %mm1
	punpcklbw %mm4, %mm2

	/*\ d = d - (a * s) \*/
	psllw $1, %mm1
	pmulhw %mm3, %mm1
	psubw %mm1, %mm2

	/*\ Pack into lower 4 bytes and save \*/
	packuswb %mm4, %mm2
	movd %mm2, (%edi, %ecx, 4)
	
	incl %ecx
	js 1b

	LOOP_END
	LEAVE

SIZE(imlib_mmx_subtract_blend_rgba_to_rgb)

PR_(imlib_mmx_subtract_blend_rgba_to_rgba):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(mV0000000, %mm5)
	LOAD_IMMQ(m00XXXXXX, %mm6)
	CLEANUP_IMMQ_LOADS(2)

	LOOP_START
1:
	/*\ Load source and destination \*/
	movd (%esi, %ecx, 4), %mm1
	movd (%edi, %ecx, 4), %mm2

	/*\ Get alpha from source and target and unpack/copy to eight bytes \*/
	movq %mm2, %mm3
	pxor %mm6, %mm3
	paddusb %mm1, %mm3
	punpcklbw %mm3, %mm3
	punpckhwd %mm3, %mm3
	punpckhdq %mm3, %mm3
	psrlw $1, %mm3

	/*\ Make alpha value that gets multiplied with alpha channel
	|*| 0x8000, (-1.0), so that the alpha result is s + d
	\*/
	psrlq $16, %mm3
	por %mm5, %mm3

	/*\ Unpack source and destination, bytes to words \*/
	punpcklbw %mm4, %mm1
	punpcklbw %mm4, %mm2

	/*\ d = d - (a * s) \*/
	psllw $1, %mm1
	pmulhw %mm3, %mm1
	psubw %mm1, %mm2

	/*\ Pack into lower 4 bytes and save \*/
	packuswb %mm4, %mm2
	movd %mm2, (%edi, %ecx, 4)
	
	incl %ecx
	js 1b

	LOOP_END
	LEAVE

SIZE(imlib_mmx_subtract_blend_rgba_to_rgba)

PR_(imlib_mmx_subtract_copy_rgba_to_rgb):
	ENTER

	LOAD_IMMQ(m0XXX0XXX, %mm5)
	CLEANUP_IMMQ_LOADS(1)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ Clear alpha channel of source \*/
	pand %mm5, %mm1

	/*\ d = d - s, unsigned saturation, and save \*/
	psubusb %mm1, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	pand %mm5, %mm1
	psubusb %mm1, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_subtract_copy_rgba_to_rgb)

PR_(imlib_mmx_subtract_copy_rgba_to_rgba):
	ENTER

	LOAD_IMMQ(mX000X000, %mm5)
	CLEANUP_IMMQ_LOADS(1)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ Negate destination alphas \*/
	pxor %mm5, %mm2

	/*\ d = d - s, unsigned saturation, and save \*/
	psubusb %mm1, %mm2

	/*\ Negate result alphas \*/
	pxor %mm5, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	pxor %mm5, %mm2
	psubusb %mm1, %mm2
	pxor %mm5, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_subtract_copy_rgba_to_rgba)

PR_(imlib_mmx_subtract_copy_rgb_to_rgba):
	ENTER

	LOAD_IMMQ(mX000X000, %mm5)
	CLEANUP_IMMQ_LOADS(1)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ d = d - s, unsigned saturation, and save \*/
	psubusb %mm1, %mm2

	/*\ Make result alpha 0xff \*/
	por %mm5, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	psubusb %mm1, %mm2
	por %mm5, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_subtract_copy_rgb_to_rgba)

PR_(imlib_mmx_reshade_blend_rgba_to_rgb):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(m000V0V0V, %mm6)
	CLEANUP_IMMQ_LOADS(1)

	LOOP_START
1:
	/*\ Load source and destination \*/
	movd (%esi, %ecx, 4), %mm1
	movd (%edi, %ecx, 4), %mm2

	/*\ Get alpha from source and unpack/copy to eight bytes \*/
	movq %mm1, %mm3
	punpcklbw %mm3, %mm3
	punpckhwd %mm3, %mm3
	punpckhdq %mm3, %mm3
	psrlw $1, %mm3

	psrlq $16, %mm3

	/*\ Unpack source and destination, bytes to words \*/
	punpcklbw %mm4, %mm1
	punpcklbw %mm4, %mm2

	/*\ d = d + (2 * a * (s - 127)) \*/
	psubw %mm6, %mm1
	psllw $2, %mm1
	pmulhw %mm3, %mm1
	paddw %mm1, %mm2

	/*\ Pack into lower 4 bytes and save \*/
	packuswb %mm4, %mm2
	movd %mm2, (%edi, %ecx, 4)
	
	incl %ecx
	js 1b

	LOOP_END
	LEAVE

SIZE(imlib_mmx_reshade_blend_rgba_to_rgb)

PR_(imlib_mmx_reshade_blend_rgba_to_rgba):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(mI0000000, %mm5)
	LOAD_IMMQ(m000V0V0V, %mm6)
	LOAD_IMMQ(m00XXXXXX, %mm7)
	CLEANUP_IMMQ_LOADS(3)

	LOOP_START
1:
	/*\ Load source and destination \*/
	movd (%esi, %ecx, 4), %mm1
	movd (%edi, %ecx, 4), %mm2

	/*\ Get alpha from source and target and unpack/copy to eight bytes \*/
	movq %mm2, %mm3
	pxor %mm7, %mm3
	paddusb %mm1, %mm3
	punpcklbw %mm3, %mm3
	punpckhwd %mm3, %mm3
	punpckhdq %mm3, %mm3
	psrlw $1, %mm3

	/*\ Make the alpha value that gets multiplied to the
	|*|  alpha channels 0x4000 (0.5), so the resulting alpha value is
	|*|  the sum of the source and destination alpha values.
	\*/
	psrlq $16, %mm3
	por %mm5, %mm3

	/*\ Unpack source and destination, bytes to words \*/
	punpcklbw %mm4, %mm1
	punpcklbw %mm4, %mm2

	/*\ d = d + (2 * a * (s - 127)), (alpha channel: d = d + (2 * 0.5 * (s - 0)) ) \*/
	psubw %mm6, %mm1
	psllw $2, %mm1
	pmulhw %mm3, %mm1
	paddw %mm1, %mm2

	/*\ Pack into lower 4 bytes and save \*/
	packuswb %mm4, %mm2
	movd %mm2, (%edi, %ecx, 4)
	
	incl %ecx
	js 1b

	LOOP_END
	LEAVE

SIZE(imlib_mmx_reshade_blend_rgba_to_rgba)

PR_(imlib_mmx_reshade_copy_rgba_to_rgb):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(m0XXX0XXX, %mm5)
	LOAD_IMMQ(m0VVV0VVV, %mm6)
	CLEANUP_IMMQ_LOADS(2)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ To take advantage of saturation and be able to do 8 bytes
	|*|  at a time, we divide reshading into two separate steps:
	|*|  adding values above 128, and subtracting values below 128
	|*| These values go into %mm1 and %mm3 respectively
	|*| - %mm1 becomes (2 * (s - 127))
	|*| - %mm3 becomes (2 * (255 - (127 + s))) = (2 * (128 - s))
	\*/
	movq %mm1, %mm3
	psubusb %mm6, %mm1
	paddusb %mm1, %mm1
	paddusb %mm6, %mm3
	pxor %mm5, %mm3
	paddusb %mm3, %mm3

	/*\ Clear alpha channel of s1 and s2 \*/
	pand %mm5, %mm1
	pand %mm5, %mm3

	/*\ d = d + s1 - s2, unsigned saturation, and save \*/
	paddusb %mm1, %mm2
	psubusb %mm3, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	movq %mm1, %mm3
	psubusb %mm6, %mm1
	paddusb %mm1, %mm1
	paddusb %mm6, %mm3
	pxor %mm5, %mm3
	paddusb %mm3, %mm3
	pand %mm5, %mm1
	pand %mm5, %mm3
	paddusb %mm1, %mm2
	psubusb %mm3, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_reshade_copy_rgba_to_rgb)

PR_(imlib_mmx_reshade_copy_rgba_to_rgba):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(m0XXX0XXX, %mm5)
	LOAD_IMMQ(m0VVV0VVV, %mm6)
	CLEANUP_IMMQ_LOADS(2)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	/*\ This time, the alpha channels have to be added.
	|*| For that, the alpha channel of %mm1 should remain
	|*|  the same.  This is done by subtracting 0 from the
	|*|  alpha channel, and then doing the *2 via a separate
	|*|  register, clearing its alpha channel first.
	\*/
	movq %mm1, %mm3
	psubusb %mm6, %mm1
	movq %mm1, %mm0
	pand %mm5, %mm0
	paddusb %mm0, %mm1
	paddusb %mm6, %mm3
	pxor %mm5, %mm3
	paddusb %mm3, %mm3

	/*\ Clear alpha channel of s2 \*/
	pand %mm5, %mm3

	/*\ d = d + s1 - s2, unsigned saturation, and save \*/
	paddusb %mm1, %mm2
	psubusb %mm3, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	movq %mm1, %mm3
	psubusb %mm6, %mm1
	movq %mm1, %mm0
	pand %mm5, %mm0
	paddusb %mm0, %mm1
	paddusb %mm6, %mm3
	pxor %mm5, %mm3
	paddusb %mm3, %mm3
	pand %mm5, %mm3
	paddusb %mm1, %mm2
	psubusb %mm3, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_reshade_copy_rgba_to_rgba)

PR_(imlib_mmx_reshade_copy_rgb_to_rgba):
	ENTER

	pxor %mm4, %mm4
	LOAD_IMMQ(m0XXX0XXX, %mm5)
	LOAD_IMMQ(m0VVV0VVV, %mm6)
	LOAD_IMMQ(mX000X000, %mm7)
	CLEANUP_IMMQ_LOADS(3)

	subl $4, %esi
	subl $4, %edi

	LOOP_START
	incl %ecx
	jz 2f
1:
	/*\ Load source and destination \*/
	movq (%esi, %ecx, 4), %mm1
	movq (%edi, %ecx, 4), %mm2

	movq %mm1, %mm3
	psubusb %mm6, %mm1
	paddusb %mm1, %mm1
	paddusb %mm6, %mm3
	pxor %mm5, %mm3
	paddusb %mm3, %mm3

	/*\ d = d + s1 - s2, unsigned saturation, and save \*/
	paddusb %mm1, %mm2
	psubusb %mm3, %mm2

	/*\ Make result alpha 0xff \*/
	por %mm7, %mm2
	movq %mm2, (%edi, %ecx, 4)
	
	addl $2, %ecx
	js 1b
	jnz 3f
2:
	movd (%esi), %mm1
	movd (%edi), %mm2
	movq %mm1, %mm3
	psubusb %mm6, %mm1
	paddusb %mm1, %mm1
	paddusb %mm6, %mm3
	pxor %mm5, %mm3
	paddusb %mm3, %mm3
	paddusb %mm1, %mm2
	psubusb %mm3, %mm2
	por %mm7, %mm2
	movd %mm2, (%edi)
3:
	LOOP_END
	LEAVE

SIZE(imlib_mmx_reshade_copy_rgb_to_rgba)

#endif

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif
