#include <config.h>
#include "asm.h"

#ifdef DO_MMX_ASM

/*\ 
|*| MMX assembly rotation routine for Imlib2
|*| Written by Willem Monsuwe <willem@stack.nl>
\*/

.text
	.align 8
FN_(imlib_mmx_RotateAA)


/*\ Prototype: __imlib_mmx_RotateAA(DATA32 *src, DATA32 *dest, int sow, int sw,
|*|				int sh, int dow, int dw, int dh, int x, int y,
|*|				int dxh, int dyh, int dxv, int dyv)
\*/

#define src	8(%ebp)
#define dest	12(%ebp)
#define sow	16(%ebp)
#define sw	20(%ebp)
#define sh	24(%ebp)
#define dow	28(%ebp)
#define dw	32(%ebp)
#define dh	36(%ebp)
#define x	40(%ebp)
#define y	44(%ebp)
#define dxh	48(%ebp)
#define dyh	52(%ebp)
#define dxv	56(%ebp)
#define dyv	60(%ebp)

/*\ Local variables \*/
#define j	-4(%ebp)
#define dly	-8(%ebp)
#define dlx	-12(%ebp)
#define sht	-16(%ebp)
#define swt	-20(%ebp)
#define m0fffh	-24(%ebp)
#define m0fff	-28(%ebp)
#define mulsow	-32(%ebp)

PR_(imlib_mmx_RotateAA):
	pushl %ebp
	movl %esp, %ebp
	subl $40, %esp
	pushl %ebx
	pushl %ecx
	pushl %edx
	pushl %edi
	pushl %esi

	/*\ Check (dw > 0) && (dh > 0) \*/
	cmpl $0, dw
	jle .rotate_leave
	cmpl $0, dh
	jle .rotate_leave

	pxor %mm7, %mm7
	movl sow, %eax
	sall $16, %eax
	orl $1, %eax
	movl %eax, mulsow
	movl $0x0fff, %eax
	movl %eax, m0fff
	movl %eax, m0fffh

	/*\ mm6 = x, y \*/
	movq x, %mm6

	/*\ edi = dest + dw \*/
	movl dest, %edi
	movl dw, %eax
	leal (%edi, %eax, 4), %edi

	/*\ dlx = dxv - dw * dxh \*/
	movl dw, %eax
	imull dxh, %eax
	negl %eax
	addl dxv, %eax
	movl %eax, dlx

	/*\ dly = dyv - dw * dyh \*/
	movl dw, %eax
	imull dyh, %eax
	negl %eax
	addl dyv, %eax
	movl %eax, dly

	/*\ j = dh \*/
	movl dh, %eax
	movl %eax, j

	/*\ Check if all coordinates will be inside the source \*/
	/*\ x < sw \*/
	movl sw, %edx
	movl x, %ecx
	cmpl %edx, %ecx
	jae .rotate_outside
	/*\ x + dxh * dw < sw \*/
	movl dxh, %ebx
	imull dw, %ebx
	addl %ebx, %ecx
	cmpl %edx, %ecx
	jae .rotate_outside
	/*\ x + dxh * dw + dxv * dh < sw \*/
	movl dxv, %eax
	imull dh, %eax
	subl %eax, %ecx
	cmpl %edx, %ecx
	jae .rotate_outside
	/*\ x + dxv * dh < sw \*/
	subl %ebx, %ecx
	cmpl %edx, %ecx
	jae .rotate_outside

	/*\ y < sh \*/
	movl sh, %edx
	movl y, %ecx
	cmpl %edx, %ecx
	jae .rotate_outside
	/*\ y + dyh * dw < sh \*/
	movl dyh, %ebx
	imull dw, %ebx
	addl %ebx, %ecx
	cmpl %edx, %ecx
	jae .rotate_outside
	/*\ y + dyh * dw + dyv * dh < sh \*/
	movl dyv, %eax
	imull dh, %eax
	addl %eax, %ecx
	cmpl %edx, %ecx
	jae .rotate_outside
	/*\ y + dyv * dh < sh \*/
	subl %ebx, %ecx
	cmpl %edx, %ecx
	jae .rotate_outside

.rotate_inside:
	movl sow, %ebx
	movl src, %edx
.inside_loop_y:

	/*\ i = -dw \*/
	movl dw, %ecx
	negl %ecx
.inside_loop_x:
	/*\ esi = src + x >> 12 + (y >> 12) * sow \*/
	movq %mm6, %mm0
	psrad $12, %mm0
	packssdw %mm0, %mm0
	pmaddwd mulsow, %mm0
	movd %mm0, %eax
	leal (%edx, %eax, 4), %esi

	/*\ x and y \*/
	movq %mm6, %mm0
	pand m0fff, %mm0
	movq %mm0, %mm1
	/*\ mm0 = x & 0xfff \*/
	punpcklwd %mm0, %mm0
	punpckldq %mm0, %mm0
	/*\ mm1 = y & 0xfff \*/
	punpckhwd %mm1, %mm1
	punpckldq %mm1, %mm1

	/*\ Load and unpack four pixels in parralel
	|*| %mm2 = ptr[0],   %mm3 = ptr[1]
	|*| %mm4 = ptr[sow], %mm5 = ptr[sow + 1]
	\*/
	movq (%esi), %mm2
	movq (%esi, %ebx, 4), %mm4
	movq %mm2, %mm3
	movq %mm4, %mm5
	punpcklbw %mm7, %mm2
	punpcklbw %mm7, %mm4
	punpckhbw %mm7, %mm3
	punpckhbw %mm7, %mm5

	/*\ X interpolation: r = l + (r - l) * xap \*/
	psubw %mm2, %mm3
	psubw %mm4, %mm5
	psllw $4, %mm3
	psllw $4, %mm5
	pmulhw %mm0, %mm3
	pmulhw %mm0, %mm5
	paddw %mm2, %mm3
	paddw %mm4, %mm5

	/*\ Y interpolation: d = u + (d - u) * yap \*/
	psubw %mm3, %mm5
	psllw $4, %mm5
	pmulhw %mm1, %mm5
	paddw %mm3, %mm5
	packuswb %mm5, %mm5
	movd %mm5, (%edi, %ecx, 4)
	
	paddd dxh, %mm6

	incl %ecx
	jnz .inside_loop_x

	paddd dlx, %mm6
	movl dow, %ecx
	leal (%edi, %ecx, 4), %edi
	decl j
	jnz .inside_loop_y

	jmp .rotate_leave

.rotate_outside:
	movl sw, %eax
	decl %eax
	sall $12, %eax
	movl %eax, swt
	movl sh, %eax
	decl %eax
	sall $12, %eax
	movl %eax, sht
	
	movl sow, %ebx
	movl src, %edx
.outside_loop_y:

	/*\ i = -dw \*/
	movl dw, %ecx
	negl %ecx
.outside_loop_x:
	/*\ esi = src + x >> 12 + (y >> 12) * sow \*/
	movq %mm6, %mm0
	psrad $12, %mm0
	packssdw %mm0, %mm0
	pmaddwd mulsow, %mm0
	movd %mm0, %eax
	leal (%edx, %eax, 4), %esi

	/*\ x & 0xfff and y & 0xfff \*/
	movq %mm6, %mm0
	pand m0fff, %mm0
	movq %mm0, %mm1

	/*\ x < swt \*/
	movq %mm6, %mm2
	psrlq $32, %mm2
	movd %mm6, %eax
	cmpl swt, %eax
	jae 2f

	/*\ y < sht \*/
	movd %mm2, %eax
	cmpl sht, %eax
	jae 1f
	/*\ 1234 \*/
.interp_argb:
	/*\ Unpack x and y \*/
	punpcklwd %mm0, %mm0
	punpckldq %mm0, %mm0
	punpckhwd %mm1, %mm1
	punpckldq %mm1, %mm1
	/*\ Load and unpack four pixels in parralel
	|*| %mm2 = ptr[0],   %mm3 = ptr[1]
	|*| %mm4 = ptr[sow], %mm5 = ptr[sow + 1]
	\*/
	movq (%esi), %mm2
	movq (%esi, %ebx, 4), %mm4
	movq %mm2, %mm3
	movq %mm4, %mm5
	punpcklbw %mm7, %mm2
	punpcklbw %mm7, %mm4
	punpckhbw %mm7, %mm3
	punpckhbw %mm7, %mm5

	/*\ X interpolation: r = l + (r - l) * xap \*/
	psubw %mm2, %mm3
	psubw %mm4, %mm5
	psllw $4, %mm3
	psllw $4, %mm5
	pmulhw %mm0, %mm3
	pmulhw %mm0, %mm5
	paddw %mm2, %mm3
	paddw %mm4, %mm5

	/*\ Y interpolation: d = u + (d - u) * yap \*/
	psubw %mm3, %mm5
	psllw $4, %mm5
	pmulhw %mm1, %mm5
	paddw %mm3, %mm5
	packuswb %mm5, %mm5
	movd %mm5, (%edi, %ecx, 4)
	jmp .outside_il_end
1:
	/*\ (-y-1) < 4096 \*/
	notl %eax
	cmpl $4095, %eax
	ja 1f
	/*\ ..34 \*/
	pxor m0fff, %mm1
	movd (%esi, %ebx, 4), %mm2
	movd 4(%esi, %ebx, 4), %mm4

.interp_rgb_a0:
	/*\ Unpack x and y \*/
	punpcklwd %mm0, %mm0
	punpckldq %mm0, %mm0
	punpckhwd %mm1, %mm1
	/*\ Unpack two pixels \*/
	punpcklbw %mm7, %mm2
	punpcklbw %mm7, %mm4
	/*\ Interpolate \*/
	psubw %mm2, %mm4
	psllw $4, %mm4
	pmulhw %mm0, %mm4
	paddw %mm2, %mm4
	/*\ Separate out alpha, multiply with mm1, and subtract \*/
	movq %mm4, %mm2
	psllq $48, %mm1
	psllw $4, %mm4
	pmulhw %mm1, %mm4
	psubw %mm4, %mm2
	packuswb %mm2, %mm2
	movd %mm2, (%edi, %ecx, 4)
	jmp .outside_il_end
1:
	/*\ (y - sht) < 4096 \*/
	notl %eax
	subl sht, %eax
	cmpl $4095, %eax
	ja .outside_il_0
	/*\ 12.. \*/
	movd (%esi), %mm2
	movd 4(%esi), %mm4
	jmp .interp_rgb_a0
2:
	/*\ Switch x and y \*/
	psrlq $32, %mm0
	psllq $32, %mm1
	/*\ -x-1 < 4096 \*/
	notl %eax
	cmpl $4095, %eax
	ja 2f

	pxor m0fff, %mm1
	/*\ y < sht \*/
	movd %mm2, %eax
	cmpl sht, %eax
	jae 1f
	/*\ .2.4 \*/
	movd 4(%esi), %mm2
	movd 4(%esi, %ebx, 4), %mm4
	jmp .interp_rgb_a0
1:
	/*\ (-y-1) < 4096 \*/
	notl %eax
	cmpl $4095, %eax
	ja 1f
	/*\ ...4 \*/
	movd 4(%esi, %ebx, 4), %mm2
.interp_a000:
	/*\ Separate out alpha, multiply with mm0 and mm1 \*/
	pxor m0fff, %mm1
	punpcklbw %mm7, %mm2
	movq %mm2, %mm3
	psllq $2, %mm0
	psrlq $30, %mm1
	pmulhw %mm0, %mm1
	pxor m0fff, %mm1
	psllq $48, %mm1
	psllw $4, %mm3
	pmulhw %mm1, %mm3
	psubw %mm3, %mm2
	packuswb %mm2, %mm2
	movd %mm2, (%edi, %ecx, 4)
	jmp .outside_il_end
1:
	/*\ (y - sht) < 4096 \*/
	notl %eax
	subl sht, %eax
	cmpl $4095, %eax
	ja .outside_il_0
	/*\ .2.. \*/
	pxor m0fff, %mm0
	movd 4(%esi), %mm2
	jmp .interp_a000
2:
	/*\ (x - swt) < 4096 \*/
	notl %eax
	subl swt, %eax
	cmpl $4095, %eax
	ja .outside_il_0

	/*\ y < sht \*/
	movd %mm2, %eax
	cmpl sht, %eax
	jae 1f
	/*\ 1.3. \*/
	movd (%esi), %mm2
	movd (%esi, %ebx, 4), %mm4
	jmp .interp_rgb_a0
1:
	/*\ (-y-1) < 4096 \*/
	notl %eax
	cmpl $4095, %eax
	ja 1f
	/*\ ..3. \*/
	movd (%esi, %ebx, 4), %mm2
	jmp .interp_a000
1:
	/*\ (y - sht) < 4096 \*/
	notl %eax
	subl sht, %eax
	cmpl $4095, %eax
	ja .outside_il_0
	/*\ 1... \*/
	pxor m0fff, %mm0
	movd (%esi), %mm2
	jmp .interp_a000

.outside_il_0:
	movl $0, %eax
	movl %eax, (%edi, %ecx, 4)
	
.outside_il_end:
	paddd dxh, %mm6

	incl %ecx
	jnz .outside_loop_x

	paddd dlx, %mm6
	movl dow, %ecx
	leal (%edi, %ecx, 4), %edi
	decl j
	jnz .outside_loop_y

.rotate_leave:
	emms
	popl %esi
	popl %edi
	popl %edx
	popl %ecx
	popl %ebx
	movl %ebp, %esp
	popl %ebp
	ret

SIZE(imlib_mmx_RotateAA)

#endif

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif
