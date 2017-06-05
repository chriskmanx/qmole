#include <config.h>
#include "asm.h"

#ifdef DO_MMX_ASM

/*\ 
|*| MMX assembly rgba rendering routines for Imlib2
|*| Written by Willem Monsuwe <willem@stack.nl>
|*|
|*| Special (hairy) constructs are only commented on first use.
\*/

/*\ All functions have the same calling convention:
|*|  __imlib_mmx_rgbXXX(void *src, int sjmp, void *dst, int dw,
|*|			int w, int h, int dx, int dy)
\*/

#define src	8(%ebp)
#define sjmp	12(%ebp)
#define dst	16(%ebp)
#define dw	20(%ebp)
#define w	24(%ebp)
#define h	28(%ebp)
#define dx	32(%ebp)
#define dy	36(%ebp)

.text
	.align 8
FN_(imlib_mmx_rgb565_fast)
FN_(imlib_mmx_bgr565_fast)
FN_(imlib_mmx_rgb555_fast)
FN_(imlib_mmx_bgr555_fast)

FN_(imlib_get_cpuid)

#include "asm_loadimmq.S"

/*\ Common code \*/
/*\ Save registers, load common parameters \*/
#define ENTER			\
	pushl %ebp;		\
	movl  %esp, %ebp;	\
	pushl %ebx;		\
	pushl %ecx;		\
	pushl %edx;		\
	pushl %edi;		\
	pushl %esi;		\
	movl  src,  %esi;	\
	movl  dst,  %edi;	\
	movl  w,    %ebx;	\
	movl  h,    %edx;	\
	addl %ebx, sjmp

#define LOOP_START		\
	testl %edx, %edx;	\
	jz 4f;			\
	testl %ebx, %ebx;	\
	jz 4f;			\
0:				\
	movl %ebx, %ecx

#define LOOP_END			\
3:					\
	movl sjmp, %ecx;		\
	leal (%esi, %ecx, 4), %esi;	\
	addl dw, %edi;			\
	decl %edx;			\
	jnz 0b;				\
4:

/*\ Unset MMX mode, reset registers, return \*/
#define LEAVE			\
	emms;			\
	popl %esi;		\
	popl %edi;		\
	popl %edx;		\
	popl %ecx;		\
	popl %ebx;		\
	movl %ebp, %esp;	\
	popl %ebp;		\
	ret



PR_(imlib_mmx_bgr565_fast):
	LOAD_IMMQ(mul_bgr565, %mm7)	/*\ This constant is the only difference \*/
	CLEANUP_IMMQ_LOADS(1)
	jmp .rgb565_fast_entry

SIZE(imlib_mmx_bgr565_fast)

PR_(imlib_mmx_rgb565_fast):
	LOAD_IMMQ(mul_rgb565, %mm7)
	CLEANUP_IMMQ_LOADS(1)
.rgb565_fast_entry:
	ENTER

	LOAD_IMMQ(m_rb, %mm5)
	LOAD_IMMQ(m_g6, %mm6)
	CLEANUP_IMMQ_LOADS(2)

	LOOP_START

	test $1, %ecx
	jz 1f
	decl %ecx
	movd (%esi, %ecx, 4), %mm0
	movq %mm0, %mm1
	pand %mm5, %mm0
	pand %mm6, %mm1
	pmaddwd %mm7, %mm0
	por %mm1, %mm0
	psrad $5, %mm0

	movd %mm0, %eax
	movw %ax, (%edi, %ecx, 2)

	jz 3f
1:
	test $2, %ecx
	jz 2f
	subl $2, %ecx
	movq (%esi, %ecx, 4), %mm0
	movq %mm0, %mm1
	pand %mm5, %mm0
	pand %mm6, %mm1
	pmaddwd %mm7, %mm0
	por %mm1, %mm0
	pslld $11, %mm0
	psrad $16, %mm0

	packssdw %mm0, %mm0

	movd %mm0, (%edi, %ecx, 2)

	jz 3f
2:
	subl $4, %ecx
	movq (%esi, %ecx, 4), %mm0
	movq 8(%esi, %ecx, 4), %mm2
	movq %mm0, %mm1		/*\ a r g b (2x) \*/
	movq %mm2, %mm3
	pand %mm5, %mm0		/*\ 0 rrrrr000 0 bbbbb000 (2 x) \*/
	pand %mm5, %mm2
	pand %mm6, %mm1		/*\ 0 0 gggggg00 00000000 (2 x)	\*/
	pand %mm6, %mm3
	pmaddwd %mm7, %mm0	/*\ 0 000rrrrr 000000bb bbb00000 (2 x) \*/
	pmaddwd %mm7, %mm2
	por %mm1, %mm0		/*\ 0 000rrrrr ggggggbb bbb00000 (2 x) \*/
	por %mm3, %mm2
	pslld $11, %mm0		/*\ rrrrrggg gggbbbbb 0 0 (2 x) \*/
	pslld $11, %mm2
	psrad $16, %mm0		/*\ x x rrrrrggg gggbbbbb (2 x) \*/
	psrad $16, %mm2

	packssdw %mm2, %mm0	/*\ rrrrrggg gggbbbbb (4 x) \*/

	movq %mm0, (%edi, %ecx, 2)

	jnz 2b
	LOOP_END
	LEAVE

SIZE(imlib_mmx_rgb565_fast)


PR_(imlib_mmx_bgr555_fast):
	LOAD_IMMQ(mul_bgr555, %mm7)	/*\ This constant is the only difference \*/
	CLEANUP_IMMQ_LOADS(1)
	jmp .rgb555_fast_entry

SIZE(imlib_mmx_bgr555_fast)

PR_(imlib_mmx_rgb555_fast):
	LOAD_IMMQ(mul_rgb555, %mm7)
	CLEANUP_IMMQ_LOADS(1)
.rgb555_fast_entry:
	ENTER

	LOAD_IMMQ(m_rb, %mm5)
	LOAD_IMMQ(m_g5, %mm6)
	CLEANUP_IMMQ_LOADS(2)

	LOOP_START

	test $1, %ecx
	jz 1f
	decl %ecx
	movd (%esi, %ecx, 4), %mm0
	movq %mm0, %mm1
	pand %mm5, %mm0
	pand %mm6, %mm1
	pmaddwd %mm7, %mm0
	por %mm1, %mm0
	psrad $5, %mm0

	movd %mm0, %eax
	movw %ax, (%edi, %ecx, 2)

	jz 3f
1:
	test $2, %ecx
	jz 2f
	subl $2, %ecx
	movq (%esi, %ecx, 4), %mm0
	movq %mm0, %mm1
	pand %mm5, %mm0
	pand %mm6, %mm1
	pmaddwd %mm7, %mm0
	por %mm1, %mm0
	psrld $6, %mm0

	packssdw %mm0, %mm0

	movd %mm0, (%edi, %ecx, 2)

	jz 3f
2:
	subl $4, %ecx
	movq (%esi, %ecx, 4), %mm0
	movq 8(%esi, %ecx, 4), %mm2
	movq %mm0, %mm1		/*\ a r g b (2x) \*/
	movq %mm2, %mm3
	pand %mm5, %mm0		/*\ 0 rrrrr000 0 bbbbb000 (2 x) \*/
	pand %mm5, %mm2
	pand %mm6, %mm1		/*\ 0 0 ggggg000 00000000 (2 x)	\*/
	pand %mm6, %mm3
	pmaddwd %mm7, %mm0	/*\ 0 000rrrrr 00000bbb bb000000 (2 x) \*/
	pmaddwd %mm7, %mm2
	por %mm1, %mm0		/*\ 0 000rrrrr gggggbbb bb000000 (2 x) \*/
	por %mm3, %mm2
	psrld $6, %mm0		/*\ 0 0 0rrrrrgg gggbbbbb (2 x) \*/
	psrld $6, %mm2

	packssdw %mm2, %mm0	/*\ 0rrrrrgg gggbbbbb (4 x) \*/

	movq %mm0, (%edi, %ecx, 2)

	jnz 2b
	LOOP_END
	LEAVE

SIZE(imlib_mmx_rgb555_fast)

PR_(imlib_get_cpuid):
	pushl %ebx
	pushl %edx

	pushf
	popl %eax
	movl %eax, %ebx
	xorl $0x200000, %eax
	pushl %eax
	popf
	pushf
	popl %eax
	xorl %ebx, %eax
	andl $0x200000, %eax
	jz 1f
	xorl %eax, %eax
	cpuid
	testl %eax, %eax
	jz 1f
	movl $1, %eax
	cpuid
	and $0x00000f00, %eax
	and $0xfffff0ff, %edx
	orl %edx, %eax
1:
	popl %edx
	popl %ebx
	ret

SIZE(imlib_get_cpuid)

#endif

#ifdef __ELF__
.section .note.GNU-stack,"",@progbits
#endif
