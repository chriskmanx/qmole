C nettle, low-level cryptographics library
C 
C Copyright (C) 2004, Niels Möller
C  
C The nettle library is free software; you can redistribute it and/or modify
C it under the terms of the GNU Lesser General Public License as published by
C the Free Software Foundation; either version 2.1 of the License, or (at your
C option) any later version.
C 
C The nettle library is distributed in the hope that it will be useful, but
C WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
C or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
C License for more details.
C 
C You should have received a copy of the GNU Lesser General Public License
C along with the nettle library; see the file COPYING.LIB.  If not, write to
C the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
C MA 02111-1307, USA.

define(<CTX>, <%rdi>)
define(<N>, <%rsi>)
define(<DST>, <%rdx>)
define(<SRC>, <%rcx>)

define(<I>, <%rax>)
define(<J>, <%r8>)
define(<SI>, <%r9>)
define(<SJ>, <%r10>)

	.file "arcfour-crypt.asm"

	C arcfour_crypt(struct arcfour_ctx *ctx,
	C               unsigned length, uint8_t *dst,
	C               const uint8_t *src)
	.text
	ALIGN(4)
PROLOGUE(nettle_arcfour_crypt)
	C Offset pointers at end of areas
	lea	(SRC, N), SRC
	lea	(DST, N), DST
	neg	N
	jnc	.Ldone
	
	movzbl	256(CTX), XREG(I)
	movzbl	257(CTX), XREG(J)

	ALIGN(4)
.Loop:
	add	$1, XREG(I)
	movzbl	LREG(I), XREG(I)
	movzbl	(CTX, I), XREG(SI)
	add	XREG(SI), XREG(J)
	movzbl	LREG(J), XREG(J)
	movzbl	(CTX, J), XREG(SJ)
	mov	LREG(SI), (CTX, J)
	mov	LREG(SJ), (CTX, I)
	add	XREG(SI), XREG(SJ)
	movzbl	LREG(SJ), XREG(SJ)
	mov	(CTX, SJ), LREG(SI)
	xor	(SRC, N), LREG(SI)
	mov	LREG(SI), (DST, N)
	add	$1, N
	jnz	.Loop
	
	movb	LREG(I), 256(CTX)
	movb	LREG(J), 257(CTX)
.Ldone:
	ret
EPILOGUE(nettle_arcfour_crypt)
