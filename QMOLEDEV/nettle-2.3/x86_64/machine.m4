C OFFSET(i)
C Expands to 4*i, or to the empty string if i is zero
define(<OFFSET>, <ifelse($1,0,,eval(4*$1))>)

dnl LREG(reg) gives the 8-bit register corresponding to the given 64-bit register.
define(<LREG>,<ifelse(
	$1, %rax, %al,
	$1, %rbx, %bl,
	$1, %rcx, %cl,
	$1, %rdx, %dl,
	$1, %rsi, %sil,
	$1, %rdi, %dil,
	$1, %rbp, %bpl,
	$1, %r8, %r8b,
	$1, %r9, %r9b,
	$1, %r10, %r10b,
	$1, %r11, %r11b,
	$1, %r12, %r12b,
	$1, %r13, %r13b,
	$1, %r14, %r14b,
	$1, %r15, %r15b)>)dnl

define(<HREG>,<ifelse(
	$1, %rax, %ah,
	$1, %rbx, %bh,
	$1, %rcx, %ch,
	$1, %rdx, %dh)>)dnl

define(<XREG>,<ifelse(
	$1, %rax, %eax,
	$1, %rbx, %ebx,
	$1, %rcx, %ecx,
	$1, %rdx, %edx,
	$1, %rsi, %esi,
	$1, %rdi, %edi,
	$1, %rbp, %ebp,
	$1, %r8, %r8d,
	$1, %r9, %r9d,
	$1, %r10, %r10d,
	$1, %r11, %r11d,
	$1, %r12, %r12d,
	$1, %r13, %r13d,
	$1, %r14, %r14d,
	$1, %r15, %r15d)>)dnl
