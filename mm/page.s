/*
 *  linux/mm/page.s
 *
 *  (C) 1991  Linus Torvalds
 */

/*
 * page.s contains the low-level page-exception code.
 * the real work is done in mm.c
 */

.globl _page_fault

/*
_page_fault中断堆栈示意图
High Addr
--------------
%ss
%esp
%eflags
%cs
%eip  <-----------------------------|
error_code							|
%ecx								|
%edx								|
%ds									|
%es									|
%fs									|
%cr2								|
%eax  # error code (function参数2)	|
--------------
Low Addr */

_page_fault:
	xchgl %eax,(%esp)  # %eax <-> error_code	
	pushl %ecx
	pushl %edx
	push %ds
	push %es
	push %fs
	movl $0x10,%edx
	mov %dx,%ds
	mov %dx,%es
	mov %dx,%fs
	movl %cr2,%edx
	pushl %edx
	pushl %eax
	testl $1,%eax 	 # %eax.bit1 = 1 -> 页保护异常 page2790
	jne 1f
	call _do_no_page #缺页异常
	jmp 2f
1:	call _do_wp_page #页保护异常
2:	addl $8,%esp
	pop %fs
	pop %es
	pop %ds
	popl %edx
	popl %ecx
	popl %eax
	iret
