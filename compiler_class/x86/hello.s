// コード領域
	.text
	.globl _main // 他のファイルから参照できるようにglobalに
// global globl 何が違うんだ?
_main:
	pushq %rbp
	movq %rsp, %rbp
	leaq L1(%rip), %rdi
	movq $0, %rax
	callq _printf
	movq $0, %rax
	leaveq
	retq
// データ領域
	.data
L1:	.string "Hello World!\n"
