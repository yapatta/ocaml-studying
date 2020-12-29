	.data
OUT:
	.string "%lld\n"
	.text
	.global _main
// ライブラリで用意された値は_
_main:
	pushq %rbp
	movq %rsp, %rbp
	movq $3, %rax
	movq $5, %rbx
	movq $1, %rcx
L1:
	cmpq $0, %rax
	je L2
	imulq %rbx, %rcx
	subq $1, %rax
	jmp L1
L2:
	leaq OUT(%rip), %rdi
	movq %rcx, %rsi
	movq $1, %rax // $0になっていたけど$1じゃない?
	callq _printf
	movq $0, %rax
	leaveq
	retq
