	.data
OUT:
	.string "%lld\n"
	.text
	.global _main
// ライブラリで用意された値は_
_main:
	pushq %rbp
	movq %rsp, %rbp
	movq $5, %rax
	movq $1, %rbx
L1:
	cmpq $0, %rax
	je L2
	imulq %rax, %rbx
	subq $1, %rax
	jmp L1
L2:
	leaq OUT(%rip), %rdi
	movq %rbx, %rsi
	movq $1, %rax // $0になっていたけど$1じゃない?
	callq _printf
	movq $0, %rax
	leaveq
	retq
