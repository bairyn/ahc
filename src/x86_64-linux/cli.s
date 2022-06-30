# CLI front-end.

# Configure the platform.
.code64
.data
todo:
.ascii "It starts.\n"
.byte 0x00

.text
.global ns_cli_cli
ns_cli_cli:
	# Make sure we can return.
	# Load the return address (continuation) into ‘rdi’.
	lea 5(%rip), %rdi
	jmp ns_ahc_can_cont
	nop

	# Make sure we can write.
	lea 5(%rip), %rdi
	jmp ns_ahc_system_verify_writeable
	nop

	# TODO: don't bypass system.s; use the layer to handle syscalls.
	movq $1, %rax  # write
	movq $1, %rdi
	leaq todo(%rip), %rsi
	movq $11, %rdx
	syscall

	jmp ns_system_x86_64_linux_exit_success
	nop

	jmp ns_cli_cli
	nop
