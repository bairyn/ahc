# CLI front-end.

# Configure the platform.
.code64
.data
todo:
#.ascii "It starts.\n"  # 16 offset, size 11.
.ascii "It starts (code     ).\n"  # 16 offset, size 23.
.byte 0x00

ns_cli_err_msg_memory_preliminary_checks_size:
	.quad (ns_cli_err_msg_memory_preliminary_checks_end - ns_cli_err_msg_memory_preliminary_checks)
ns_cli_err_msg_memory_preliminary_checks:
	.ascii "cli startup error: prelimary base malloc and mfree checks failed; could not validate we have a working memory management system.\n"
	.byte 0x00
ns_cli_err_msg_memory_preliminary_checks_end:

.text
# A front-end: the CLI.
#
# This doesn't return.
.global ns_cli_cli
ns_cli_cli:
	# Make sure we can return.
	# Load the return address (continuation) into ‘rdi’.
	leaq 9f(%rip), %rdi
	jmp ns_ahc_can_cont
9:
	nop

	# Make sure we can write.
	leaq 9f(%rip), %rdi
	jmp ns_ahc_system_verify_writeable
9:
	nop

	# Preliminarily check we can malloc and free.
	movq $0, %rcx
	movq $0, %rdx
	movq $8388608, %rsi  # Get 1MiB.
	#movq $8388607, %rsi  # Uncomment to a not-divisible-by-8 error.
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_base_malloc_require, %rax
	jmp ns_system_x86_64_linux_module_begin
9:
	nop

	movq %rdi, %rdi  # size, in bits
	movq %rsi, %rsi  # pointer

	# Validate size of the allocation.
	cmpq $8388608, %rdi
	jz 1f
0:
	# Error: prelimiary checks failed!
	movq $2, %rdi
	leaq ns_cli_err_msg_memory_preliminary_checks(%rip), %rsi
	leaq ns_cli_err_msg_memory_preliminary_checks_size(%rip), %rdx
	movq (%rdx), %rdx
	movq $ns_system_x86_64_linux_exit_custom, %rax
	jmp ns_system_x86_64_linux_module_begin
	nop
	hlt
1:

	# Validate we can write and read to and from our allocation.
	movb $0x03, 0(%rsi)        # First byte.
	movb $0xE7, 1(%rsi)        # Second byte.
	movb $0xD9, 1048575(%rsi)  # Last, 1,048,576-th byte.
	#movb $0xD9, 1048576(%rsi)  # Uncomment for what should commonly be a segfault error.

	movb 0(%rsi), %dil
	cmpb $0x03, %dil
	jnz 0b

	movb 1(%rsi), %dil
	cmpb $0xE7, %dil
	jnz 0b

	movb 1048575(%rsi), %dil
	cmpb $0xD9, %dil
	jnz 0b

	# Free.
	movq $0, %r8
	movq $0, %rcx
	movq %rsi, %rdx
	movq $8388608, %rsi  # Free our 1MiB allocation.
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_base_malloc_require, %rax
	jmp ns_system_x86_64_linux_module_begin
9:
	nop

	jmp 0f
1:
	# A child thread will run this briefly.
	#jmp 1b  # Uncomment to instead let the thread join hang forever.
	movq $ns_system_x86_64_linux_exit_success, %rax
	jmp ns_system_x86_64_linux_module_begin
	nop
0:

	# Test we can start and join threads.
	movq $0, %rdx
	leaq 1b(%rip), %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_fork_require, %rax
	jmp ns_system_x86_64_linux_module_begin
9:
	nop

	# Now join.
	leaq 9f(%rip), %rdx
	xchgq %rdi, %rdx
	jmp *%rdx
9:
	nop

	# Make sure we can sleep for a nanosecond.
	movq $1, %rdx  # 1ns
	movq $0, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	jmp ns_system_x86_64_linux_module_begin
9:
	nop

	# Make sure we can sleep for 0 nanoseconds.
	movq $0, %rdx  # 0ns
	movq $0, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	jmp ns_system_x86_64_linux_module_begin
9:
	nop

	# Uncomment the jmp to sleep for 2.5 seconds.
	movq $500000000, %rdx  # 500ms
	movq $2, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	#jmp ns_system_x86_64_linux_module_begin
9:
	nop

	# TODO test ns_system_x86_64_linux_print_u64
	movq $1324, %rcx
	movq $4, %rdx
	leaq todo(%rip), %rsi
	addq $16, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_print_u64, %rax
	jmp ns_system_x86_64_linux_module_begin
9:
	nop

	# TODO: don't bypass system.s; use the layer to handle syscalls.
	movq $1, %rax  # write
	movq $1, %rdi
	leaq todo(%rip), %rsi
	#movq $11, %rdx
	movq $23, %rdx
	syscall

	movq $ns_system_x86_64_linux_exit_success, %rax
	jmp ns_system_x86_64_linux_module_begin
	nop

	jmp ns_cli_cli
	nop
