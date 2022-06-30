# Utilities.

# Configure the platform.
.code64

.data

ns_util_err_msg_not_writeable_size:
	.quad (ns_util_err_msg_not_writeable_end - ns_util_err_msg_not_writeable)
ns_util_err_msg_not_writeable:
.ascii "Error: AHC machine code cannot change itself!  Verify compilation and system configuration for self-modifying code.\n"
.byte 0x00
ns_util_err_msg_not_writeable_end:

.text

# Can we return?
.global ns_ahc_can_cont
ns_ahc_can_cont:
	jmp %rdi

# Make sure that the code can rewrite itself.
.global ns_ahc_system_verify_writeable
ns_ahc_system_verify_writeable:
	# TODO
	movq $2, %rdi
	leaq ns_util_err_msg_not_writeable(%rip), %rsi
	leaq ns_util_err_msg_not_writeable_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp ns_system_x86_64_linux_exit_custom

	jmp %rdi
