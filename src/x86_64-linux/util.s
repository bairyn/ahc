# Utilities.

# Configure the platform.
.code64

.data

ns_util_err_msg_not_writeable_size:
	.quad (ns_util_err_msg_not_writeable_end - ns_util_err_msg_not_writeable)
ns_util_err_msg_not_writeable:
	.ascii "Error:\n"
	.ascii "	AHC machine code cannot change itself!  Verify compilation and system\n"
	.ascii "	configuration for self-modifying code.\n"
	.byte 0x00
ns_util_err_msg_not_writeable_end:

.text

# Can we return?
# 	%rdi: return
#
# Clobbers nothing.
.global ns_ahc_can_cont
ns_ahc_can_cont:
	jmp *%rdi

# Make sure that the code can rewrite itself.
#
# For segv handling, only call this once per process instantion (maybe this
# requirement may be removed with further enhancements in the future).
# 	%rdi: return
#
# (I'm trying to keep dependence on the stack at a minimum, but %rsp is used
# here mainly because it was convenient to have more storage space, so this
# procedure assumes at least that %rsp maintains a valid stack.)
#
# Clobbers TODO.
.global ns_ahc_system_verify_writeable
ns_ahc_system_verify_writeable:
	# Backup return.
	subq $16, %rsp
	movq $0, 8(%rsp)  # Padding.
	movq %rdi, 0(%rsp)

	# First, trap SEGV to print our error message instead just aborting with
	# SEGV.
	leaq 0f(%rip), %rsi
	movq $0, %rdx
	leaq 9f(%rip), %rdi
	jmp ns_system_x86_64_linux_trap_segv
9:
	nop

	# Patch the machine code between local symbols 0 and 1 (see
	# https://ftp.gnu.org/old-gnu/Manuals/gas-2.9.1/html_chapter/as_5.html) to
	# replace the first three bytes in this section with 0xEB XX 0x90, where
	# 0xEB XX is a relative jump by one byte, and 0x90 is a nop.  Jump to the
	# end of this section.
	leaq 0f(%rip), %rdi
	leaq 1f(%rip), %rsi
	subq %rdi, %rsi
	subq $2, %rsi
	movb $0x90, 2(%rdi)  # This may trigger a segfault if there is an error!
	movb %sil, 1(%rdi)
	movb $0xEB, (%rdi)
	nop

	# If this machine code is unchanged, it prints an error message about
	# self-modifiability.
0:
	movq $2, %rdi
	leaq ns_util_err_msg_not_writeable(%rip), %rsi
	leaq ns_util_err_msg_not_writeable_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# Okay, now restore the default SEGV trap.
	leaq 9f(%rip), %rdi
	jmp ns_system_x86_64_linux_restore_trap_segv
9:
	nop

	# (Uncomment this to trigger a segfault to make sure it's actually being
	# restored:)
	#movq $0, %rdi
	#movq (%rdi), %rdi

	# Restore return.
	movq 0(%rsp), %rdi
	# 8(%rsp): padding.
	addq $16, %rsp

	# Return.
	jmp *%rdi
	nop
