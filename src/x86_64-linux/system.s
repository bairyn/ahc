# This module provides procedures to interface with the kernel through
# syscalls.

# See ‘arch/x86/entry/syscalls/syscall_64.tbl’ (thanks,
# https://unix.stackexchange.com/a/499016).

# Configure the platform.
.code64

.data

# 152 bytes of data to restore segv trap.
ns_system_x86_64_linux_sigaction_restore_segv:
	.quad 0x0  # SIG_DFL = 0
	.rept (152-8)
	.byte 0x00
	.endr
# 152 bytes of data (per-process-instantation) to set segv trap.
ns_system_x86_64_linux_sigaction_static_set_segv:
	.quad 0x0  # SIG_DFL = 0
	.rept (152-8)
	.byte 0x00
	.endr
# (per-process-instantation) to set segv trap.
ns_system_x86_64_linux_sigaction_static_set_segv_cont:
	.quad 0x0
# (per-process-instantation) to set segv trap.
ns_system_x86_64_linux_sigaction_static_set_segv_data:
	.quad 0x0

# For our compiler, we only require the ability to read and write files and to
# exit.  We can work out the rest.  But we also add some concurrency support
# and shell support.  Also add a few other utilities as needed.
.text

# Write to a file.  The FRP time context is an implicit parameter.
# 	%rdi: AHC runtime environment pointer.
# 	%rsi: return
# 	%rdx: pointer to filepath
# 	%rcx: size of filepath
# 	%r8:  pointer to data to write
# 	%r9:  size of data to write
#
# 	Callback called with:
# 	%rdi: Type of write result (success or failure; 0: failure; 1: success)
# 	%TODO: bytes blah
# 	TODO: futures?
# 	TODO
.global ns_system_x86_64_linux_write
ns_system_x86_64_linux_write:
	# TODO
	nop
	jmp ns_system_x86_64_linux_exit_failure

.global ns_system_x86_64_linux_exit_success
ns_system_x86_64_linux_exit_success:
	movq $60, %rax  # exit
	movq $0, %rdi
	syscall

	jmp ns_system_x86_64_linux_exit_success
	nop

.global ns_system_x86_64_linux_exit_failure
ns_system_x86_64_linux_exit_failure:
	movq $60, %rax  # exit
	movq $1, %rdi
	syscall

	jmp ns_system_x86_64_linux_exit_failure
	nop

# Exit with a code and error message.
# 	%rdi: Error code.
# 	%rsi: Error message.
# 	%rdx: Error message size.
#
# 	(Note: this brings into play Linux's blocking mechanisms.)
.global ns_system_x86_64_linux_exit_custom
ns_system_x86_64_linux_exit_custom:
	movq %rdi, %r10

	movq %rdx, %rdx
	movq %rsi, %rsi
	movq $2, %rdi
	movq $1, %rax  # write
	syscall

	movq %r10, %rdi

	movq $60, %rax  # exit
	movq %rdi, %rdi
	syscall

	jmp ns_system_x86_64_linux_exit_custom
	nop

# An optional segv trap handler that hands off execution to what was statically
# set in ‘ns_system_x86_64_linux_sigaction_static_set_segv_cont’ and
# ‘ns_system_x86_64_linux_sigaction_static_set_segv_data’
ns_system_x86_64_linux_trap_middleman:
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_data(%rip), %rdi
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_cont(%rip), %rsi
	jmp *%rsi

# Call a callback on segfault.  Only use this once per process instantiation.
#
# Note this is called before self-modifying-code support is verified.
# 	%rdi: return
# 	%rsi: callback continuation, where to jump.
# 	%rdx: callback pointer / user data.
.global ns_system_x86_64_linux_trap_segv
ns_system_x86_64_linux_trap_segv:
	# Backup the continuation.
	movq %rdi, %r8

	# Set the first 8 bytes in our static data.
	#leaq ns_system_x86_64_linux_sigaction_static_set_segv(%rip), %rdi
	#ns_system_x86_64_linux_sigaction_static_set_segv
	# Use a wrapper layer to track user data.
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_cont(%rip), %rdi
	movq %rsi, (%rdi)
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_data(%rip), %rdi
	movq %rdx, (%rdi)
	leaq ns_system_x86_64_linux_trap_middleman(%rip), %rdi

	# rt_sigaction(…)
	movq $11, %rdi  # signum for SEGV is 11.
	leaq ns_system_x86_64_linux_sigaction_static_set_segv(%rip), %rsi
	movq $0, %rdx
	movq $0, %r10
	movq $13, %rax  # rt_sigaction
	syscall

	# Return.
	movq %r8, %rdi
	jmp *%rdi
	nop

# Restore default segfault trap behaviour, but override any previous
# non-default setting with the default handler.
# 	%rdi: return
.global ns_system_x86_64_linux_restore_trap_segv
ns_system_x86_64_linux_restore_trap_segv:
	# Backup the continuation.
	movq %rdi, %r8

	# rt_sigaction(…)
	movq $11, %rdi  # signum for SEGV is 11.
	leaq ns_system_x86_64_linux_sigaction_restore_segv(%rip), %rsi
	movq $0, %rdx
	movq $0, %r10
	movq $13, %rax  # rt_sigaction
	syscall

	# Return.
	movq %r8, %rdi
	jmp *%rdi
	nop
