# This module provides procedures to interface with the kernel through
# syscalls.

# See ‘arch/x86/entry/syscalls/syscall_64.tbl’ (thanks,
# https://unix.stackexchange.com/a/499016).

# Configure the platform.
.code64

# For our compiler, we only require the ability to read and write files and to
# exit.  We can work out the rest.  But we also add some concurrency support
# and shell support.
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
