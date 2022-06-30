# This module provides procedures to interface with the kernel through
# syscalls.

# See ‘arch/x86/entry/syscalls/syscall_64.tbl’ (thanks,
# https://unix.stackexchange.com/a/499016).

# See also the Intel(R) 64 and IA-32 Architectures Software Developer's Manual,
# Combined Volumes: 1, 2A, 2B, 2C, 2D, 3A, 3B, 3C, 3D, and 4 PDF document.

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

ns_system_x86_64_linux_err_msg_segv_trap_set_size:
	.quad (ns_system_x86_64_linux_err_msg_segv_trap_set_end - ns_system_x86_64_linux_err_msg_segv_trap_set)
ns_system_x86_64_linux_err_msg_segv_trap_set:
	.ascii "segv trap set error: rt_sigaction failed!  Could not trap SIGSEGV.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_segv_trap_set_end:

ns_system_x86_64_linux_err_msg_segv_trap_restore_size:
	.quad (ns_system_x86_64_linux_err_msg_segv_trap_restore_end - ns_system_x86_64_linux_err_msg_segv_trap_restore)
ns_system_x86_64_linux_err_msg_segv_trap_restore:
	.ascii "segv trap set error: rt_sigaction failed!  Could not restore SIGSEGV handler.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_segv_trap_restore_end:

# Mutate at most once, static only for ns_system_x86_64_linux_verify_errno, to
# be set before quitting with an error.
ns_system_x86_64_linux_syscall_verify_builder_size:
	.quad (ns_system_x86_64_linux_syscall_verify_builder_end - ns_system_x86_64_linux_syscall_verify_builder)
ns_system_x86_64_linux_syscall_verify_builder:
	.rept 1024
	.byte 0x00
	.endr
ns_system_x86_64_linux_syscall_verify_builder_end:

# Utility: digits
ns_system_x86_64_linux_util_digits:
.ascii "0123456789ABCDEF"
.byte 0x00

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

# Simple utility to verify a syscall was successful or else fail with an error
# message.
# 	%rdi: return
# 	%rsi: the errno to check (syscall return %rax value, actually)
# 	%rdx: an error message to print after our prefix of ‘Error (errno %d): ’
# 	%rcx: size of the error message
ns_system_x86_64_linux_verify_errno:
	# Return normally if the syscall return value is not in [-4095, -1] (https://stackoverflow.com/a/2538212).
	cmpq $-4095, %rsi  # if %rsi < -4095
	jl   0f            # return

	cmpq $-1, %rsi  # if %rsi > -1
	jg   0f         # return

	# We have an error.  Prepare the message.
	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rdi
# 	%rdx: an error message to print after our prefix of ‘Error (errno %d): ’
	movb $'E, 0(%rdi)
	movb $'r, 1(%rdi)
	movb $'r, 2(%rdi)
	movb $'o, 3(%rdi)
	movb $'r, 4(%rdi)
	movb $' , 5(%rdi)
	movb $'(, 6(%rdi)
	movb $'e, 7(%rdi)
	movb $'r, 8(%rdi)
	movb $'r, 9(%rdi)
	movb $'n, 10(%rdi)
	movb $'o, 11(%rdi)
	movb $' , 12(%rdi)
	movb $' , 13(%rdi)
	movb $' , 14(%rdi)
	movb $' , 15(%rdi)
	movb $' , 16(%rdi)
	movb $'), 17(%rdi)
	movb $':, 18(%rdi)
	movb $' , 19(%rdi)

	movq %rdx, %r8

	# If %rcx > ns_system_x86_64_linux_syscall_verify_builder_size + 20, %rcx = … + 20.
	leaq ns_system_x86_64_linux_syscall_verify_builder_size(%rip), %rdx
	movq (%rdx), %rdx
	addq $20, %rdx
	cmpq %rdx, %rcx
	jna 9f
	movq %rdx, %rcx  # %rcx <- …_size + 20
9:

	# Append the error message.
	addq $20, %rdi
8:
	# Break if size reaches 0.# or we reach a null byte.
	testq %rcx, %rcx
	jz 7f
	#movq (%rdi), %rdx
	#testq %rdx, %rdx
	#jz 7f

	movb (%r8), %r9b
	movb %r9b, (%rdi)
	dec %rcx
	inc %rdi
	inc %r8

	jmp 8b
7:

	# Encode the error code (%rsi) into offsets 13 through 17 of %rdi.
	not %rsi
	inc %rsi  # %rsi = -%rsi

	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rdi
	addq $13, %rdi  # Base.
	movq $4, %rcx  # Size.
	movq %rsi, %r8  # Current dividend.
6:
	# Break if out of space.
	testq %rcx, %rcx
	jz 5f
	dec %rcx

	# Divide by 10 to get the least significant digit.
	movq $0, %rdx
	movq %r8, %rax
	movq $10, %r9
	divq %r9
	# mod (remainder) is %rdx, quotient is %rax.
	# Use %rdx to write another digit, starting from the right.
	addq $'0, %rdx
	addq %rcx, %rdi
	movb %dl, (%rdi)
	subq %rcx, %rdi
	# The next dividend will be %rax.
	movq %rax, %r8

	# Loop back unless %r8 is 0.
	testq %r8, %r8
	jnz 6b
5:

	# Hand over control to exit_custom().
	movq $3, %rdi
	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rsi
	leaq ns_system_x86_64_linux_syscall_verify_builder_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp ns_system_x86_64_linux_exit_custom

0:
	# Return normally.
	jmp *%rdi

# An optional segv trap handler that hands off execution to what was statically
# set in ‘ns_system_x86_64_linux_sigaction_static_set_segv_cont’ and
# ‘ns_system_x86_64_linux_sigaction_static_set_segv_data’
ns_system_x86_64_linux_trap_middleman:
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_data(%rip), %rdi
	movq (%rdi), %rdi
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_cont(%rip), %rsi
	movq (%rsi), %rsi
	jmp *%rsi

	# Redundantly after a jump (never executed), adapt to the signal handler's
	# expectation of a conventional return.
	movq (%rsp), %rdi
	addq $8, %rsp
	jmp *%rdi

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
	movq $0, %r10  # sizeof sigset_t: 128, but we'll just use 0.
	movq $13, %rax  # rt_sigaction
	syscall

	# Verify.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_segv_trap_set(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_segv_trap_set_size(%rip), %rcx
	movq (%rcx), %rcx
	lea 5(%rip), %rdi
	jmp ns_system_x86_64_linux_verify_errno
	nop

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
	movq $0, %r10  # sizeof sigset_t: 128, but we'll just use 0.
	movq $13, %rax  # rt_sigaction
	syscall

	# Verify.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_segv_trap_restore(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_segv_trap_restore_size(%rip), %rcx
	movq (%rcx), %rcx
	lea 5(%rip), %rdi
	jmp ns_system_x86_64_linux_verify_errno
	nop

	# Return.
	movq %r8, %rdi
	jmp *%rdi
	nop
