# CLI front-end.

# Configure the platform.
.code64

# Module implicit parameters:
#
# Unless otherwise specified, the following implicit parameters are required by
# every procedure in this module:
# 	%r15:
# 		Call mode and options working storage unit.
# 		The first 7 (most significant, BE) bytes must be
# 		(‘printf "0x%02x\n" =<< randomRIO (0x00, 0xFF)’):
# 			0xF4 0x0D 0xCA 0x88  0x48 0x2D 0x41 (0xXX)
#
# 		The final (least significant) byte is an options bitfield.  Unspecified
# 		bits must be 0.
#
# 		Note this is conventionally a caller-saved register, so it should be
# 		preserved between module action calls and conventional calls.
# 	%r14:
# 		Exception return continuation, if overriding default.
#
# 		Like a left-branch handler connection for ‘Either’, specify an
# 		alternative continuation to return to rather than ‘%rdi’ for the typical
# 		pattern of a procedure action returning to its first address as a
# 		continuation, optionally with 0 or more arguments.  Most errors, e.g.
# 		syscalls that return errno's (this can be useful for e.g. specially
# 		handling certain ) will, rather than calling ‘exit_custom’, call this
# 		overriding handler.  For errors that aren't special fatal errors that
# 		cannot be caught, this is analagous to a catch block.
#
# 		If the exception return continuation bit is enabled (bit 1,
# 		second-most-significant bit, in the last byet in $r15), then %r14 is
# 		used instead of the default exception return handler.  If this bit is
# 		disabled, then %r14 is ignored for this purpose.
#
# 		If working storage is available, then a pattern may be for procedures
# 		that require cleanup when unwinding, for these procedures to store the
# 		old %r14 and set their own %r14 as a layer on top, that cleans up and
# 		then restores the old %r14.
#
# 		Note this is conventionally a caller-saved register, so it should be
# 		preserved between module action calls and conventional calls.
#
# 		Unless otherwise specified, this takes the following parameters:
# 			%rdi: Numeric code.
# 			%rsi: String size.
# 			%rdx: String.
# 			%rcx: 0.

# (See note in ‘system’ for having a module router instead of a separate
# ‘.data’ section.)
#.data
.text

.global ns_cli_module_begin  # This jumps to ‘route’ too if you call it.
ns_cli_module_begin:
	.byte 0xEB, 0x36  # skip 54 bytes (jump) if execution begins here.
	.byte 0x90, 0xF4, 0x00, 0x0E  # Pad to 8 bytes, and 0x0E is like a version.
	.byte 0x00, 0x00
.global ns_cli_module_size
.set ns_cli_module_size, (ns_cli_module_end - ns_cli_module_begin)
.global ns_cli_module_size_field  # Should be at offset 8 relative to ‘begin’!
ns_cli_module_size_field:
	.quad (ns_cli_module_end - ns_cli_module_begin)  # Size.
	.quad 0x1324ABBC  # ABI.
	.quad 0  # Module hash, sha256sum of module string with a NULL hash field and NULL external module references.  TODO; just 0 until implemented.
	.quad (_module_name_end - _module_name)  # Size of the module name string.
	.quad (_module_name - ns_cli_module_begin)  # Module-relative offset to the siz of the module name string.
	.quad 0  # Header terminating null.
# Indirectly call an action in this module.
#
# Parameters:
# 	%rax:
# 		The offset relative to ‘begin’ of the module action to call, e.g. with
# 			movq $ns_system_x86_64_linux_fork_require, %rax
# 		Using %rdi instead of %rax followed by remaining arguments would have
# 		been more consistent, but this helps avoid shuffling around the order
# 		of the arguments with a rotation.  So we're just using a different
# 		convention from our own for convenience.
#
# Clobbers %r11 (plus anything the module action clobbers).
#
# This implementation is simple enough that you could probably reproduce it on
# the caller's side.  However, this pattern might be extended to provide
# arguments to other modules (other modules' ‘module_begin’s) so that we may
# perform our own run-time linking.
#
# However, the ‘system’ module itself does not depend on other modules.
#
# Note: other modules can then access this module through the ‘module_begin’
# (that is, ‘ns_system_x86_64_linux_module_route’) symbol.  For portability
# (e.g. maybe you want to copy a module instance somewhere else), it helps to
# only refer to ‘module_begin’ for other modules in one place, so that when
# re-linking things, you only have one spot to update.
ns_cli_module_route:  # Must be at offset 54.
	leaq ns_cli_module_begin(%rip), %r11
	addq %r11, %rax
	jmpq *%rax
	nop

# Module dependencies.  Run-time re-linking and relocation will need to handle
# this.
_system:
	jmp ns_system_x86_64_linux_module_begin
	nop
	hlt
_util:
	jmp ns_util_module_begin
	nop
	hlt

_mod_dep_end:
.quad 0x12342345
.quad 0

# Now the .data stuffs.

_module_name:
	.ascii "cli"
_module_name_end:

ns_cli_err_msg_memory_preliminary_checks_size:
	.quad (ns_cli_err_msg_memory_preliminary_checks_end - ns_cli_err_msg_memory_preliminary_checks)
ns_cli_err_msg_memory_preliminary_checks:
	.ascii "Error: cli startup error: prelimary base malloc and mfree checks failed; could not validate we have a working memory management system.\n"
	.byte 0x00
ns_cli_err_msg_memory_preliminary_checks_end:

ns_cli_path_dev_stdout_size:
	.quad (ns_cli_path_dev_stdout_end - ns_cli_path_dev_stdout)
ns_cli_path_dev_stdout:
	.ascii "/dev/stdout"
	.byte 0x00
ns_cli_path_dev_stdout_end:

ns_cli_path_dev_stdin_size:
	.quad (ns_cli_path_dev_stdin_end - ns_cli_path_dev_stdin)
ns_cli_path_dev_stdin:
	.ascii "/dev/stdin"
	.byte 0x00
ns_cli_path_dev_stdin_end:

ns_cli_path_proc_self_cmdline_size:
	.quad (ns_cli_path_proc_self_cmdline_end - ns_cli_path_proc_self_cmdline)
ns_cli_path_proc_self_cmdline:
	.ascii "/proc/self/cmdline"
	.byte 0x00
ns_cli_path_proc_self_cmdline_end:

ns_cli_msg_starts_size:
	.quad (ns_cli_msg_starts_end - ns_cli_msg_starts)  # 23
ns_cli_msg_starts_u_offset:
	.quad 16
ns_cli_msg_starts_u_size:
	.quad 4
ns_cli_msg_starts_s_offset:
	.quad 38
ns_cli_msg_starts_s_size:
	.quad 12
ns_cli_msg_starts:
	.ascii "It starts (code     ) (cmdline starts             ).\n"
	.byte 0x00
ns_cli_msg_starts_end:

ns_cli_date_command_size:
	.quad (ns_cli_date_command_end - ns_cli_date_command)
ns_cli_date_command:
	.ascii "sh\x00"
	.ascii "-c\x00"
	.ascii "date +'%Y-%M-%d_%H:%M:%S'\x00"
	#.byte 0x00
ns_cli_date_command_end:

.text
# A front-end: the CLI.
#
# This doesn't return.
#
# Parameters:
# 	(None.)
#
# We use the stack here just to track cleanup requirements.
.global ns_cli_cli
.set ns_cli_cli, (_ns_cli_cli - ns_cli_module_begin)
.global ns_cli_cli_with_prelim_checks
.set ns_cli_cli_with_prelim_checks, (_ns_cli_cli_with_prelim_checks - ns_cli_module_begin)
_ns_cli_cli:
_ns_cli_cli_with_prelim_checks:
	# Make sure we can return.
	# Load the return address (continuation) into ‘rdi’.
	leaq 9f(%rip), %rdi
	movq $ns_util_can_cont, %rax
	jmp _util
9:
	nop

	# Make sure we can write.
	leaq 9f(%rip), %rdi
	movq $ns_util_system_verify_writeable, %rax
	jmp _util
9:
	nop

	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_validate_implicit_arguments, %rax
	jmp _system
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Backup %r15 and %r14.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	# Reserve extra working storage space if desired.
	subq $16, %rsp
	movq $0, 8(%rsp)
	movq $0, 0(%rsp)

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	#leaq ns_cli_exit_custom_force(%rip), %r14
	leaq 6f(%rip), %r14

	# Enable custom error handlers for both success and exit.
	orq $0x06, %r15

	# Preliminarily check we can malloc and free.
	movq $0, %rcx
	movq $0, %rdx
	movq $8388608, %rsi  # Get 1MiB.
	#movq $8388607, %rsi  # Uncomment to a not-divisible-by-8 error.
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_base_malloc_require, %rax
	jmp _system
9:
	nop

	movq %rdi, %rdi  # size, in bits
	movq %rsi, %rsi  # pointer

	# Validate size of the allocation.
	cmpq $8388608, %rdi
	jz 1f
0:
	# Error: prelimiary checks failed!
	movq $0, %rcx
	leaq ns_cli_err_msg_memory_preliminary_checks(%rip), %rdx
	leaq ns_cli_err_msg_memory_preliminary_checks_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	movq $ns_system_x86_64_linux_exit_custom, %rax
	jmp _system
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
	movq $ns_system_x86_64_linux_base_mfree, %rax
	jmp _system
9:
	nop

	jmp 0f
1:
	# A child thread will run this briefly.
	#jmp 1b  # Uncomment to instead let the thread join hang forever.
	movq $ns_system_x86_64_linux_exit_success, %rax
	jmp _system
	nop
0:

	# Test we can start and join threads.
	movq $0, %rdx
	leaq 1b(%rip), %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_fork_require, %rax
	jmp _system
9:
	nop

	# Now join.
	movq %rsi, %rsi
	leaq 9f(%rip), %rdx
	xchgq %rdx, %rdi
	jmpq *%rdx
9:
	nop

	# Make sure we can sleep for a nanosecond.
	movq $1, %rdx  # 1ns
	movq $0, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	jmp _system
9:
	nop

	# Make sure we can sleep for 0 nanoseconds.
	movq $0, %rdx  # 0ns
	movq $0, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	jmp _system
9:
	nop

	# Uncomment the jmp to sleep for 2.5 seconds.
	movq $500000000, %rdx  # 500ms
	movq $2, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	#jmp _system
9:
	nop

	# Test ns_system_x86_64_linux_print_u64.
	movq $1324, %rcx  # Value.
	leaq ns_cli_msg_starts_u_size(%rip), %rdx
	movq (%rdx), %rdx  # Size.
	leaq ns_cli_msg_starts(%rip), %rdi
	leaq ns_cli_msg_starts_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	addq %rdi, %rsi  # Base.
	leaq 9f(%rip), %rdi  # Return address.
	movq $ns_system_x86_64_linux_print_u64, %rax
	jmp _system
9:
	nop

	# Make sure we can open and close ‘/proc/self/cmdline’.
	# Uncomment the 2 stdin path leaq to read from ‘/dev/stdin’ instead of
	# ‘/proc/self/cmdline’.
	movq $0, %rcx
	leaq ns_cli_path_proc_self_cmdline(%rip), %rdx
	leaq ns_cli_path_proc_self_cmdline_size(%rip), %rsi
	#leaq ns_cli_path_dev_stdin(%rip), %rdx
	#leaq ns_cli_path_dev_stdin_size(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_new_reader, %rax
	jmp _system
9:
	nop

	# ‘tuple_read()’ -> %rax.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	leaq 9f(%rip), %rdi
	movq $ns_util_get_tuple_read, %rax
	jmp _util
9:
	nop
	movq %rdi, %rax
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Read.
	subq $16, %rsp       # Start backing up.
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	subq $16, %rsp
	movq %rdx, 8(%rsp)
	movq %rcx, 0(%rsp)
	subq $16, %rsp
	movq %r8,  8(%rsp)
	movq %r9,  0(%rsp)   # Done backing up.
	movq %rcx, %r9       # ‘.read’.
	subq $40, %rsp
	movq $0x1, 0(%rsp)   # Options bitfield (enable timeout, enable timeout handler, etc.)
	movq $2,   8(%rsp)   # timeout_seconds
	movq $500000000, 16(%rsp)  # timeout_nanoseconds
	leaq ns_cli_msg_starts_s_size(%rip), %r10
	movq (%r10), %r10
	movq %r10, 24(%rsp)  # read_size
	leaq ns_cli_msg_starts(%rip), %r10
	movq %r10, 32(%rsp)  # data_pointer
	leaq ns_cli_msg_starts_s_offset(%rip), %r10
	movq (%r10), %r10
	addq 32(%rsp), %r10
	movq %r10, 32(%rsp)

	movq %rsp, %r8       # Tuple user data.
	movq %rax, %rcx      # Tuple continuation.
	movq %rdi, %rdx      # User data.
	leaq 9f(%rip), %rsi  # Success handler.
	movq $0, %rdi        # Error handler.
	jmp *%r9
9:
	nop
	addq $40, %rsp
	movq 0(%rsp), %r9    # Start restoring.
	movq 8(%rsp), %r8
	addq $16, %rsp
	movq 0(%rsp), %rcx
	movq 8(%rsp), %rdx
	addq $16, %rsp
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp       # Done restoring.

	# Close /proc/self/cmdline.
	movq %rsi, %rdx      # Get close callback.
	movq %rdi, %rsi      # Get user data.
	leaq 9f(%rip), %rdi  # Get return address.
	jmpq *%rdx
9:
	nop

	# Make sure we can open and close /dev/stdout.
	movq $0, %rcx
	leaq ns_cli_path_dev_stdout(%rip), %rdx
	leaq ns_cli_path_dev_stdout_size(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_new_writer, %rax
	jmp _system
9:
	nop

	# ‘tuple_write()’ -> %rax.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	leaq 9f(%rip), %rdi
	movq $ns_util_get_tuple_write, %rax
	jmp _util
9:
	nop
	movq %rdi, %rax
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Uncomment the jmpq to write a message stdout.
	subq $16, %rsp       # Start backing up.
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	subq $16, %rsp
	movq %rdx, 8(%rsp)
	movq %rcx, 0(%rsp)
	subq $16, %rsp
	movq %r8,  8(%rsp)
	movq %r9,  0(%rsp)   # Done backing up.
	movq %rcx, %r9       # ‘.write’.
	subq $40, %rsp
	movq $0x1, 0(%rsp)   # Options bitfield (enable timeout, enable timeout handler, etc.)
	movq $2,   8(%rsp)   # timeout_seconds
	movq $500000000, 16(%rsp)  # timeout_nanoseconds
	leaq ns_cli_msg_starts_size(%rip), %r10
	movq (%r10), %r10
	movq %r10, 24(%rsp)  # write_size
	leaq ns_cli_msg_starts(%rip), %r10
	movq %r10, 32(%rsp)  # data_pointer

	movq %rsp, %r8       # Tuple user data.
	movq %rax, %rcx      # Tuple continuation.
	movq %rdi, %rdx      # User data.
	leaq 9f(%rip), %rsi  # Success handler.
	movq $0, %rdi        # Error handler.
	jmp *%r9
9:
	nop
	addq $40, %rsp
	movq 0(%rsp), %r9    # Start restoring.
	movq 8(%rsp), %r8
	addq $16, %rsp
	movq 0(%rsp), %rcx
	movq 8(%rsp), %rdx
	addq $16, %rsp
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp       # Done restoring.

	# Close /dev/stdout.
	movq %rsi, %rdx      # Get close callback.
	movq %rdi, %rsi      # Get user data.
	leaq 9f(%rip), %rdi  # Get return address.
	jmpq *%rdx
9:
	nop

	# Make sure we can run ‘date’ through ‘system()’.
	# Uncomment the options override to enable inheritance and make the current
	# date printed out visible.
	leaq ns_cli_date_command(%rip), %rcx
	leaq ns_cli_date_command_size(%rip), %rdx
	movq (%rdx), %rdx
	movq $0x00, %rsi
	#movq $0x02, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_shell, %rax
	jmp _system
9:
	nop

	# Join ‘date’.
	movq %rdx, 8(%rsp)   # We've backed up the joiner user data.
	movq $0,   0(%rsp)

	movq %rdi, %rdx      # We'll call %rdx as our tuple call.
	movq %rsi, %rdi      # Load tuple call user data.
	leaq 9f(%rip), %rsi  # Set return address.
	jmp *%rdx            # Call the tuple (not joining quite yet; almost there)
9:
	nop

	# Now joiner is at %rdi and takes (return, joiner user data) args.
	movq %rdi,     %rdx
	movq 8(%rsp),  %rsi
	leaq 9f(%rip), %rdi
	jmp *%rdx
9:
	nop

	# Now do the same, except with ‘util’'s ‘shell_simple’ interface.
	movq $0, %rcx
	leaq ns_cli_date_command(%rip), %rdx
	leaq ns_cli_date_command_size(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	movq $ns_util_shell_simple, %rax
	jmp _system
9:
	nop

	# Finally, call ‘cli_after_prelim_checks’.
	leaq 9f(%rip), %rdi
	jmp _ns_cli_after_prelim_checks
9:
	nop

	jmp 5f  # Skip error cleanup.
6:
	# Error cleanup.  Special error handler.
	#
	# TODO: fix resource leak: cleanup memory allocations and readers and
	# writers if they have been opened but an exception was thrown before they
	# were closed!
	#
	# Just cleanup aapropriately and return to the previous error handler.
	#
	# Remember, we now have:
	# 	%rdi: Numeric code.
	# 	%rsi: String size.
	# 	%rdx: String.
	# 	%rcx: 0.
	# 1) Copy the regular cleanup except for these 4 parameters.
	addq $16, %rsp
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp
	# 2) Then return not to %rdi, but to the previous %r14.
	testq $0x2, %r15  # Double check we still have an overridden exception handler.
	jz 0f
1:
	jmpq *%r14
	nop
0:
	movq %rcx, %rcx
	movq %rdx, %rdx
	movq %rsi, %rsi
	movq %rdi, %rdi
	movq $ns_system_x86_64_linux_exit_custom, %rax
	jmp _system
	nop
5:
	# Cleanup.

	# Restore extra space.
	addq $16, %rsp

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	# Exit success.
	movq $ns_system_x86_64_linux_exit_success, %rax
	jmp _system
	nop

	nop
	hlt

	jmp ns_cli_cli
	nop
	hlt

# A wrapper around ‘exit_custom_force’, which is here since it requires no
# arguments and does not make this module require more than one linking point
# for runtime linking to refer to other modules (see the discussion on
# referring to other modules from one location for more information).
_ns_cli_exit_custom_force:
	movq $ns_system_x86_64_linux_exit_custom_force, %rax
	jmp _system
	nop
	hlt

# The CLI front-end, after some preliminary tests to make sure the system is
# functioning.  A more complete test suite is (once implemented) separate from
# these preliminary checks.
#
# Parameters:
# 	%rdi: Return.
.global ns_cli_after_prelim_checks
.set ns_cli_after_prelim_checks, (_ns_cli_after_prelim_checks - ns_cli_module_begin)
_ns_cli_after_prelim_checks:
	# TODO
	#nop
	#hlt

	# Return.
	jmp *%rdi
	nop

.global ns_cli_module_end
ns_cli_module_end:
