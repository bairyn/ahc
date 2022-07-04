# This module provides procedures to interface with the kernel through
# syscalls.

# TODO ‘exec’ after system
# TODO ‘networking’ after exec
# TODO probably for join, maybe add an option to override failing exit codes
# than crashing yourself.
# TODO how to handle networking?

# See ‘arch/x86/entry/syscalls/syscall_64.tbl’ (thanks,
# https://unix.stackexchange.com/a/499016).

# See also the Intel(R) 64 and IA-32 Architectures Software Developer's Manual,
# Combined Volumes: 1, 2A, 2B, 2C, 2D, 3A, 3B, 3C, 3D, and 4 PDF document.

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
# 		Options:
# 			Bit 0: Unused.
# 			Bit 1: Instead of the default error handler (print a message and
# 			       exit), return to the %r14 continuation (see its description
# 			       for more information).
# 			Bit 2: Unused.
# 			Bit …: Unused.
# 			Bit 63: Unused.
# 			(Unused bits should be 0, but this may or may not be checked.)
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

# We could have ‘.data’ here, but for modularity and portability, put all of
# this module in the same section, so it can be e.g. copied and referenced from
# a common base point, ‘module_begin’, with ‘module_size’.  (E.g. possibly use
# this as part of a compiled RTS.)
#.data
.text
.global ns_system_x86_64_linux_module_begin  # This jumps to ‘route’ too if you call it.
ns_system_x86_64_linux_module_begin:
	.byte 0xEB, 0x36  # skip 54 bytes (jump) if execution begins here.
	.byte 0x90, 0xF4, 0x00, 0x0E  # Pad to 8 bytes, and 0x0E is like a version.
	.byte 0x00, 0x00
.global ns_system_x86_64_linux_module_size
.set ns_system_x86_64_linux_module_size, (ns_system_x86_64_linux_module_end - ns_system_x86_64_linux_module_begin)
.global ns_system_x86_64_linux_module_size_field  # Should be at offset 8 relative to ‘begin’!
ns_system_x86_64_linux_module_size_field:
	.quad (ns_system_x86_64_linux_module_end - ns_system_x86_64_linux_module_begin)  # Size.
	.quad 0x1324ABBC  # ABI.
	.quad 0  # Module hash, sha256sum of module string with a NULL hash field and NULL external module references.  TODO; just 0 until implemented.
	.quad (_module_name_end - _module_name)  # Size of the module name string.
	.quad (_module_name - ns_system_x86_64_linux_module_begin)  # Module-relative offset to the siz of the module name string.
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
ns_system_x86_64_linux_module_route:  # Must be at offset 54.
	leaq ns_system_x86_64_linux_module_begin(%rip), %r11
	addq %r11, %rax
	jmpq *%rax
	nop

# Module dependencies.  Run-time re-linking and relocation will need to handle
# this.
# (‘system’ depends on no other module.)

_mod_dep_end:
.quad 0x12342345
.quad 0

# Now the .data stuffs.

_module_name:
	.ascii "system_x86_64_linux"
_module_name_end:

# 152 bytes of data to restore segv trap.
ns_system_x86_64_linux_sigaction_restore_segv:
	.quad 0x0  # SIG_DFL = 0
	.rept (152-8)
	.byte 0x00
	.endr
# 152 bytes of data (per-process-instantation) to set segv trap.
ns_system_x86_64_linux_sigaction_static_set_segv:
	.quad 0x0  # Will be overwritten exactly once per process instantiation.
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
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "segv trap set error: rt_sigaction failed!  Could not trap SIGSEGV.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_segv_trap_set_end:

ns_system_x86_64_linux_err_msg_segv_trap_restore_size:
	.quad (ns_system_x86_64_linux_err_msg_segv_trap_restore_end - ns_system_x86_64_linux_err_msg_segv_trap_restore)
ns_system_x86_64_linux_err_msg_segv_trap_restore:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "segv trap set error: rt_sigaction failed!  Could not restore SIGSEGV handler.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_segv_trap_restore_end:

# Note: sizes:
# 	λ length $ show (maxBound :: Word8)
# 	3
# 	λ length $ show (maxBound :: Word16)
# 	5
# 	λ length $ show (maxBound :: Word32)
# 	10
# 	λ length $ show (maxBound :: Word64)
# 	20

# Mutate at most once, static only for ns_system_x86_64_linux_verify_errno, to
# be set before quitting with an error.
ns_system_x86_64_linux_syscall_verify_builder_size:
	.quad (ns_system_x86_64_linux_syscall_verify_builder_end - ns_system_x86_64_linux_syscall_verify_builder)
ns_system_x86_64_linux_syscall_verify_builder:
	.rept 1024
	.byte 0x00
	.endr
ns_system_x86_64_linux_syscall_verify_builder_end:

ns_system_x86_64_linux_err_msg_fork_join_unknown_code_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_unknown_code_end - ns_system_x86_64_linux_err_msg_fork_join_unknown_code)
ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_offset:
	.quad 29
ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_size:
	.quad 10
ns_system_x86_64_linux_err_msg_fork_join_unknown_code:
	.ascii "Error: fork join error (code           ): the ‘waitid’ syscall gave us a ‘code’ siginfo_t attribute that we don't recognize.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_unknown_code_end:

ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch_end - ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch)
ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch:
	.ascii "Error: fork join error: the return address or %rip-based location appears to be clobbered after the ‘waitid’ syscall!\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch_end:

ns_system_x86_64_linux_err_msg_fork_join_exited_failure_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_exited_failure_end - ns_system_x86_64_linux_err_msg_fork_join_exited_failure)
ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_offset:
	.quad 29
ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_size:
	.quad 3
ns_system_x86_64_linux_err_msg_fork_join_exited_failure:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "fork join error (code    ): the ‘waitid’ syscall revealed a child process exited with a non-zero exit code!\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_exited_failure_end:

ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_end - ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo)
ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_offset:
	.quad 30
ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_size:
	.quad 3
ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo:
	.ascii "Error: fork join error (signo    ): the ‘waitid’ syscall gave us a signo we don't recognize!  Should be SIGCHLD = 17.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_end:

ns_system_x86_64_linux_err_msg_fork_join_waitid_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_waitid_end - ns_system_x86_64_linux_err_msg_fork_join_waitid)
ns_system_x86_64_linux_err_msg_fork_join_waitid:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "fork join error: the ‘waitid’ syscall failed!\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_waitid_end:

ns_system_x86_64_linux_err_msg_clock_nanosleep_size:
	.quad (ns_system_x86_64_linux_err_msg_clock_nanosleep_end - ns_system_x86_64_linux_err_msg_clock_nanosleep)
ns_system_x86_64_linux_err_msg_clock_nanosleep:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "monotonic_nanosleep error: the ‘clock_nanosleep’ syscall failed!  Failed to sleep.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_clock_nanosleep_end:

# Utility: digits
ns_system_x86_64_linux_util_digits:
.ascii "0123456789ABCDEF"
.byte 0x00

# Memory management metadata.

# CURRENTLY UNUSED.
#
# This is a root reference into the graph of base allocations of memory.  Each
# allocation begins with a fixed size header that just references up to 3 other
# allocations.  These pointers are after the size of this allocation.
ns_system_x86_64_linux_memory_pointer:
	.quad 0  # size
	.quad 0  # ref0
	.quad 0  # ref1
	.quad 0  # ref2

# (We could just write to machine code that has these error messages, but since
# we're at a low level, we may want to wait until we have higher level
# abstractions before we attempt that.)
ns_system_x86_64_linux_err_msg_base_malloc_error_size:
	.quad (ns_system_x86_64_linux_err_msg_base_malloc_error_end - ns_system_x86_64_linux_err_msg_base_malloc_error)
ns_system_x86_64_linux_err_msg_base_malloc_error:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "base malloc error: mmap() failed!  Could not allocate memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_malloc_error_end:

ns_system_x86_64_linux_err_msg_base_mfree_error_size:
	.quad (ns_system_x86_64_linux_err_msg_base_mfree_error_end - ns_system_x86_64_linux_err_msg_base_mfree_error)
ns_system_x86_64_linux_err_msg_base_mfree_error:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "base mfree error: munmap() failed!  Could not free memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_mfree_error_end:

ns_system_x86_64_linux_err_msg_base_malloc_4th_size:
	.quad (ns_system_x86_64_linux_err_msg_base_malloc_4th_end - ns_system_x86_64_linux_err_msg_base_malloc_4th)
ns_system_x86_64_linux_err_msg_base_malloc_4th:
	.ascii "Error: base malloc argument error: the 4th argument must be zero!  Could not allocate memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_malloc_4th_end:

ns_system_x86_64_linux_err_msg_base_mfree_5th_size:
	.quad (ns_system_x86_64_linux_err_msg_base_mfree_5th_end - ns_system_x86_64_linux_err_msg_base_mfree_5th)
ns_system_x86_64_linux_err_msg_base_mfree_5th:
	.ascii "Error: base mfree argument error: the 5th argument must be zero!  Could not free memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_mfree_5th_end:

ns_system_x86_64_linux_err_msg_base_malloc_enomem_size:
	.quad (ns_system_x86_64_linux_err_msg_base_malloc_enomem_end - ns_system_x86_64_linux_err_msg_base_malloc_enomem)
ns_system_x86_64_linux_err_msg_base_malloc_enomem:
	.ascii "Error: base malloc error: not enough memory, so mmap() failed with ENOMEM (12, %rax -12)!  Could not allocate memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_malloc_enomem_end:

ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_size:
	.quad (ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_end - ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible)
ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible:
	.ascii "Error: base malloc error: invalid base_malloc arguments: the requested bits must be divisible by 8; be sure to specify length in bits, not bytes!  Could not allocate memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_end:

ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible_size:
	.quad (ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible_end - ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible)
ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible:
	.ascii "Error: base mfree error: invalid base_mfree arguments: the requested bits must be divisible by 8; be sure to specify length in bits, not bytes!  Could not free memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible_end:

ns_system_x86_64_linux_err_msg_fork_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_end - ns_system_x86_64_linux_err_msg_fork)
ns_system_x86_64_linux_err_msg_fork:
	.ascii "fork error: the ‘fork()’ syscall failed!  Could not fork.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_end:

ns_system_x86_64_linux_err_msg_new_writer_r8_not_zero_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_r8_not_zero_end - ns_system_x86_64_linux_err_msg_new_writer_r8_not_zero)
ns_system_x86_64_linux_err_msg_new_writer_r8_not_zero:
	.ascii "Error: ‘new_writer’ error: %r8 should be 0 but is not!  Please set the arguments correctly.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_r8_not_zero_end:

ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero_end - ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero)
ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero:
	.ascii "Error: ‘new_reader’ error: %r8 should be 0 but is not!  Please set the arguments correctly.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero_end:

ns_system_x86_64_linux_err_msg_new_writer_open_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_open_failed_end - ns_system_x86_64_linux_err_msg_new_writer_open_failed)
ns_system_x86_64_linux_err_msg_new_writer_open_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_writer’ error: the ‘open’ syscall failed!  Could not create the new writer.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_open_failed_end:

ns_system_x86_64_linux_err_msg_new_reader_open_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_open_failed_end - ns_system_x86_64_linux_err_msg_new_reader_open_failed)
ns_system_x86_64_linux_err_msg_new_reader_open_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_reader’ error: the ‘open’ syscall failed!  Could not create the new reader.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_open_failed_end:

ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_end - ns_system_x86_64_linux_err_msg_new_writer_unsupported_options)
ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_u_offset:
	.quad 16
ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_u_size:
	.quad 20
ns_system_x86_64_linux_err_msg_new_writer_unsupported_options:
	.ascii "Error (bitfield                     ): ‘new_writer’ error: %rcx has unsupported options bits enabled!  Please set the arguments correctly.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_end:

ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_end - ns_system_x86_64_linux_err_msg_new_reader_unsupported_options)
ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_u_offset:
	.quad 16
ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_u_size:
	.quad 20
ns_system_x86_64_linux_err_msg_new_reader_unsupported_options:
	.ascii "Error (bitfield                     ): ‘new_reader’ error: %rcx has unsupported options bits enabled!  Please set the arguments correctly.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_end:

ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated_end - ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated)
ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated:
	.ascii "Error: ‘new_writer’ error: the pathname should be null-terminated but is not!  Please set the arguments correctly.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated_end:

ns_system_x86_64_linux_err_msg_new_reader_path_not_null_terminated_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_path_not_null_terminated_end - ns_system_x86_64_linux_err_msg_new_reader_path_not_null_terminated)
ns_system_x86_64_linux_err_msg_new_reader_path_not_null_terminated:
	.ascii "Error: ‘new_reader’ error: the pathname should be null-terminated but is not!  Please set the arguments correctly.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_path_not_null_terminated_end:

ns_system_x86_64_linux_err_msg_new_writer_close_close_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_close_close_failed_end - ns_system_x86_64_linux_err_msg_new_writer_close_close_failed)
ns_system_x86_64_linux_err_msg_new_writer_close_close_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_writer’ ‘.close’ error: the ‘close’ syscall failed!  Could not close the writer.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_close_close_failed_end:

ns_system_x86_64_linux_err_msg_new_reader_close_close_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_close_close_failed_end - ns_system_x86_64_linux_err_msg_new_reader_close_close_failed)
ns_system_x86_64_linux_err_msg_new_reader_close_close_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_reader’ ‘.close’ error: the ‘close’ syscall failed!  Could not close the reader.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_close_close_failed_end:

ns_system_x86_64_linux_err_msg_invalid_implicit_args_size:
	.quad (ns_system_x86_64_linux_err_msg_invalid_implicit_args_end - ns_system_x86_64_linux_err_msg_invalid_implicit_args)
ns_system_x86_64_linux_err_msg_invalid_implicit_args:
	#.ascii "Error: implicit args validation error: invalid implicit arguments: actions in this module required implicit arguments as parameters %r15 and %r14 to be as specified, but they are invalid!  Check the usage of this module, to make sure its procedures are being called correctly; check the documentation.\n"
	.ascii "Error:\n"
	.ascii "	implicit args validation error: invalid implicit arguments: actions in\n"
	.ascii "	this module required implicit arguments as parameters %r15 and %r14 to be\n"
	.ascii "	as specified, but they are invalid!  Check the usage of this module, to\n"
	.ascii "	make sure its procedures are being called correctly; check the\n"
	.ascii "	documentation.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_invalid_implicit_args_end:

# For our compiler, we only require the ability to read and write files and to
# exit.  We can work out the rest.  But we also add some concurrency support
# and shell support.  Also add a few other utilities as needed.
.text

# ################################################################
# Base.
# ################################################################

# Do nothing but return (or call an alternative continuation).
#
# Parameters:
# 	%rdi: return
.global ns_system_x86_64_linux_nop
.set ns_system_x86_64_linux_nop, (_ns_system_x86_64_linux_nop - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_nop:
	jmpq *%rdi
	nop

# ################################################################
# Memory management.
# ################################################################

# Instead of taking the time to think about alternative paradigms, I just
# followed a simple memory allocator that claims ranges of unclaimed virtual
# memory.  (If I had more time, it would be interesting to look for alternative
# approaches; but let's just get this going first.)

# On our platform, we already have at least a few options:
# - Pre-allocated, fixed-size memory.
# - ‘sbrk’
# - ‘mmap’

# We basically just make our own heap here.

# Get a chunk of memory, of largeish size, e.g. ~1GiB per allocation.
#
# You must track the metadata (especially the bytes reserved to pass onto the
# ‘free’ action) as the API is meant to be a minimal API layer around the
# platform.
#
# This is probably also unsuitable for making many small allocations, so for
# normal applications, you probably want to add an abstraction or layer on top
# of this one to handle many variable-size allocations.
#
# This call will exit with failure if memory cannot be allocated except when
# there is a shortage of memory to be allocated.  (In the future, support for
# overriding error handling is probably a good idea (TODO).)
#
# Allocations are anonymous and shared.
#
# Parameters:
# 	%rdi: Return with args:
# 		%rdi: Zero on non-fatal error (no available memory), one on success.
# 		%rsi:
# 			If non-fatal error, pointer to static data containing information
# 			about the error, currently just a null-terminated string.  If
# 			success, number of bits allocated (currently, equal to requested
# 			size).
# 		%rdx: Pointer in virtual memory to the beginning of the allocation.
# 			I recommend having the size of the allocation as the first uint64,
# 			then 3 uint64 pointers to other allocations, then your own base
# 			allocation header to customize how you want to handle and track
# 			allocations at runtime, but how you use this allocation usp to you,
# 			but since we don't track the allocations and metadata ourselves
# 			(although we might be able to get information from the kernel),
# 			this must be handled at a higher level.
#
# 			If non-fatal error, unused.
# 	%rsi: Requested size in bits.
# 	%rdx:
# 		Offered ‘price’, where a higher price may be more likely to result in
# 		allocation if resources are scarce.
# 	%rcx:
# 		Please set to 0.  Future versions may want to add flags or other
# 		options for base allocation.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
# 	- %rax
#
# (However, currently %rsi does happen to be preserved, but this is for the
# current implementation.)
.global ns_system_x86_64_linux_base_malloc
.set ns_system_x86_64_linux_base_malloc, (_ns_system_x86_64_linux_base_malloc - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_base_malloc:
	# Verify the 4th argument is 0.
	testq %rcx, %rcx
	jz 1f
0:
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_4th(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_4th_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	# The next two lines are equivalent to the jump.  The intel manual says a
	# 16-byte relative is ‘N.S.’ in 64-bit mode, but 32-bit relative jumps are
	# ‘Valid’ in 64-bit mode.
	#.byte 0xE9
	#.long _ns_system_x86_64_linux_exit_custom - . - 4
	nop
	hlt
1:

	# Now backup the return in the storage unit used for the ‘int fd’
	# parameter, which is ignored.  This is at the cost that in some
	# implementations (probably not typical for our own platform), it is
	# required to be ‘-1’.  5th Parameter.
	movq %rdi, %r8

	# Convert %rsi from bit units to byte quantities.  Ensure it is divisible
	# by 8.
	testq $0x7, %rsi
	jz 1f
0:
	# Error: bits not divisible by 8!
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:
	shr $3, %rsi

	# So just ignore the price in %rdx for now.  Cool idea, though, I guess.
	movq %rdx, %rdx

	# Request an allocation of memory via ‘mmap()’.
	movq $0, %r9
	movq %r8, %r8
	movq $33, %r10  # MAP_ANONYMOUS (0x20) | MAP_SHARED (0x01) = 33; requires linux >= 2.4.
	movq $7, %rdx
	movq %rsi, %rsi
	movq $0, %rdi
	movq $9, %rax  # mmap
	syscall

	# Check for ENOMEM (12) with %rax of -12, in which case we skip verification.
	cmpq $-12, %rax
	jnz 1f
0:
	# We'll just return with an error message indicating we don't have enough
	# memory.

	# Set up continuation arguments.
	leaq ns_system_x86_64_linux_err_msg_base_malloc_enomem(%rip), %rsi
	movq $0, %rdx
	movq %rsi, %rsi
	movq $0, %rdi

	movq %r8, %rcx
	jmpq *%rcx
	nop
1:

	# Back up %rsi (number of bits requested and allocated) and %rax
	# (pointer).
	movq %rsi, %r9
	movq %rax, %rax

	# Verify, unless it was due to insufficient resources.
	leaq ns_system_x86_64_linux_err_msg_base_malloc_error(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_error_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Return.
	# Set up continuation arguments.
	movq %rax, %rdx  # allocation
	movq %r9, %rsi  # bits allocated
	shl $3, %rsi
	movq $1, %rdi

	movq %r8, %rcx
	jmpq *%rcx
	nop

# At a low-level, free an allocation made by
# ‘ns_system_x86_64_linux_base_malloc’.
#
# You must keep track of the allocations, and its size, and must handle
# resource management and freeing on top of what uses this memory.  This API is
# meant to be a minimal wrapper around the platform, a base upon which a higher
# level allocation or memory management API can be written.
#
# Parameters:
# 	%rdi: Return.
# 	%rsi: Size of the allocation to free *in bits*.
# 	%rdx:
# 		Base address: pointer to the virtual memory address of the beginning of
# 		the allocation.
# 	%rcx:
# 		Offered ‘price’, where a lower price may be more likely to result in
# 		the allocation being freed if resources are scarce.
# 	%r8:
# 		Please set to 0.  Future versions may want to add flags or other
# 		options for base memory freeing.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers except %r9:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %rax
#
# (Although %rdi happens to be preserved.)
.global ns_system_x86_64_linux_base_mfree
.set ns_system_x86_64_linux_base_mfree, (_ns_system_x86_64_linux_base_mfree - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_base_mfree:
	# Verify the 5th argument is 0.
	testq %r8, %r8
	jz 1f
0:
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_base_mfree_5th(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_mfree_5th_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# Now backup the return in the storage unit used for %r8, since we
	# otherwise don't use it.
	movq %rdi, %r8

	# Convert %rsi from bit units to byte quantities.  Ensure it is divisible
	# by 8.
	testq $0x7, %rsi
	jz 1f
0:
	# Error: bits not divisible by 8!
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:
	shr $3, %rsi

	# So just ignore the price in %rcx for now.  Cool idea, though, I guess.
	movq %rcx, %rcx

	# Free the memory via ‘munmap()’.
	movq %rsi, %rsi
	movq %rdx, %rdi
	movq $11, %rax  # munmap
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_base_mfree_error(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_base_mfree_error_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Return.
	movq %r8, %rdi
	jmpq *%rdi
	nop

# Simple utility to wrap around base_malloc but require successful allocation,
# failing also if there is not enough memory available, treating ENOMEM as a
# fatal error, so that we crash with an error message when this happens.
#
# (TODO: allow callbacks and greater configurability for such errors, to let
# higher levels cleanup and such.)
#
# Takes almost the same arguments as base_malloc, except the %rdi is updated:
# 	%rdi: Return with args:
# 		%rdi:
# 			Number of bits allocated (currently, equal to requested size).
# 		%rsi: Pointer in virtual memory to the beginning of the allocation.
# 			I recommend having the size of the allocation as the first uint64,
# 			then 3 uint64 pointers to other allocations, then your own base
# 			allocation header to customize how you want to handle and track
# 			allocations at runtime, but how you use this allocation usp to you,
# 			but since we don't track the allocations and metadata ourselves
# 			(although we might be able to get information from the kernel),
# 			this must be handled at a higher level.
# 	%rsi: Requested size in bits.
# 	%rdx:
# 		Offered ‘price’, where a higher price may be more likely to result in
# 		allocation if resources are scarce.
# 	%rcx:
# 		Please set to 0.  Future versions may want to add flags or other
# 		options for base allocation.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
# 	- %rax
.global ns_system_x86_64_linux_base_malloc_require
.set ns_system_x86_64_linux_base_malloc_require, (_ns_system_x86_64_linux_base_malloc_require - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_base_malloc_require:
	# (Technically, internally, we would do a real wrapper, but since the mmap
	# syscall leaves us with limited working storage space from registers
	# alone, we'll just copy it and modify it under the hood, to avoid
	# needlessly adding a stack %rsp dependency.)

	# Verify the 4th argument is 0.
	testq %rcx, %rcx
	jz 1f
0:
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_4th(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_4th_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# Now backup the return in the storage unit used for the ‘int fd’
	# parameter, which is ignored.  This is at the cost that in some
	# implementations (probably not typical for our own platform), it is
	# required to be ‘-1’.  5th Parameter.
	movq %rdi, %r8

	# Convert %rsi from bit units to byte quantities.  Ensure it is divisible
	# by 8.
	testq $0x7, %rsi
	jz 1f
0:
	# Error: bits not divisible by 8!
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:
	shr $3, %rsi

	# So just ignore the price in %rdx for now.  Cool idea, though, I guess.
	movq %rdx, %rdx

	# Request an allocation of memory via ‘mmap()’.
	movq $0, %r9
	movq %r8, %r8
	movq $33, %r10  # MAP_ANONYMOUS (0x20) | MAP_SHARED (0x01) = 33; requires linux >= 2.4.
	movq $7, %rdx
	movq %rsi, %rsi
	movq $0, %rdi
	movq $9, %rax  # mmap
	syscall

	# Back up %rsi (number of bits requested and allocated) and %rax
	# (pointer).
	movq %rsi, %r9
	movq %rax, %rax

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_base_malloc_error(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_error_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Return.
	# Set up continuation arguments.
	movq %rax, %rsi  # allocation
	movq %r9, %rdi  # bits allocated
	shl $3, %rdi

	movq %r8, %rdx
	jmpq *%rdx
	nop

# ################################################################
# Concurrency.
# ################################################################

# TODO: like ns_system_x86_64_linux_fork_require, except insufficient resources
# is a non-fatal error.
#
# This clobbers TODO.
#.global ns_system_x86_64_linux_fork
ns_system_x86_64_linux_fork:
	# TODO
	hlt

# Spawn another executor.
#
# The new executor (thread) can exit with exit_success.
#
# Note: as this spawns a new OS thread, it may be desireable to add a layer of
# abstraction for threads on top of this, e.g. by gradually increasing and
# decreasing the pool of OS threads as needed, where the higher-level
# abstraction has less bookkeeping costs than creating and destroying OS
# threads.
#
# Note: the joiner does use the thread's stack space in order to give the
# kernel a place to give us information about the child thread.
#
# Parameters:
# 	%rdi: Return with arguments:
# 		%rdi: joiner: continuation callback with the following parameters:
# 			%rdi: Return after joining, blocking and waiting until the thread finishes.  (And fail with an error instead if the joined thread failed with an error.)
# 			%rsi: User data, to make this callback a closure.
# 		%rsi: user data that must be provided as the second argument (%rsi) to the joiner.
# 	%rsi: Where the new executor starts executing.
# 	%rdx:
# 		Offered ‘price’, where a higher price may be more likely to result in
# 		allocation if resources are scarce.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers except r9:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %rax
#
# (Technically, the clobbering can be exploited in the current implementation
# such that %r9 can pass user data to the child thread.)
.global ns_system_x86_64_linux_fork_require
.set ns_system_x86_64_linux_fork_require, (_ns_system_x86_64_linux_fork_require - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_fork_require:
	# Note: I would use the clone syscall (56) to be more explicit but only use
	# the fork syscall (57) for convenience because it requires fewer storage
	# units.

	# So just ignore the price in %rdx for now.  Cool idea, though, I guess.
	movq %rdx, %rdx

	# Clear out the first 32 bits of the PID, which we store as the user data.
	movq $0, %rax

	# Backup input arguments.
	movq %rdi, %r8
	#movq %rsi, %r9  # Just use xchngq in the Verify step instead.  Saves us a register.

	# Fork.
	movq $57, %rax  # fork  #  (First 32 bits are 0.)
	syscall

	# Verify and backup %rsi.
	leaq ns_system_x86_64_linux_err_msg_fork(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_size(%rip), %rdx
	movq (%rdx), %rdx
	xchgq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (If this doesn't exit, it doesn't clobber %r8.)
9:
	nop
	xchgq %rax, %rsi

	# If we're the child, go to the child continuation.
	testq %rax, %rax
	jnz 0f
	jmpq *%rsi
	nop
0:

	# We're the parent.  Set up arguments to return and then return.
	movq %rax, %rsi
	leaq _ns_system_x86_64_linux_fork_join(%rip), %rdi
	movq %r8, %rdx
	jmpq *%rdx
	nop

# Used by ‘ns_system_x86_64_linux_fork_require’; the action part of the
# closure.
#
# Parameters:
# 	%rdi: Return after joining, blocking and waiting until the thread finishes.  (And fail with an error instead if the joined thread failed with an error.)
# 	%rsi: User data.  (Internally, the PID of the thread to join to.)
#
# Without any error, this clobbers no registers; it uses the stack to preserve
# them.  (This is optional, as in the x64 linux calling convention, these are
# caller preserved, not callee-preserved.)
#
# This uses 336 bytes of stack space (equivalent to 42 ‘uint64_t’s).
_ns_system_x86_64_linux_fork_join:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# According to the man pages, the ‘waitid’ (247 for x86_64-linux; there's
	# an ‘x32’ 529 listin in syscall_64.tbl) is preferred for new applications
	# over the nonstandard ‘wait4’ (61) syscall, although the former takes 5
	# arguments and the latter takes 4.

	# Since we're already using stack space here as a convenience to interface
	# with the kernel so it has a place to write the information to, we'll also
	# just use the stack to backup the input parameters, rather than taking the
	# 4-parameter variant.
	#
	# Also backup %rax, %r11, and the first 4 standard parameter registers.
	subq $16, %rsp
	movq $0,   8(%rsp)  # Padding for a 16-byte-aligned stack.
	movq %rdi, 0(%rsp)

	subq $16, %rsp
	movq %rsi, 8(%rsp)
	movq %rax, 0(%rsp)

	subq $16, %rsp
	movq %r11, 8(%rsp)
	movq %rdx, 0(%rsp)

	subq $16, %rsp
	movq %rcx, 8(%rsp)

	# For an extra check, explained below, backup our
	# _ns_system_x86_64_linux_fork_join instance.
	leaq _ns_system_x86_64_linux_fork_join(%rip), %r11
	movq %r11, 0(%rsp)

	# siginfo_t looks to be uint32_t signo, uint32_t errno, uint32_t code,
	# padding, fields union.  (Apparently some architectures swap the order of
	# errno and code).  sizeof() on my system tells me it's 128 bytes long.
	# Just give _.
	#
	# However, in case the kernel thinks this struct is longer, we'll perform
	# an extra check to make sure our recovered return address (!) is the same
	# as an extra backup of the return address that we put below this stack.
	# Also for extra validation, get a copy of the thread's current execution
	# pointer and put it after and before to make sure it checks out.
	subq $256, %rsp
	# Track a pointer to this siginfo_t struct we just made.
	movq %rsp, %rdx

	# For an extra check, explained below, backup our
	# _ns_system_x86_64_linux_fork_join instance.
	leaq _ns_system_x86_64_linux_fork_join(%rip), %r11
	subq $16, %rsp
	movq %r11, 8(%rsp)

	# Back up an extra %rdi before, not just after.
	movq %rdi, 0(%rsp)

	# Back up %r15 and %r14.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	# Before we hand over control to any actions capable of causing non-fatal
	# exceptions, we need to provide a cleanup routine now, via the implicit
	# parameters %r15 and %r14.  We won't change %r15; we have no need to
	# change it; we'll only change %r14 and restore it, so that *if* it's used,
	# cleanup is handled correctly.

	# We'll reserve more space on the stack.
	leaq 6f(%rip), %r14

0:
	# ‘waitid’ syscall (linux >= 2.6.9).
	movq $0, %r8  # struct rusage *
	movq $4, %r10  # int options - WEXITED (4), but not WSTOPPED (2) or WCONTINUED (8).
	movq %rdx, %rdx  # siginfo_t * - we allocated space on our thread's stack for this struct.
	movq %rsi, %rsi  # id_t (probably int / u32) - the PID (just the user data)
	movq $1, %rdi  # idtype_t (probably enum / int): P_PID = 1, so ‘id’ is the PID.
	movq $247, %rax  # waitid
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_fork_join_waitid(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_waitid_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Double check our 2 values, to make sure they still match.
	# Check original %rdi.
	movq 16(%rsp), %rdi
	movq 336(%rsp), %rsi
	cmpq %rsi, %rdi
	jne 2f

	# Check the %rip-based location, for fork_join.
	movq 24(%rsp), %rdi
	movq 288(%rsp), %rsi
	cmpq %rsi, %rdi
	jne 2f

	jmp 3f
2:
	# Error: We don't recognize the code.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
3:

	# Sanity check: just make sure signo is SIGCHLD (17).
	movq $0, %rdi
	movl 32(%rsp), %edi  # signo
	cmpq $17, %rdi
	jz 1f
	# We don't recognize the signo.

	# First, write to this instance's error message storage to print the
	# unknown code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_print_u64
9:
	nop

	# Error: We don't recognize the code.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# See if the process terminated yet, rather than us receiving a process
	# status change for some other reason, in which case repeat the ‘waitid’
	# syscall.
	#
	# uint32_t signo (always CLD_EXITED), uint32_t status/errno, uint32_t code
	# (we're only interested in CLD_EXITED=1, CLD_KILLED=2, and CLD_DUMPED=3;
	# not CLD_STOPPED=5, CLD_TRAPPED=4, or CLD_CONTINUED=6), padding, union.
	movq $0, %rdi
	movl 40(%rsp), %edi  # code
	cmpq $5, %rdi
	jz 0b
	cmpq $4, %rdi
	jz 0b
	cmpq $6, %rdi
	jz 0b

	cmpq $1, %rdi
	jz 1f
	cmpq $2, %rdi
	jz 1f
	cmpq $3, %rdi
	jz 1f
2:
	# We don't recognize the code.

	# First, write to this instance's error message storage to print the
	# unknown code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_print_u64
9:
	nop

	# Error: We don't recognize the code.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# The process terminated.
	# But now see if the process exited succesfully.  If it didn't, abort /
	# exit with error.
	movq $0, %rdi
	movl 36(%rsp), %edi  # status/errno
	testq %rdi, %rdi
	jz 0f
1:
	# A thread failed.

	# First, write to this instance's error message storage to print the
	# exit code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_print_u64
9:
	nop

	# Error: a thread failed.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# The thread didn't fail.

	jmp 5f  # Skip to cleanup.
6:
	# Error handler.
	#
	# Just cleanup aapropriately and return to the previous error handler.
	#
	# Remember, we now have:
	# 	%rdi: Numeric code.
	# 	%rsi: String size.
	# 	%rdx: String.
	# 	%rcx: 0.

	# 1) Copy the regular cleanup except for these 4 parameters.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp
	#movq 0(%rsp), %rdi
	addq $16, %rsp
	addq $256, %rsp
	#movq 8(%rsp), %rcx
	addq $16, %rsp
	#movq 0(%rsp), %rdx
	movq 8(%rsp), %r11
	addq $16, %rsp
	movq 0(%rsp), %rax
	#movq 8(%rsp), %rsi
	addq $16, %rsp
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
	jmp _ns_system_x86_64_linux_exit_custom
	nop
5:
	# Cleanup.

	# Restore the stack and registers.

	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	movq 0(%rsp), %rdi
	# 8(%rsp): Extra fork_join reference.
	addq $16, %rsp

	addq $256, %rsp  # struct siginfo_t

	# 0(%rsp): Extra fork_join reference.
	movq 8(%rsp), %rcx
	addq $16, %rsp

	movq 0(%rsp), %rdx
	movq 8(%rsp), %r11
	addq $16, %rsp

	movq 0(%rsp), %rax
	movq 8(%rsp), %rsi
	addq $16, %rsp

	# 0(%rsp): Another %rdi.
	# 8(%rsp): Padding.
	addq $16, %rsp  # Another %rdi.

	# Return.
	jmpq *%rdi
	nop

# ################################################################
# Reading and writing.
# ################################################################

# The read/write API provided here uses non-blocking IO.

# WARNING: in this low-level API, there is no enforcement against resource
# leaks (creating without closing) or multiple destructions from duplications
# (closing more than once), so you must handle this responsibility yourself.
# Be sure to consume resources created by calling one the callbacks.

# Linear types is a good way for the compiler to ensure that in any otherwise
# successful program execution, each creation of a resource leads to exactly
# one destruction of a resource.  There are other constraints for resources to
# follow, but this is a common one.  (Thanks for the inspiration, Rust and
# ‘Linear Types Can Change the World’!)

# Create a new writer, an instance capable of writing to a filepath.
#
# The FRP time context is an implicit parameter.
#
# ‘new_writer’ will call return with multiple callbacks.  You must call exactly
# one, at which point the original callbacks are considered consumed and must
# not be re-used since they have been destroyed, but you must instead only use
# any new callbacks that are provided.  Most callbacks will return a new set of
# callbacks with the same or with a compatible API.  The close callbacks do not
# return a new set of callbacks.  (Responsibility for managing resources is not
# dealt with here, as this is meant to be a low-level API.)
#
# Symlinks are followed except for noclobber.
#
# This tree may be easier to parse if you configure your vim to visually
# display tabs and spaces differently, as I have.  Tabs are used for
# indentation, and spaces for alignment here.
#
# Parameters:
# 	%rdi: Return with single-use-only (consumes the entire set at that level)
# 	      (clobbering!) callbacks, with parameters:
# 		%rdi: User data as part of a returned closure.  You pass it to %rsi or
# 		      any other callback so that it has data to work with.  Under the
# 		      hood, this is just the file descriptor.
# 		%rsi: Close callback to destroy the resource and discard any pending
# 		      reads or writes, with parameters:
# 			%rdi: Return.
# 			%rsi: User data.
# 		%rdx: Query status callback, with parameters:
# 			%rdi: Return, with parameters:
# 				[%rdi, %rsi, …, %r8]: user data and set of new callbacks
# 				                      corresponding to the original
# 				                      [%rdi, %rsi, …, %r8] arguments.  Don't
# 				                      use the old ones, because by now they are
# 				                      already consumed (although the new ones
# 				                      may refer to the same address).
# 				%r9: zero if not ready for writing, 1 if ready.
# 			%rsi: User data.
# 		%rcx: Attempt to write optionally with timeout callback, with parameters:
# 			%rdi: Return on non-fatal error (basically EAGAIN; means it would
# 			      block and is not available for the write or read), with
# 			      parameters:
# 				[%rdi, %rsi, …, %r8]: user data and set of new callbacks
# 				                      corresponding to the original
# 				                      [%rdi, %rsi, …, %r8] arguments.  Don't
# 				                      use the old ones, because by now they are
# 				                      already consumed (although the new ones
# 				                      may refer to the same address).
# 				%r9: status code: 0 in the general case, 1 if the resource is
# 				     exhausted (e.g. we've detected that it's closed or
# 				     otherwise won't be available for any writes in the
# 				     future), as a platform-specific convenience hint.
# 			%rsi: Return on success, with parameters:
# 				[%rdi, %rsi, …, %r8]: user data and set of new callbacks
# 				                      corresponding to the original
# 				                      [%rdi, %rsi, …, %r8] arguments.  Don't
# 				                      use the old ones, because by now they are
# 				                      already consumed (although the new ones
# 				                      may refer to the same address).
# 				%r9: Number of bytes written.
# 			%rdx: tuple callback to get options and timeout, with parameters
# 			      (we use a tuple instead of direct parameters to compress
# 			      parameters into fewer parameters (3 into 2) so we don't
# 			      depend on a stack as much):
# 				%rdi: Return (please return back to this address when done),
# 				      with parameters:
# 					%rdi: Options bitfield: bit 0 is boolean for 0 to disable
# 					      timeout, 1 to enable timeout.
# 					%rsi: Seconds for the timeout (i64).
# 					%rdx: Nanoseconds for the timeout (i64).
# 					%rcx: Please set to 0, to make future enhancements more
# 					      convenient.
# 				%rsi: User data.
# 			%rcx: user data supplied to %rdx as a closure
# 			%r8: Number of bytes to request a write for (i.e., the size).
# 			%r9: The data to request a write for (i.e., the data).
# 		%r8: currently 0, in case future versions want to add functionality.
# 		     (If the API is extended with APIs, I'd probably make the next
# 		     addition a tuple to contain, e.g. set %r8 to 1 and then %r9
# 		     becomes the tuple callback (don't worry, %rdi is already available
# 		     as the user data).)
# 	%rsi: Size of filepath.
# 	%rdx: Pointer to filepath.  Null-terminate it for the kernel.
# 	%rcx: Options bitfield:
# 		Bit 0: noclobber: 0 does nothing, and 1 overrides other conflicting
# 		                  options for handling already-existing files and
# 		                  automatically closes the writer if the file already
# 		                  exists.  The ‘noclobber’ flag.  Autoclose if exists.
# 		Bit 1: append: if the file already exists, append writes instead of
# 		               overwriting or discarding the original file, if 1;
# 		               if 0, work as though this writer replaces anything
# 		               already there.
# 		Bit 2: direct: auto-sync and avoid caching where possible if 1.
# 		Bit 3: incognito: NOATIME: request concealing the access via the ATIME
# 		                  attribute.
# 		Bit 4: fallback: use more default bits in case the syscall flags is
# 		                 giving the kernel issues.
# 		Bit 5: shared: don't enable O_COEXEC, so that child processes made
# 		               through ‘exec’ to replace an old process already have
# 		               this file descriptor opened too, that is it is
# 		               inherited.
# 		Rest: set to 0.
# 	%r8: Please set to 0, so that future versions can accept an extra
# 	     parameter if needed, e.g. for I/O options.
#
# This clobbers all parameter registers, because they are passed to the return
# continuation, and one extra working storage unit is needed to hold the return
# continuation itself:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
#
# (Since the stack is used, this preserves (but would otherwise clobber) all
# syscall registers (%rax, %rcx, %r11, %r10) and parameter registers (%rdi,
# %rsi, %rdx, %rcx, %r8, %r9), except the parameter registers used for the
# callback:)
# 	- %rax
# 	- %rcx
# 	- %r11
# 	- %r10 (syscall parameter)
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
#
# This action requires the stack in order for its memory requirements.
.global ns_system_x86_64_linux_new_writer
.set ns_system_x86_64_linux_new_writer, (_ns_system_x86_64_linux_new_writer - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_new_writer:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# First backup the input arguments and what we clobber onto the stack.

	subq $16, %rsp
	movq %rax, 8(%rsp)
	movq %rcx, 0(%rsp)

	subq $16, %rsp
	movq %r11, 8(%rsp)
	movq %r10, 0(%rsp)

	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)

	subq $16, %rsp
	movq %rdx, 8(%rsp)
	movq %rcx, 0(%rsp)

	subq $16, %rsp
	movq %r8, 8(%rsp)
	movq %r9, 0(%rsp)

	# Backup %r15 and %r14.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	leaq 6f(%rip), %r14

	# Next just perform a few checks.

	# Make sure %r8 is 0.
	testq %r8, %r8
	jz 0f
	# Error: invalid arguments.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Now we can clear %r8, discard it and use it for our own purposes, since
	# we can restore it to $0.
	movq %r8, %r8

	# Make sure unsupported bits in the options bitfield are 0, to aid in
	# future enhancements.
	testq $0xFFFFFFFFFFFFFFC0, %rcx
	jz 0f

	# Error: invalid arguments.

	# First, write to this instance's error message storage to print the
	# exit code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_u_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_print_u64
9:
	nop

	# Error.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_unsupported_options(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Ensure the filepath is null-terminated.
	movq 24(%rsp), %rdx  # Data.
	movq 32(%rsp), %rsi  # Size.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_is_null_terminated
9:
	testq %rdi, %rdi
	jnz 0f
1:
	# Error: the filepath is not null-terminated.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Calculate the flags, into %rsi.
	movq $0, %rsi  # Clear flags.
	movq 16(%rsp), %rdi  # Original %rcx, options bitfield.

	# Base options.
	orq $0x40,  %rsi  # |= (O_CREAT=64 (0x40))
	orq $0x800, %rsi  # |= (O_NONBLOCK=2048 (0x800))
	orq $0x0,   %rsi  # |= (O_RDONLY=0 (0x0))
# O_CLOEXEC=524288 (0x80000)

	# (not) Shared?
	testq $0x20, %rdi
	jnz 0f
1:
	orq $0x80000, %rsi  # O_CLOEXEC=524288 (0x80000)
0:

	# (not) Fallback?
	testq $0x10, %rdi
	jnz 0f
1:
	orq $0x8000, %rsi  # O_LARGEFILE=32768 (0x8000)
0:

	# Incognito?
	testq $0x08, %rdi
	jz 0f
1:
	orq $0x1000000, %rsi  # O_NOATIME=16777216 (0x1000000)
0:

	# Direct?
	testq $0x04, %rdi
	jz 0f
1:
	# (O_SYNC pulls in __O_SYNC and O_DSYNC on my system.)
	orq $0x1000,   %rsi  # O_DSYNC=4096 (0x1000)
	orq $0x100000, %rsi  # O_SYNC=1048576 (0x100000)
0:

	# Append?
	testq $0x02, %rdi
	jz 0f
1:
	orq $0x400,   %rsi  # O_APPEND=1024 (0x400)
0:

	# Noclobber?
	testq $0x01, %rdi
	jz 0f
1:
	orq $0x80,   %rsi  # O_EXCL=128 (0x80)
0:

	# Perform the syscall.
	#movq $0o664, %rdx  # chmod 0664 if_new_file; if needed can use ‘chmod’ syscall or use a shell callout function, or can add options if desired
	movq $0x1b4, %rdx  # chmod 0664 if_new_file; if needed can use ‘chmod’ syscall or use a shell callout function, or can add options if desired - decimal for this mode is 436
	movq %rsi, %rsi  # flags
	movq 24(%rsp), %rdi  # We made sure it was null-terminated, so it's compatible with the syscall interface.
	movq $2, %rax  # open
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_writer_open_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_open_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# We have a working file handle in %rax.
	# (Note: at this point, we have an additional resource we would need to
	# clean up.  However, immediately we get ready to return instead of handing
	# off control to where there may be exceptions elsewhere, so we don't need
	# to add to our cleanup requirements.  If we did, there are ways to do
	# that, and a convenient trick might be to add a prefix to the ‘6f’ error
	# cleanup address that performs a close on the file handle and then
	# naturally progresses to the 6f address right afterwards, and we just set
	# %r14 to that new 7f or whatever label right before 6f, rather than 6f.)

	# Build the arguments before we return back to the return continuation.

	# Fortunately, our case is simple enough that we can get by with having the
	# same callback for every object.

	movq 40(%rsp), %rdi  # Get the original return continuation.  Will eventually jump to this.
	movq %rax,     %r9   # Right before the jump to return, this will be %rdi.  This is the user data: the file handle.

	# Callbacks.
	leaq _ns_system_x86_64_linux_new_writer_close(%rip), %rsi
	leaq _ns_system_x86_64_linux_new_writer_query(%rip), %rdx
	leaq _ns_system_x86_64_linux_new_writer_write(%rip), %rcx
	movq $0, %r8

	jmp 5f  # Skip error cleanup.

6:
	# Error cleanup.  Special error handler.
	#
	# Just cleanup aapropriately and return to the previous error handler.
	#
	# Remember, we now have:
	# 	%rdi: Numeric code.
	# 	%rsi: String size.
	# 	%rdx: String.
	# 	%rcx: 0.
	# 1) Copy the regular cleanup except for these 4 parameters.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	movq 0(%rsp), %r10
	movq 8(%rsp), %r11
	addq $16, %rsp
	movq 8(%rsp), %rax
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
	jmp _ns_system_x86_64_linux_exit_custom
	nop
5:
	# Cleanup.

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	# Restore the working storage units and the stack, except don't preserve
	# the arguments we'll pass to the return continuation, as we specified:
	# 	- %rdi
	# 	- %rsi
	# 	- %rdx
	# 	- %rcx
	# 	- %r8
	# 	- %r9

	#movq 0(%rsp), %r9
	#movq 8(%rsp), %r8
	addq $16, %rsp

	#movq 0(%rsp), %rcx
	#movq 8(%rsp), %rdx
	addq $16, %rsp

	#movq 0(%rsp), %rsi
	#movq 8(%rsp), %rdi
	addq $16, %rsp

	movq 0(%rsp), %r10
	movq 8(%rsp), %r11
	addq $16, %rsp

	#movq 0(%rsp), %rcx
	movq 8(%rsp), %rax
	addq $16, %rsp

	# Return.
	xchgq %rdi, %r9
	jmpq *%r9
	nop

_ns_system_x86_64_linux_new_writer_close:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Backup ‘return’.
	movq %rdi, %rdx

	# Perform the ‘close’ syscall.
	movq %rsi, %rdi  # int fd
	movq $0x3, %rax  # close
	syscall

	# Verify and restore ‘return’.
	movq %rax, %rsi

	movq %rdx, %rax  # Restore ‘return’.

	leaq ns_system_x86_64_linux_err_msg_new_writer_close_close_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_close_close_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Return.
	jmpq *%rax
	nop
_ns_system_x86_64_linux_new_writer_query:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# TODO
	nop
	hlt
_ns_system_x86_64_linux_new_writer_write:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# TODO
	nop
	hlt

# Create a new reader, an instance capable of reading from a filepath.
#
# ‘new_reader’ will call the provided return continuation with multiple
# callbacks (see the tree).  You must call exactly one of these callbacks, at
# which point the original callbacks are considered consumed and must not be
# re-used since they have been destroyed, but you must instead only use any new
# callbacks that are provided.  Most callbacks will return a new set of
# callbacks with the same or with a compatible API.  The closing callbacks do
# not return a new set of callbacks.  (Responsibility for managing resources is
# not dealt with here, as this is meant to be a low-level API.)
#
# Symlinks are followed except for noclobber.
#
# This tree may be easier to parse if you configure your vim to visually
# display tabs and spaces differently, as I have.  Tabs are used for
# indentation, and spaces for alignment here.
#
# Note that read actions require memory to work (it needs to put what it reads
# somewhere) and lets you handle how to manage it.  You will need to provide a
# pointer to to the read operation action to the beginning of working storage
# available exclusively for that ‘read’ call, and it must have size at least
# the number of bytes requested to be read (or otherwise specified by the
# operation's description).
#
# Parameters:
# 	%rdi: Return with a single-use-only set (consumes the entire set at that
# 	      level) of callbacks, with parameters:
# 		%rdi: User data as part of a returned closure.  You pass it to %rsi or
# 		      any other callback so that it has data to work with.  Under the
# 		      hood, this is just the file descriptor.
# 		%rsi: Close callback to destroy the resource and discard any pending
# 		      reads or writes, with parameters:
# 			%rdi: Return.
# 			%rsi: User data.
# 		%rdx: Query status callback, with parameters:
# 			%rdi: Return, with parameters:
# 				[%rdi, %rsi, …, %r8]: user data and set of new callbacks
# 				                      corresponding to the original
# 				                      [%rdi, %rsi, …, %r8] arguments.  Don't
# 				                      use the old ones, because by now they are
# 				                      already consumed (although the new ones
# 				                      may refer to the same address).
# 				%r9: zero if not ready for writing, 1 if ready.
# 			%rsi: User data.
# 		%rcx: Callback to attempt to read, optionally with a timeout, with
# 		      parameters:
# 			%rdi: Return on non-fatal error (basically EAGAIN; means it would
# 			      block and is not available for the write or read), with
# 			      parameters:
# 				[%rdi, %rsi, …, %r8]: user data and set of new callbacks
# 				                      corresponding to the original
# 				                      [%rdi, %rsi, …, %r8] arguments.  Don't
# 				                      use the old ones, because by now they are
# 				                      already consumed (although the new ones
# 				                      may refer to the same address).
# 				%r9: status code: 0 in the general case, 1 if the resource is
# 				     exhausted (e.g. we've detected that it's closed or
# 				     otherwise won't be available for any reads in the
# 				     future, e.g. all a file has been comprehensively read), as
# 				     a platform-specific convenience hint.
# 			%rsi: Return on success, with parameters:
# 				[%rdi, %rsi, …, %r8]: user data and set of new callbacks
# 				                      corresponding to the original
# 				                      [%rdi, %rsi, …, %r8] arguments.  Don't
# 				                      use the old ones, because by now they are
# 				                      already consumed (although the new ones
# 				                      may refer to the same address).
# 				%r9: Number of bytes read.
# 			%rdx: tuple callback to get options and timeout, with parameters
# 			      (we use a tuple instead of direct parameters to compress
# 			      parameters into fewer parameters (3 into 2) so we don't
# 			      depend on a stack as much):
# 				%rdi: Return (please return back to this address when done),
# 				      with parameters:
# 					%rdi: Options bitfield: bit 0 is boolean for 0 to disable
# 					      timeout, 1 to enable timeout.
# 					%rsi: Seconds for the timeout (i64).
# 					%rdx: Nanoseconds for the timeout (i64).
# 					%rcx: Please set to 0, to make future enhancements more
# 					      convenient.
# 				%rsi: User data.
# 			%rcx: user data supplied to %rdx as a closure
# 			%r8: Tuple for remaining read parameters, with parameters:
# 				%rdi: Return (please return back to this address when done),
# 				      with parameters:
# 					%rdi: Number of bytes to request reading, and also the
# 					      maximum number of bytes to read.
# 					%rsi: As noted, memory for a successful read to output to.
# 					%rdx: Please set to 0, so that future versions can add
# 					      functionality more conveniently.
# 				%rsi: User data.
# 			%r9: User data to provide to the tuple continuation.
# 		%r8: currently 0, in case future versions want to add functionality.
# 		     (If the API is extended with APIs, I'd probably make the next
# 		     addition a tuple to contain, e.g. set %r8 to 1 and then %r9
# 		     becomes the tuple callback (don't worry, %rdi is already available
# 		     as the user data).)
# 	%rsi: Size of filepath.
# 	%rdx: Pointer to filepath.  Null-terminate it for the kernel.
# 	%rcx: Options bitfield:
# 		Bit 3: incognito: NOATIME: request concealing the access via the ATIME
# 		                  attribute.
# 		Bit 4: fallback: use more default bits in case the syscall flags is
# 		                 giving the kernel issues.
# 		Bit 5: shared: don't enable O_COEXEC, so that child processes made
# 		               through ‘exec’ to replace an old process already have
# 		               this file descriptor opened too, that is it is
# 		               inherited.
# 		Rest: set to 0.
# 	%r8: Please set to 0, so that future versions can accept an extra
# 	     parameter if needed, e.g. for I/O options.
#
# This clobbers all parameter registers, because they are passed to the return
# continuation, and one extra working storage unit is needed to hold the return
# continuation itself:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
#
# (Since the stack is used, this preserves (but would otherwise clobber) all
# syscall registers (%rax, %rcx, %r11, %r10) and parameter registers (%rdi,
# %rsi, %rdx, %rcx, %r8, %r9), except the parameter registers used for the
# callback:)
# 	- %rax
# 	- %rcx
# 	- %r11
# 	- %r10 (syscall parameter)
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
#
# This action requires the stack in order for its memory requirements.
.global ns_system_x86_64_linux_new_reader
.set ns_system_x86_64_linux_new_reader, (_ns_system_x86_64_linux_new_reader - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_new_reader:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# First backup the input arguments and what we clobber onto the stack.

	subq $16, %rsp
	movq %rax, 8(%rsp)
	movq %rcx, 0(%rsp)

	subq $16, %rsp
	movq %r11, 8(%rsp)
	movq %r10, 0(%rsp)

	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)

	subq $16, %rsp
	movq %rdx, 8(%rsp)
	movq %rcx, 0(%rsp)

	subq $16, %rsp
	movq %r8, 8(%rsp)
	movq %r9, 0(%rsp)

	# Backup %r15 and %r14.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	leaq 6f(%rip), %r14

	# Next just perform a few checks.

	# Make sure %r8 is 0.
	testq %r8, %r8
	jz 0f
	# Error: invalid arguments.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_reader_r8_not_zero_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Now we can clear %r8, discard it and use it for our own purposes, since
	# we can restore it to $0.
	movq %r8, %r8

	# Make sure unsupported bits in the options bitfield are 0, to aid in
	# future enhancements.
	testq $0xFFFFFFFFFFFFFFC7, %rcx
	jz 0f
	# Error: invalid arguments.

	# First, write to this instance's error message storage to print the
	# exit code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_u_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_print_u64
9:
	nop

	# Error.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_unsupported_options(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Ensure the filepath is null-terminated.
	movq 24(%rsp), %rdx  # Data.
	movq 32(%rsp), %rsi  # Size.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_is_null_terminated
9:
	testq %rdi, %rdi
	jnz 0f
1:
	# Error: the filepath is not null-terminated.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_writer_path_not_null_terminated_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Calculate the flags, into %rsi.
	movq $0, %rsi  # Clear flags.
	movq 16(%rsp), %rdi  # Original %rcx, options bitfield.

	# Base options.
	# (This is a reader; don't worry about O_CREAT.)
	orq $0x800, %rsi  # |= (O_NONBLOCK=2048 (0x800))
	orq $0x1,   %rsi  # |= (O_WRONLY=1 (0x1))

	# (not) Shared?
	testq $0x20, %rdi
	jnz 0f
1:
	orq $0x80000, %rsi  # O_CLOEXEC=524288 (0x80000)
0:

	# (not) Fallback?
	testq $0x10, %rdi
	jnz 0f
1:
	orq $0x8000, %rsi  # O_LARGEFILE=32768 (0x8000)
0:

	# Incognito?
	testq $0x08, %rdi
	jz 0f
1:
	orq $0x1000000, %rsi  # O_NOATIME=16777216 (0x1000000)
0:

	# Perform the syscall.
	#movq $0o664, %rdx  # chmod 0664 if_new_file; if needed can use ‘chmod’ syscall or use a shell callout function, or can add options if desired
	movq $0x1b4, %rdx  # chmod 0664 if_new_file; if needed can use ‘chmod’ syscall or use a shell callout function, or can add options if desired - decimal for this mode is 436
	movq %rsi, %rsi  # flags
	movq 24(%rsp), %rdi  # We made sure it was null-terminated, so it's compatible with the syscall interface.
	movq $2, %rax  # open
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_reader_open_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_open_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# We have a working file handle in %rax.
	# (Note: at this point, we have an additional resource we would need to
	# clean up.  However, immediately we get ready to return instead of handing
	# off control to where there may be exceptions elsewhere, so we don't need
	# to add to our cleanup requirements.  If we did, there are ways to do
	# that, and a convenient trick might be to add a prefix to the ‘6f’ error
	# cleanup address that performs a close on the file handle and then
	# naturally progresses to the 6f address right afterwards, and we just set
	# %r14 to that new 7f or whatever label right before 6f, rather than 6f.)

	# Build the arguments before we return back to the return continuation.

	# Fortunately, our case is simple enough that we can get by with having the
	# same callback for every object.

	movq 40(%rsp), %rdi  # Get the original return continuation.  Will eventually jump to this.
	movq %rax,     %r9   # Right before the jump to return, this will be %rdi.  This is the user data: the file handle.

	# Callbacks.
	leaq _ns_system_x86_64_linux_new_reader_close(%rip), %rsi
	leaq _ns_system_x86_64_linux_new_reader_query(%rip), %rdx
	leaq _ns_system_x86_64_linux_new_reader_read(%rip),  %rcx
	movq $0, %r8

	jmp 5f  # Skip error cleanup.

6:
	# Error cleanup.  Special error handler.
	#
	# Just cleanup aapropriately and return to the previous error handler.
	#
	# Remember, we now have:
	# 	%rdi: Numeric code.
	# 	%rsi: String size.
	# 	%rdx: String.
	# 	%rcx: 0.
	# 1) Copy the regular cleanup except for these 4 parameters.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	movq 0(%rsp), %r10
	movq 8(%rsp), %r11
	addq $16, %rsp
	movq 8(%rsp), %rax
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
	jmp _ns_system_x86_64_linux_exit_custom
	nop
5:
	# Cleanup.

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	# Restore the working storage units and the stack, except don't preserve
	# the arguments we'll pass to the return continuation, as we specified:
	# 	- %rdi
	# 	- %rsi
	# 	- %rdx
	# 	- %rcx
	# 	- %r8
	# 	- %r9

	#movq 0(%rsp), %r9
	#movq 8(%rsp), %r8
	addq $16, %rsp

	#movq 0(%rsp), %rcx
	#movq 8(%rsp), %rdx
	addq $16, %rsp

	#movq 0(%rsp), %rsi
	#movq 8(%rsp), %rdi
	addq $16, %rsp

	movq 0(%rsp), %r10
	movq 8(%rsp), %r11
	addq $16, %rsp

	#movq 0(%rsp), %rcx
	movq 8(%rsp), %rax
	addq $16, %rsp

	# Return.
	xchgq %rdi, %r9
	jmpq *%r9
	nop

_ns_system_x86_64_linux_new_reader_close:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Backup ‘return’.
	movq %rdi, %rdx

	# Perform the ‘close’ syscall.
	movq %rsi, %rdi  # int fd
	movq $0x3, %rax  # close
	syscall

	# Verify and restore ‘return’.
	movq %rax, %rsi

	movq %rdx, %rax  # Restore ‘return’.

	leaq ns_system_x86_64_linux_err_msg_new_writer_close_close_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_close_close_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Return.
	jmpq *%rax
	nop
_ns_system_x86_64_linux_new_reader_query:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# TODO
	nop
	hlt
_ns_system_x86_64_linux_new_reader_read:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# TODO
	nop
	hlt

# TODO

# ################################################################
# Shell calls.
# ################################################################

# TODO

# ################################################################
# Exiting and process maangement.
# ################################################################

# Exit with a successful status code.
#
# Parameters:
# 	(None.)
.global ns_system_x86_64_linux_exit_success
.set ns_system_x86_64_linux_exit_success, (_ns_system_x86_64_linux_exit_success - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_exit_success:
	movq $60, %rax  # exit
	movq $0, %rdi
	syscall

	jmp _ns_system_x86_64_linux_exit_success
	nop

.global ns_system_x86_64_linux_exit_failure
.set ns_system_x86_64_linux_exit_failure, (_ns_system_x86_64_linux_exit_failure - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_exit_failure:
	movq $60, %rax  # exit
	movq $1, %rdi
	syscall

	jmp _ns_system_x86_64_linux_exit_failure
	nop

# Exit with a non-zero code and error message (or 0 with success and _no_ error
# message).
#
# Parameters:
# 	%rdi: Error code.
# 	%rsi: Error message size.
# 	%rdx: Error message.
# 	%rcx: 0 if non-fatal, 1 if fatal and %r14 can be used if %r14 is enabled.
# 	      Likely you'll want to set this to ‘0’.  (As explained in the module
# 	      description, bit 1 in %r15 enables if enabled %r14 to override the
# 	      default.)
#
# (Note: this brings into play Linux's blocking mechanisms.)
#
# (TODO: allow callbacks and greater configurability for such errors, to let
# higher levels cleanup and such.)
#
# TODO: handle success too, which is *not* an exception!
.global ns_system_x86_64_linux_exit_custom
.set ns_system_x86_64_linux_exit_custom, (_ns_system_x86_64_linux_exit_custom - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_exit_custom:
	# Before anything else, check if it's successful (zero code) or a failure
	# (non-zero code).
	testq %rdi, %rdi
	jnz 0f
1:
	# It's success.
	jmp _ns_system_x86_64_linux_exit_success
	nop
	hlt
0:
	# It's an error.

	# Fatal?
	testq %rcx, %rcx
	jz 0f
1:
	# Fatal, *or* non-fatal but the default error handler is used, which is the
	# error handler for fatal errors.
	movq %rdi, %r10  # Back up the original %rdi, the original exit code.

	#movq %rsi, %rdx  # Would do it like this, but just simplest to xchngq.
	#movq %rdx, %rsi  # %rdx would be clobbered.
	xchgq %rsi, %rdx
	movq $2, %rdi
	movq $1, %rax  # write
	syscall

	# Ignore the exit status of ‘write’; ignore any errors here; we're going to
	# exit anyway.

	movq %r10, %rdi  # Restore the original %rdi, the exit code.

	movq $60, %rax  # exit
	movq %rdi, %rdi
	syscall

	nop
	hlt
	jmp 1b
	nop
	hlt
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:
	# Non-fatal.

	# First, check if we've enabled a non-default error / non-zero-exit handler.
	testq $0x2, %r15
	jz 1b
0:

	# We have a non-default error handler.

	# So instead we'll call %r14 with parameters:
	# 	%rdi: Numeric code.
	# 	%rsi: String size.
	# 	%rdx: String.
	# 	%rcx: 0.
	movq %rdx, %rdx
	movq %rsi, %rsi
	movq %rdi, %rdi
	movq $0,   %rcx
	jmp *%r14
	nop
	hlt

# ################################################################
# Misc.
# ################################################################

# Return to the last function on the call stack.
#
# OLD variant:
#
# This clobbers %rdi.
#
# If you have the shadow stack enabled on your system, you will need to use
# ‘net; nop’ variant instead, which doesn't clobber %rdi.
.global ns_system_x86_64_linux_cc_call_stack_return
.set ns_system_x86_64_linux_cc_call_stack_return, (_ns_system_x86_64_linux_cc_call_stack_return - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_cc_call_stack_return:
	ret
	nop
	#movq (%rsp), %rdi
	#addq $8, %rsp
	#jmpq *%rdi
	#nop

# clock_nanosleep the given number of nanoseconds against the CLOCK_MONOTONIC=1
# clock.
#
# Parameters:
# 	%rdi: Return.
# 	%rsi: i64 seconds.
# 	%rdx: i64 nanoseconds.
#
# Note: this depends on a working stack and uses stack space to give the kernel
# a place to store timespec output we need.
#
# For convenience, since the stack is used, it is also used to preserve all
# registers.  (Not required, since conventionally the registers we use here are
# all caller-preserved.)
#
# This clobbers nothing.
.global ns_system_x86_64_linux_monotonic_nanosleep
.set ns_system_x86_64_linux_monotonic_nanosleep, (_ns_system_x86_64_linux_monotonic_nanosleep - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_monotonic_nanosleep:
	# timespec is a pair of signed longs: seconds and nanoseconds.

	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Backup %rax.
	subq $16, %rsp
	# 8(%rsp): padding.
	movq %rax, 0(%rsp)

	# Backup %rcx and %r11.
	subq $16, %rsp
	movq %rcx, 8(%rsp)
	movq %r11, 0(%rsp)

	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)

	# Backup %rdx and %r10.
	subq $16, %rsp
	movq %rdx, 8(%rsp)
	movq %r10, 0(%rsp)

	# Backup %r8.
	subq $16, %rsp
	# 8(%rsp): padding.
	movq %r8, 0(%rsp)

	# Get non-register working storage units for the request timespec struct
	# too.
	subq $16, %rsp
	movq %rdx, 8(%rsp)  # i64 nanoseconds.
	movq %rsi, 0(%rsp)  # i64 seconds.
	movq %rsp, %r8      # Request.

	# Clobber buffer cushion.
	subq $16, %rsp

	# Reserve space for the syscall's time remaining output.
	subq $16, %rsp
	movq %rsp, %r10  # Track struct to hold remaining sleep.

	# Backup %r15 and %r14.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	leaq 6f(%rip), %r14

1:
	# Perform the syscall.
	movq %r10, %r10  # struct timespec *remain
	movq %r8, %rdx  # const struct timespec *request
	movq $0, %rsi  # int flags = 0
	movq $1, %rdi  # clockid_t clockid = CLOCK_MONOTONIC
	movq $230, %rax  # clock_nanosleep
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_clock_nanosleep(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_clock_nanosleep_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# The syscall returns 0 when it's successfully completed the duration of
	# sleep.  Otherwise it was interrupted and we'll need to resume the sleep.
	#
	# So check %rax.
	testq %rax, %rax
	jz 0f

	# Okay, copy ‘remain’ into ‘request’ and resume the sleep.
	movq 0(%r10), %rdi
	movq 8(%r10), %rsi
	movq %rdi, 0(%r8)
	movq %rsi, 8(%r8)

	jmp 1b
0:

	jmp 5f  # Skip error cleanup.

6:
	# Error cleanup.  Special error handler.
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
	addq $16, %rsp
	addq $16, %rsp
	movq 0(%rsp), %r8
	addq $16, %rsp
	movq 0(%rsp), %r10
	#movq 8(%rsp), %rdx
	addq $16, %rsp
	#movq 0(%rsp), %rsi
	#movq 8(%rsp), %rdi
	addq $16, %rsp
	movq 0(%rsp), %r11
	#movq 8(%rsp), %rcx
	addq $16, %rsp
	movq 0(%rsp), %rax
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
	jmp _ns_system_x86_64_linux_exit_custom
	nop

5:
	# Cleanup.

	# Restore stack and storage units.

	# Clear time remaining space.
	addq $16, %rsp

	# Pop the clobber buffer.
	addq $16, %rsp

	# Pop request.
	addq $16, %rsp

	# Restore %r8.
	# 8(%rsp): padding.
	movq 0(%rsp), %r8
	addq $16, %rsp

	# Restore %r10 and %rdx.
	movq 0(%rsp), %r10
	movq 8(%rsp), %rdx
	addq $16, %rsp

	# Restore %rsi and %rdi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Restore %r11 and %rcx.
	movq 0(%rsp), %r11
	movq 8(%rsp), %rcx
	addq $16, %rsp

	# Restore %rax.
	# 8(%rsp): padding.
	movq 0(%rsp), %rax
	addq $16, %rsp

	# Return.
	jmpq *%rdi
	nop

# Utility to write a u64 in ascii.
#
# Right padded.
#
# Parameters:
# 	%rdi: Return with arguments:
# 		%rdi: Number of written digits printed to encode the number, excluding writen padding and leftover digits.
# 		%rsi: Number of digits or characters (bytes) written or skipped, which here should be equal to the input size.
# 		%rdx: Number of digits leftover for which there was not enough space to print
# 	%rsi: Pointer to memory to write ascii digits to; start address.
# 	%rdx: Size of the memory region for writing the string.
# 	%rcx: The u64 to print.
#
# This clobbers %r11, %rax, %r10, and all argument registers:
# 	- %r11
# 	- %rax
# 	- %r10
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
.global ns_system_x86_64_linux_print_u64
.set ns_system_x86_64_linux_print_u64, (_ns_system_x86_64_linux_print_u64 - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_print_u64:
	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)
	# Validate implicit parameters.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_validate_implicit_arguments
9:
	nop
	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	movq %rdx, %r11  # Size.

	#movq %rsi, %rsi  # Base.
	movq %rdx, %r10  # Current size.
	movq %rcx, %r8   # Current dividend.

5:
	# Break if out of space.
	testq %r10, %r10
	jz 4f
	dec %r10

	# Divide by 10 to get the least significant digit.
	movq $0, %rdx
	movq %r8, %rax
	movq $10, %r9
	divq %r9
	# mod (remainder) is %rdx, quotient is %rax.
	# Use %rdx to write another digit, starting from the right.
	addq $'0, %rdx
	addq %r10, %rsi
	movb %dl, (%rsi)
	subq %r10, %rsi
	# The next dividend will be %rax.
	movq %rax, %r8

	# Loop back unless %r8 is 0.
	testq %r8, %r8
	jnz 5b
4:

	# Discard the original dividend and use this storage unit for the return
	# continuation.
	movq %rdi, %rcx

	# %r11 - %r10, that is, size - current size, tells us the number of printed
	# digits.
	movq %r10, %rdi
	subq %r11, %rdi

	# %r11 is the number of digits skipped or written (original size).
	movq %r11, %rsi

	# Now just do a shorter loop to see how many iterations we need before we
	# get to 0.  Find %rdx; we'll count with %r10 before putting 5r10 into %rdx.
	movq $0, %r10
5:
	# Already finished dividing the number to 0?
	testq %r8, %r8
	jz 4f

	# Nope, it's non-zero.  Increment number of divisions (digits), divide, and
	# try again.
	inc %r10

	# Divide by 10 to get the least significant digit.
	movq $0, %rdx
	movq %r8, %rax
	movq $10, %r9
	divq %r9
	# mod (remainder) is %rdx, quotient is %rax.
	# Use %rdx to write another digit, starting from the right.
	#addq $'0, %rdx
	# The next dividend will be %rax.
	movq %rax, %r8

	jmp 5b
4:
	movq %r10, %rdx

	# Return.
	movq %rcx, %rcx
	jmpq *%rcx
	nop

# Simple utility to verify a syscall was successful or else fail with an error
# message.
# 	%rdi: return
# 	%rsi: the errno to check (syscall return %rax value, actually)
# 	%rdx: size of the error message
# 	%rcx: an error message to print after our prefix of ‘Error (errno %d): ’
#
# If this procedure action returns with no error detected, no storage units are
# clobbered.
.global ns_system_x86_64_linux_verify_errno
.set ns_system_x86_64_linux_verify_errno, (_ns_system_x86_64_linux_verify_errno - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_verify_errno:
	# Return normally if the syscall return value is not in [-4095, -1] (https://stackoverflow.com/a/2538212).
	cmpq $-4095, %rsi  # if %rsi < -4095
	jl   0f            # return

	cmpq $-1, %rsi  # if %rsi > -1
	jg   0f         # return

	# We have an error.  Prepare the message.
	xchgq %rdx, %rcx  # Just 'cause I swapped %rdx and %rcx in the docs and
	                  # adding this instruction is convenient.
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
	jna 8f
	movq %rdx, %rcx  # %rcx <- …_size + 20
8:

	# Append the error message.
	addq $20, %rdi
7:
	# Break if size reaches 0.# or we reach a null byte.
	testq %rcx, %rcx
	jz 6f
	#movq (%rdi), %rdx
	#testq %rdx, %rdx
	#jz 6f

	movb (%r8), %r9b
	movb %r9b, (%rdi)
	dec %rcx
	inc %rdi
	inc %r8

	jmp 7b
6:

	# Encode the error code (%rsi) into offsets 13 through 17 of %rdi.
	not %rsi
	inc %rsi  # %rsi = -%rsi

	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rdi
	addq $13, %rdi  # Base.
	movq $4, %rcx  # Size.
	movq %rsi, %r8  # Current dividend.
5:
	# Break if out of space.
	testq %rcx, %rcx
	jz 4f
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
	jnz 5b
4:

	# Hand over control to exit_custom().
	movq $0, %rcx
	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rdx
	leaq ns_system_x86_64_linux_syscall_verify_builder_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $3, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt

0:
	# Return normally.
	jmpq *%rdi
	nop

# Utility to determine whether a string is null-terminated.
#
# Parameters:
# 	%rdi: Return, with parameters:
# 		%rdi: 0 if not null-terminated, 1 if null-terminated.
# 		%rsi: if null-terminated, what the offset is to the null byte.
# 	%rsi: Size of data to scan.
# 	%rdx: Data to scan (pointer to a string).
#
# This clobbers %rdi, %rsi, %rdx, %rcx, and %r8.
.global ns_system_x86_64_linux_is_null_terminated
.set ns_system_x86_64_linux_is_null_terminated, (_ns_system_x86_64_linux_is_null_terminated - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_is_null_terminated:
	# Back up original size.
	movq %rsi, %r8

1:
	# Finished scanning without a detection?
	testq %rsi, %rsi
	jz 0f

	# Null byte detected here?
	movq (%rdx), %rcx
	testq %rcx, %rcx
	jnz 2f
3:
	# Not a null byte here; keep scanning.
	dec %rsi
	inc %rdx
2:
	# We found a null byte here.
	subq %rsi, %r8
	movq %r8, %rsi  # Got offset.
	movq $1, %rdx  # 1: it's null-terminated.
	xchgq %rdx, %rdi
	jmpq *%rdx
0:
	# No null byte found.
	movq $0, %rsi
	xchgq %rsi, %rdi
	jmpq *%rsi
	nop

# Make sure %r15 and %r14 are at least as specified.
#
# (Just check the first 3 bytes of %r15.)
#
# You can normally check it like this:
# 		# Backup %rdi and %rsi.
# 		subq $16, %rsp
# 		movq %rdi, 8(%rsp)
# 		movq %rsi, 0(%rsp)
# 		# Validate implicit parameters.
# 		leaq 9f(%rip), %rdi
# 		movq $ns_system_x86_64_linux_validate_implicit_arguments, %rax
# 		jmp _system
# 	9:
# 		nop
# 		# Restore %rdi and %rsi.
# 		movq 0(%rsp), %rsi
# 		movq 8(%rsp), %rdi
# 		addq $16, %rsp
#
# Or internally in this module,
# 		# Backup %rdi and %rsi.
# 		subq $16, %rsp
# 		movq %rdi, 8(%rsp)
# 		movq %rsi, 0(%rsp)
# 		# Validate implicit parameters.
# 		leaq 9f(%rip), %rdi
# 		jmp _ns_system_x86_64_linux_validate_implicit_arguments
# 	9:
# 		nop
# 		# Restore %rdi and %rsi.
# 		movq 0(%rsp), %rsi
# 		movq 8(%rsp), %rdi
# 		addq $16, %rsp
#
# Note: if the check fails, the error will be fatal, so cleanup not needed on a
# fatal crash (e.g. you used the stack to backup %rdi and %rsi, so it's okay to
# leave the stack pointer at an unknown but valid location so it cannot be
# unwound) can be skipped.
#
# Parameters:
# 	%rdi: Return.
#
# Clobbers:
# 	%rsi:
# 		to save us extra instruction dependencies, since the architecture
# 		supports testq and cmpq with 32-bit immediates but not 64-bit
# 		immediates, we just use %rsi to hold our constants.
.global ns_system_x86_64_linux_validate_implicit_arguments
.set ns_system_x86_64_linux_validate_implicit_arguments, (_ns_system_x86_64_linux_validate_implicit_arguments - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_validate_implicit_arguments:
	# Check the first 7 bytes are the magic code:
	# 	0xF4 0x0D 0xCA 0x88  0x48 0x2D 0x41 (0xXX)
	# (Inverted:)
	# 	0x0B 0xF2 0x35 0x77  0xB7 0xD2 0xBE (0xXX)
	#testq $0x0BF23577B7D2BE00, %r15  # Test no inverted bits are set (‘printf "0x%02X\n" $ complement (0x03 :: Word8)’).
	movq $0x0BF23577B7D2BE00, %rsi
	testq %rsi, %r15
	jnz 1f

	# Now do unsigned below and above checks to test it's:
	# 	- > 0xF40DCA88482D41FF, ||
	# 	- < 0xF40DCA88482D4100
	#cmpq $0xF40DCA88482D41FF, %r15
	movq $0xF40DCA88482D41FF, %rsi
	cmpq %rsi, %r15
	ja 1f

	#cmpq $0xF40DCA88482D4100, %r15
	movq $0xF40DCA88482D4100, %rsi
	cmpq %rsi, %r15
	jb 1f

	jmp 0f
1:
	# Error: invalid implicit arguments!
	#
	# Make this a fatal error so that exit_custom doesn't try to look at the
	# implicit paramaters.  Redundantly, on top of this, back up the original
	# %r15 and %r14 and set our own implicit parameters to one that specifies
	# the default (fatal) error handler.
	movq %r15, %r9
	movq %r14, %r8
	movq $0xF40DCA88482D4100, %r15
	leaq _ns_system_x86_64_linux_exit_custom(%rip), %r14
	movq $1, %rcx
	leaq ns_system_x86_64_linux_err_msg_invalid_implicit_args(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_invalid_implicit_args_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:
	# Tests passed.

	# Return.
	jmp *%rdi
	nop

# An optional segv trap handler that hands off execution to what was statically
# set in ‘ns_system_x86_64_linux_sigaction_static_set_segv_cont’ and
# ‘ns_system_x86_64_linux_sigaction_static_set_segv_data’
ns_system_x86_64_linux_trap_middleman:
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_data(%rip), %rdi
	movq (%rdi), %rdi
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_cont(%rip), %rsi
	movq (%rsi), %rsi
	jmpq *%rsi
	nop

	# Redundantly after a jump (never executed), adapt to the signal handler's
	# expectation of a conventional return.
	movq (%rsp), %rdi
	addq $8, %rsp
	jmpq *%rdi
	nop

# Call a callback on segfault.  Only use this once per process instantiation.
#
# Note this is called before self-modifying-code support is verified.
# 	%rdi: return
# 	%rsi: callback continuation, where to jump.
# 	%rdx: callback pointer / user data.
.global ns_system_x86_64_linux_trap_segv
.set ns_system_x86_64_linux_trap_segv, (_ns_system_x86_64_linux_trap_segv - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_trap_segv:
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
	# Set the first 8 bytes in our static data.
	leaq ns_system_x86_64_linux_trap_middleman(%rip), %rdi
	leaq ns_system_x86_64_linux_sigaction_static_set_segv(%rip), %rsi
	movq %rdi, (%rsi)
	#movq $ns_system_x86_64_linux_trap_middleman, (%rsi)

	# rt_sigaction(…)
	movq $11, %rdi  # signum for SEGV is 11.  (INT is 2, and USR1 is 10.)
	leaq ns_system_x86_64_linux_sigaction_static_set_segv(%rip), %rsi
	movq $0, %rdx
	movq $8, %r10  # sizeof sigset_t: 128, but we'll just use 0.  Weird, disas glibc .so has this set to 8, and that made it progress further.  Whatever, so it's 8, not 0 or 128.
	movq $13, %rax  # rt_sigaction  (x32 has a different number.)
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_segv_trap_set(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_segv_trap_set_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (If this doesn't exit, it doesn't clobber %r8.)
9:
	nop

	# TODO: FIXME: have:
	# 	5:
	# 		jmp 5b
	# here (optionally also at the beginning of trap_middleman()), adjust
	# SIGSEGV1 to USR1, then start ahc, then ‘pkill -USR1 ahc’, and then ahc
	# will segfault.  It seems correct to me: glibc seems to do it as I do and
	# set it up here (objdump -d libc.so), and a C ‘rt_sigaction’ experiment
	# with a null-initialized buffer except the first 8 bytes point to the
	# handler worked on my sistem.  I don't know why this thing is segfaulting.
	# Oh well, we can worry about this later.  Until this gets fixed, the user
	# will see a segfault instead of an error message if code sections are
	# unwritable.

	# Return.
	movq %r8, %rdi
	jmpq *%rdi
	nop

# Restore default segfault trap behaviour, but override any previous
# non-default setting with the default handler.
# 	%rdi: return
.global ns_system_x86_64_linux_restore_trap_segv
.set ns_system_x86_64_linux_restore_trap_segv, (_ns_system_x86_64_linux_restore_trap_segv - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_restore_trap_segv:
	# Backup the continuation.
	movq %rdi, %r8

	# rt_sigaction(…)
	movq $11, %rdi  # signum for SEGV is 11.
	leaq ns_system_x86_64_linux_sigaction_restore_segv(%rip), %rsi
	movq $0, %rdx
	movq $8, %r10  # sizeof sigset_t: 128, but we'll just use 0.  Weird, disas glibc .so has this set to 8, and that made it progress further.  Whatever, so it's 8, not 0 or 128.
	movq $13, %rax  # rt_sigaction  (x32 has a different number.)
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_segv_trap_restore(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_segv_trap_restore_size(%rip), %rcx
	movq (%rcx), %rcx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Return.
	movq %r8, %rdi
	jmpq *%rdi
	nop

.global ns_system_x86_64_linux_module_end
ns_system_x86_64_linux_module_end:
