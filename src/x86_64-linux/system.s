# This module provides procedures to interface with the kernel through
# syscalls.

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
# 			       for more information).  However, the ‘.*_force’ exit methods
# 			       can override these settings.  This setting applies to
# 			       non-zero exit codes.
# 			Bit 2: For a non-forced, standard successful exit, also call the
# 			       %r14 callback to cleanup resources; success and failure can
# 			       be dicriminated by checking the exit code.
# 			Bit 3: Unused.
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
# 			%rdi: Numeric code (non-zero is a failure; zero is a success).
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

# A string constant used by ‘get_platform_name’.
ns_system_x86_64_linux_platform_name_size:
	.quad (ns_system_x86_64_linux_platform_name_end - ns_system_x86_64_linux_platform_name)
ns_system_x86_64_linux_platform_name:
	.ascii "x86_64-linux"
	.byte 0x00
ns_system_x86_64_linux_platform_name_end:

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
	.quad 22
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

ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdin_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdin_end - ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdin)
ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdin:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: the ‘close’ syscall failed!  Failed to close the stdin child end on the parent.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdin_end:

ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdout_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdout_end - ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdout)
ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdout:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: the ‘close’ syscall failed!  Failed to close the stdout child end on the parent.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdout_end:

ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stderr_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stderr_end - ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stderr)
ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stderr:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: the ‘close’ syscall failed!  Failed to close the stderr child end on the parent.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stderr_end:

ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdin_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdin_end - ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdin)
ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdin:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: the ‘close’ syscall failed!  Failed to close the stdin parent end on the child.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdin_end:

ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdout_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdout_end - ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdout)
ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdout:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: the ‘close’ syscall failed!  Failed to close the stdout parent end on the child.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdout_end:

ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stderr_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stderr_end - ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stderr)
ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stderr:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: the ‘close’ syscall failed!  Failed to close the stderr parent end on the child.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stderr_end:

ns_system_x86_64_linux_err_msg_shell_child_start_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_start_failed_end - ns_system_x86_64_linux_err_msg_shell_child_start_failed)
ns_system_x86_64_linux_err_msg_shell_child_start_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: the ‘exec’ syscall failed!  Failed to start the command from the child.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_start_failed_end:

ns_system_x86_64_linux_err_msg_shell_child_exec_returned_on_success_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_exec_returned_on_success_end - ns_system_x86_64_linux_err_msg_shell_child_exec_returned_on_success)
ns_system_x86_64_linux_err_msg_shell_child_exec_returned_on_success:
	.ascii "Error: shell error: the ‘exec’ syscall returned without error!  This shouldn't happen.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_exec_returned_on_success_end:

ns_system_x86_64_linux_err_msg_shell_child_close_stdin_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_close_stdin_end - ns_system_x86_64_linux_err_msg_shell_child_close_stdin)
ns_system_x86_64_linux_err_msg_shell_child_close_stdin:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to close stdin on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_close_stdin_end:

ns_system_x86_64_linux_err_msg_shell_child_close_stdout_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_close_stdout_end - ns_system_x86_64_linux_err_msg_shell_child_close_stdout)
ns_system_x86_64_linux_err_msg_shell_child_close_stdout:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to close stdout on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_close_stdout_end:

ns_system_x86_64_linux_err_msg_shell_child_close_stderr_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_close_stderr_end - ns_system_x86_64_linux_err_msg_shell_child_close_stderr)
ns_system_x86_64_linux_err_msg_shell_child_close_stderr:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to close stderr on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_close_stderr_end:

ns_system_x86_64_linux_err_msg_shell_child_dup2_stdin_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_dup2_stdin_end - ns_system_x86_64_linux_err_msg_shell_child_dup2_stdin)
ns_system_x86_64_linux_err_msg_shell_child_dup2_stdin:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to dup2 stdin on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_dup2_stdin_end:

ns_system_x86_64_linux_err_msg_shell_child_dup2_stdout_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_dup2_stdout_end - ns_system_x86_64_linux_err_msg_shell_child_dup2_stdout)
ns_system_x86_64_linux_err_msg_shell_child_dup2_stdout:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to dup2 stdout on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_dup2_stdout_end:

ns_system_x86_64_linux_err_msg_shell_child_dup2_stderr_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_dup2_stderr_end - ns_system_x86_64_linux_err_msg_shell_child_dup2_stderr)
ns_system_x86_64_linux_err_msg_shell_child_dup2_stderr:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to dup2 stderr on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_dup2_stderr_end:

ns_system_x86_64_linux_err_msg_shell_child_close_post_stdin_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_close_post_stdin_end - ns_system_x86_64_linux_err_msg_shell_child_close_post_stdin)
ns_system_x86_64_linux_err_msg_shell_child_close_post_stdin:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to close stdin (post) on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_close_post_stdin_end:

ns_system_x86_64_linux_err_msg_shell_child_close_post_stdout_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_close_post_stdout_end - ns_system_x86_64_linux_err_msg_shell_child_close_post_stdout)
ns_system_x86_64_linux_err_msg_shell_child_close_post_stdout:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to close stdout (post) on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_close_post_stdout_end:

ns_system_x86_64_linux_err_msg_shell_child_close_post_stderr_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_child_close_post_stderr_end - ns_system_x86_64_linux_err_msg_shell_child_close_post_stderr)
ns_system_x86_64_linux_err_msg_shell_child_close_post_stderr:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "shell error: failed to close stderr (post) on the child!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_child_close_post_stderr_end:

ns_system_x86_64_linux_err_msg_shell_invalid_argument_command_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_invalid_argument_command_end - ns_system_x86_64_linux_err_msg_shell_invalid_argument_command)
ns_system_x86_64_linux_err_msg_shell_invalid_argument_command:
	.ascii "Error: shell error: invalid arguments: the last command argument should be null-terminated but wasn't!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_invalid_argument_command_end:

ns_system_x86_64_linux_err_msg_shell_invalid_argument_environment_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_invalid_argument_environment_end - ns_system_x86_64_linux_err_msg_shell_invalid_argument_environment)
ns_system_x86_64_linux_err_msg_shell_invalid_argument_environment:
	.ascii "Error: shell error: invalid arguments: the last environment item should be null-terminated but wasn't!  Could not execute a shell or system command.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_invalid_argument_environment_end:

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

# (Naming clarification: ‘new_writer’'s ‘.write’ action's ‘write’ syscall
# failed.)
ns_system_x86_64_linux_err_msg_new_writer_write_write_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_write_write_failed_end - ns_system_x86_64_linux_err_msg_new_writer_write_write_failed)
ns_system_x86_64_linux_err_msg_new_writer_write_write_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_writer’ ‘.write’ error: the ‘write’ syscall failed!  Could not request a write.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_write_write_failed_end:

ns_system_x86_64_linux_err_msg_new_writer_write_write_timeout_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_write_write_timeout_end - ns_system_x86_64_linux_err_msg_new_writer_write_write_timeout)
ns_system_x86_64_linux_err_msg_new_writer_write_write_timeout:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_writer’ ‘.write’ error: the ‘write’ syscall timed out!  Failed to request a write.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_write_write_timeout_end:

ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_failed_end - ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_failed)
ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_writer’ ‘.write’ error: the ‘clock_gettime’ syscall failed (start sample)!  Could not request a write.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_failed_end:

ns_system_x86_64_linux_err_msg_new_writer_write_select_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_write_select_failed_end - ns_system_x86_64_linux_err_msg_new_writer_write_select_failed)
ns_system_x86_64_linux_err_msg_new_writer_write_select_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_writer’ ‘.write’ error: the ‘select’ syscall failed!  Could not request a write.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_write_select_failed_end:

ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_second_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_second_failed_end - ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_second_failed)
ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_second_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_writer’ ‘.write’ error: the ‘clock_gettime’ syscall failed (end sample)!  Could not request a write.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_second_failed_end:

ns_system_x86_64_linux_err_msg_new_reader_read_read_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_read_read_failed_end - ns_system_x86_64_linux_err_msg_new_reader_read_read_failed)
ns_system_x86_64_linux_err_msg_new_reader_read_read_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_reader’ ‘.read’ error: the ‘read’ syscall failed!  Could not request a read.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_read_read_failed_end:

ns_system_x86_64_linux_err_msg_new_reader_read_read_timeout_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_read_read_timeout_end - ns_system_x86_64_linux_err_msg_new_reader_read_read_timeout)
ns_system_x86_64_linux_err_msg_new_reader_read_read_timeout:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_reader’ ‘.read’ error: the ‘read’ syscall timed out!  Failed to request a read.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_read_read_timeout_end:

ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_failed_end - ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_failed)
ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_reader’ ‘.read’ error: the ‘clock_gettime’ syscall failed (start sample)!  Could not request a read.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_failed_end:

ns_system_x86_64_linux_err_msg_new_reader_read_select_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_read_select_failed_end - ns_system_x86_64_linux_err_msg_new_reader_read_select_failed)
ns_system_x86_64_linux_err_msg_new_reader_read_select_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_reader’ ‘.read’ error: the ‘select’ syscall failed!  Could not request a read.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_read_select_failed_end:

ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_second_failed_size:
	.quad (ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_second_failed_end - ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_second_failed)
ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_second_failed:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘new_reader’ ‘.read’ error: the ‘clock_gettime’ syscall failed (end sample)!  Could not request a read.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_second_failed_end:

ns_system_x86_64_linux_err_msg_shell_invalid_options_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_invalid_options_end - ns_system_x86_64_linux_err_msg_shell_invalid_options)
ns_system_x86_64_linux_err_msg_shell_invalid_options:
	.ascii "Error: ‘shell’ error: invalid options bitfield!  Please make sure the arguments to this action are correct.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_invalid_options_end:

ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdin_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdin_end - ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdin)
ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdin:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘shell’ error: failed to create a pipe for stdin!  The ‘pipe2’ syscall failed.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdin_end:

ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdout_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdout_end - ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdout)
ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdout:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘shell’ error: failed to create a pipe for stdout!  The ‘pipe2’ syscall failed.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdout_end:

ns_system_x86_64_linux_err_msg_shell_pipe_pair_stderr_size:
	.quad (ns_system_x86_64_linux_err_msg_shell_pipe_pair_stderr_end - ns_system_x86_64_linux_err_msg_shell_pipe_pair_stderr)
ns_system_x86_64_linux_err_msg_shell_pipe_pair_stderr:
	# (Will get an ‘Error: ’-like prefix.)
	.ascii "‘shell’ error: failed to create a pipe for stderr!  The ‘pipe2’ syscall failed.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_shell_pipe_pair_stderr_end:

# For our compiler, we only require the ability to read and write files and to
# exit.  We can work out the rest.  But we also add some concurrency support
# and shell support.  Also add a few other utilities as needed.
.text

# ################################################################
# Base.
# ################################################################

# Get a platform name string for whatever is running this binary (that is, what
# is, at the time of your running the application (not our building it), what
# is the ‘native’, not ‘host’ or ‘target’ part of it).
#
# This module already is platfom-specific, so this is easy.  It' sconstant.
#
# Parameters:
# 	%rdi: Return, with parameters:
# 		%rdi: The size of the platform name string.
# 		%rsi: A pointer to the beginning of the platform name string.  It also
# 		      happens to be null-terminated.  This null terminator is part of
# 		      the string returned.
#
# This clobbers the following registers:
# 	- %rdi
# 	- %rsi
# 	- %rdx
.global ns_system_x86_64_linux_get_platform_name
.set ns_system_x86_64_linux_get_platform_name, (_ns_system_x86_64_linux_get_platform_name - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_get_platform_name:
	# Back up the return address to %rdx.
	movq %rdi, %rdx

	# Set up arguments and return.
	leaq ns_system_x86_64_linux_platform_name(%rip), %rsi
	movq ns_system_x86_64_linux_platform_name_size(%rip), %rdi
	jmpq *%rdx
	nop

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
	nop

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
# Note: unless it's a vfork, the child process has reset exit handler settings
# (%r15 bits; see module implicit parameters docs); it has a new stack.
#
# Parameters:
# 	%rdi: Return with arguments:
# 		%rdi: joiner: continuation callback with the following parameters:
# 			%rdi: Return after joining, blocking and waiting until the thread finishes.  (And fail with an error instead if the joined thread failed with an error.)
# 			%rsi: User data, to make this callback a closure.
# 		%rsi: user data that must be provided as the second argument (%rsi) to the joiner.
# 	%rsi: Where the new executor starts executing.
# 	%rdx: Options bitfield:
# 		Bit 0: vfork: special fork pattern where the fork will exit or execve
# 		       and the stack is shared; use the vfork syscall instead of the
# 		       fork syscall, and see the documentation for this.
# 		(Rest must be 0; this may or may not be checked.)
# 	%rcx:
# 		Offered ‘price’, where a higher price may be more likely to result in
# 		allocation if resources are scarce.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
# 	- %rax
# 	- %r11
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
	movq %rcx, %rcx

	# Clear out the first 32 bits of the PID, which we store as the user data.
	movq $0, %rax

	# Backup input arguments.
	movq %rdi, %r8
	#movq %rsi, %r9  # Just use xchngq in the Verify step instead.  Saves us a register.

	# Back up option flags.
	movq %rdx, %r9

	# Fork.  (If vfork bit, then vfork instead.)
	testq $0x1, %rdx
	jz 1f
	nop
	movq $58, %rax  # vfork
	jmp 0f
	nop
1:
	movq $57, %rax  # fork  #  (First 32 bits are 0.)
0:
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
	# We're the child, but right before we go to the child continuation, first,
	# if it's not a vfork, then reset the custom error handlers (disable the
	# flags in %r15), since we have a new stack; from a perspective of linear
	# types, forking then has an implicit ‘dup’ as far as used resources are
	# concerned, unless it's ‘vfork’.
	movq  %r9,  %rdx  # Restore original %rdx (options bitfield).
	testq $0x1, %rdx  # Test for ‘vfork’.
	jnz 8f
	nop
	andq $0xFFFFFFFFFFFFFFF9, %r15  # Disable bits 1 and 2.
8:
	jmpq *%rsi  # Now jump to the child continuation.
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
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_offset(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo(%rip), %rdi
	addq %rdi, %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_size(%rip), %rsi
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
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_offset(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code(%rip), %rdi
	addq %rdi, %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_size(%rip), %rsi
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
	movl 56(%rsp), %edi  # (actually, not status/errno (offset 4), but si_status (offset 24))
	testq %rdi, %rdi
	jz 0f
1:
	# A thread failed.

	# First, write to this instance's error message storage to print the
	# exit code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_offset(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_size(%rip), %rsi
	movq (%rsi), %rsi
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure(%rip), %rdi
	addq %rdi, %rdx
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
# Note that in the returned API implementation, some methods such as the write
# action accept multiple parameters, and rather than requiring a stack is how
# memory is managed, we use tuples and nested callbacks to embed this
# information.  A callback should return the desired component (1st component,
# 3rd component, or whatever is specified) when called.  So the tuple can hold
# many arguments, and tuples can be nested.
#
# Note: the ‘.write’ action implementation currently itself uses a stack to
# perform its procedure.
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
# 			%rdi: Return on non-fatal error (basically EAGAIN timeout; means it
# 			      would block and is not available for the write or read), but
# 			      only if the options bitfield enables this handler rather than
# 			      returning an error on timeout, with parameters:
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
# 			%rdx: User data (under the hood, internally, this is the FD).
# 			%rcx: tuple callback to get options and timeout, with parameters
# 			      (we use a tuple instead of direct parameters to compress
# 			      parameters into fewer parameters (3 into 2) so we don't
# 			      depend on a stack as much):
# 				%rdi: Return (please return back to this address when done),
# 				      with parameters:
# 					%rdi: Options bitfield:
# 						Bit 0: enable timeout: 1 to enable timeout, 0 to block
# 						       indefinitely.
# 						Bit 1: enable user-handled timeout would-block (this
# 						       doesn't apply to other I/O errors), so that your
# 						       non-fatal error handler you supplied through
# 						       %rdi is returned to instead of failing with an
# 						       error.
# 					%rsi: Seconds for the timeout (i64).
# 					%rdx: Nanoseconds for the timeout (i64).
# 					%rcx: Nested tuple (accessor function) to store still more
# 					      arguments, with parameters:
# 						%rdi: Return (please return back to this address when
# 						      done), with parameters:
# 							%rdi: Number of bytes to request a write for (i.e.,
# 							      the size.)
# 							%rsi: The data to request a write for (i.e., the
# 							      data).
# 						%rdi: User data.
# 					%r8: User data supplied to %rcx as a closure.  (Doesn't
# 					     have to be an FD.)
# 					%r9: Please set to 0, to make future enhancements more
# 					     convenient.
# 				%rsi: User data.
# 			%r8: User data supplied to %rcx as a closure.  (You can pass
# 			     whatever you want; it doesn't have to be an FD.)
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
	leaq ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_u_offset(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_writer_unsupported_options_u_size(%rip), %rsi
	movq (%rsi), %rsi
	leaq ns_system_x86_64_linux_err_msg_new_writer_unsupported_options(%rip), %rdi
	addq %rdi, %rdx
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
	movq 40(%rsp), %rdx  # Data.
	movq 48(%rsp), %rsi  # Size.
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
	movq 32(%rsp), %rdi  # Original %rcx, options bitfield.

	# Base options.
	orq $0x40,  %rsi  # |= (O_CREAT=64 (0x40))
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
	movq 40(%rsp), %rdi  # We made sure it was null-terminated, so it's compatible with the syscall interface.
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

	movq 56(%rsp), %rdi  # Get the original return continuation.  Will eventually jump to this.
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

	# Backup %r15 and %r14.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Elapsed seconds waitn  (4232)
	movq $0, 0(%rsp)    # Elapsed nanosecs watn  (4224)

	# ‘fd sets’: the kernel seems to use 1024 bytes, divided unto u64s, for a
	# single FD set.  (We'll reserve twice this for cushion.)  Then it's just
	# bitfield: fd 0 corresponds to bit 0, fd 1 to bit 1, fd 2 to bit 2, etc.,
	# e.g. if only fds 3 and 7 are enabled, the set is 0b000…000010001000=136.

	# So we'll reserve 2 2048-byte FD_SETs.  One is used for NULL input sets,
	# with all bits cleared (fds disabled).  The other one we'll use to provide
	# our own FD set for select.

	# All-null FD set:
	#                   #                        (4224)
	subq $2048, %rsp    # All-null FD set        (2176)

	# All-null FD set:
	subq $2048, %rsp    # User FD set            ( 128)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # select timeout musecs   (120)
	movq $0, 0(%rsp)    # select timeout secs     (112)

	# Backup %rdi and %rsi
	subq $16, %rsp
	movq %rdi, 8(%rsp)  # Enbld?  Timeout callbk  (104)
	movq %rsi, 0(%rsp)  # Normal return on succe   (96)

	# Backup %rdx and %rcx
	subq $16, %rsp
	movq %rdx, 8(%rsp)  # User data (intrnly FD)   (88)
	movq %rcx, 0(%rsp)  # Tuple.                   (80)

	# Backup %r8 and %r9
	subq $16, %rsp
	movq %r8, 8(%rsp)   # Tuple user data.         (72)
	movq %r9, 0(%rsp)   #                          (64)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Options.                 (56)
	movq $0, 0(%rsp)    # Timeout seconds.         (48)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Timeout nanoseconds.     (40)
	movq $0, 0(%rsp)    # Size of data to write.   (32)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Data to write.           (24)
	movq $0, 0(%rsp)    #                          (16)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Space for clock_gettime  ( 8)
	movq $0, 0(%rsp)    # Space for clock_gettime  ( 0)

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	leaq 6f(%rip), %r14

	# Zero-init both FD sets.
	movq $4096, %rsi
	leaq 128(%rsp), %rdi
1:
	subq $8, %rsi
	testq %rsi, %rsi
	jz 0f
	movq $0, (%rdi)
	addq $8, %rdi
	jmp 1b
0:

	# For the user FD set, set the bit corresponding to our FD in the original
	# %rdx.
	leaq 128(%rsp), %rdi  # user FD set start.
	movq  88(%rsp), %rsi  # fd (original %rdx)
	movq %rsi, %rdx
	# Can divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	# But just shift and ‘and’ instead, since it's multiples of 2.  (shrq is
	# unsigned.)
	shrq $3, %rdx
	addq %rdx, %rdi  # Get the right byte.
	# Get the right bit within the byte.
	movq %rsi, %rcx
	andq $7, %rcx
	movq $1, %r8
	shlq %cl, %r8
	# Set the right bit within the byte.
	orb %r8b, (%rdi)

	# Last 5 bits.

	# TODO check original %r8 is 0.

	# Prepare to do the write.

	# Retrieve 2 values from the nested tuple callback, to get the size and
	# data pointer.

	# Call outer tuple.
	movq 80(%rsp), %rcx  # Restore original %rcx.
	movq 72(%rsp), %r8   # Restore original %r8.
	movq %r8, %rsi
	leaq 9f(%rip), %rdi
	jmp *%rcx
9:
	nop

	# TODO check %r9 is0.

	# While we're at it, just track options, seconds, and nanoseconds, before
	# we get into the inner tuple.
	movq %rdi, 56(%rsp)  # Options.
	movq %rsi, 48(%rsp)  # Seconds.
	movq %rdx, 40(%rsp)  # Nanoseconds.

	# Call inner tuple.
	movq %r8, %rsi
	leaq 9f(%rip), %rdi
	jmp *%rcx
9:
	nop

	# Get size and data start.
	movq %rdi, 32(%rsp)  # Size.
	movq %rsi, 24(%rsp)  # Data.

2:
	# Do the write.
	movq 32(%rsp), %rdx  # size_t size
	movq 24(%rsp), %rsi  # const uint8_t *data
	movq 88(%rsp), %rdi  # int fd (original %rdx)
	movq $1,       %rax  # write
	syscall

	# Check for EAGAIN=EWOULDBLOCK (11) with %rax of -11, in which case we skip verification.
	cmpq $-11, %rax
	jz 8f

1:
	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_write_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_write_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop
	jmp 0f  # Go to successful write block.
8:
	# The resource is not currently available for a write.

	# Reserve and initialize %r8 and %r9 as monotonic time elapsed waiting for
	# availability; seconds and nanoseconds.
	movq $0, %r8
	movq $0, %r9

	jmp 3f
	nop

	# 4 begins the timeout block.
4:
	# A timeout occurred.  We'll be returning either to a user-provided handler
	# if enabled or otherwise throwing an error.
	testq $0x2, 56(%rsp)  # Enable user-handled timeout would-block callback?
	jnz 7f
	nop
	# Not enabled.  Just error.
	movq $-11, %rax  # EAGAIN=11
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_write_timeout(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_write_timeout_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop  # Will not be reached.
	jmp _ns_system_x86_64_linux_exit_failure
	nop
	hlt
7:
	# Enabled.  Go back to the user-provided handler, then.
	movq $0,     %r9   # Status: 0 in general case, 1 if exhausted.  Just set to 0.  (Probably EDQUOT quotas would be most meaningful here in the documented man page returns, but just this to 0.)
	movq $0, %r8   # %r8 is specified to be 0.
	leaq _ns_system_x86_64_linux_new_writer_write(%rip), %rcx  # Write request callback.
	leaq _ns_system_x86_64_linux_new_writer_query(%rip), %rdx  # Query status callback.
	leaq _ns_system_x86_64_linux_new_writer_close(%rip), %rsi  # Close callback.
	movq 88(%rsp), %rdi  # Original %rdi, user data (fd).
	movq 104(%rsp), %r11  # Original %rdi.
	jmp 5f

	# 3 begins the loop of attempting a write if not yet timed out.
3:
	# So first check whether we are in a timeout condition, where we had a
	# timeout enabled and time elapsed exceeded the timeout.
	testq $0x1, 56(%rsp)
	jz 8f  # jump if timeout not enabled, then treat timeout as infinite.
	nop
	movq 4232(%rsp), %r8  # Elapsed seconds.
	movq 4224(%rsp), %r9  # Elapsed nanoseconds.
	cmpq 48(%rsp), %r8
	jae 4b  # if elapsed >= timeout, then do a timeout.
	nop
	cmpq 40(%rsp), %r9
	jae 4b  # if elapsed >= timeout, then do a timeout.
	nop
8:

	# Just wait until we can write.
#0:
	# First sample the current time.
	leaq 0(%rsp), %rsi  # Output storage; secs&nanoseconds i64 pair.
	movq $1,   %rdi  # clockid_t clockid: CLOCK_MONOTONIC=1
	movq $228, %rax  # clock_gettime
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Setup timeout data.
	#
	# Null-initialize timeout to %r8, which the syscall treats as an infinite
	# timeout.
	movq $0, %r8

	# Timeout enabled?
	testq $0x1, 56(%rsp)
	jz 8f  # jump if timeout not enabled, then treat timeout as infinite.
	nop
	leaq  112(%rsp), %r8   # timeout
8:

	# Set the timeout values to original timeout minus elapsed.
	movq 4232(%rsp), %rdx  # Get elapsed seconds.
	movq 4224(%rsp), %rcx  # Get elapsed nanoseconds.
	movq   48(%rsp), %rdi  # Get original timeout seconds.
	movq   40(%rsp), %rsi  # Get original timeout nanoseconds.
	subq %rdx, %rdi  # Remaining timeout seconds.
	subq %rcx, %rsi  # Remaining timeout nanoseconds.
	# Handle remaining nanoseconds < 0 or >= 1000000000.
	movq $0, %rdx
	movq %rsi, %rax
	movq $1000000000, %rcx
	divq %rcx  # Divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	movq %rdx, %rsi
	addq %rax, %rdi
	cmpq $0, %rsi
	jge 8f
	nop
	addq $1000000000, %rsi
	decq %rdi
8:
	# Now %rsi is set up.
	# %rsi /= 1000
	movq $0, %rdx
	movq %rsi, %rax
	movq $1000, %rcx
	divq %rcx  # Divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	movq %rax, %rsi  # Now microseconds.
	movq %rdi, 112(%rsp)  # Seconds.
	movq %rsi, 120(%rsp)  # Microseconds.

	# (Note: ‘select’ works with seconds and microseconds, not seconds and
	# nanoseconds.)
	movq %r8,        %r8   # timeout
	leaq 2176(%rsp), %rcx  # exceptfds
	leaq  128(%rsp), %rdx  # writefds
	leaq 2176(%rsp), %rsi  # readfds
	movq   88(%rsp), %rdi
	incq %rdi              # int nfds: highest internal fd plus 1.
	movq $23, %rax         # select
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_select_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_select_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Successful ‘select’.

	# Add to time elapsed and loop back to try a write again!

	# Backup begin clock sample.
	movq 0(%rsp), %r8  # Seconds.
	movq 8(%rsp), %r9  # Microseconds.

	# Sample the current time.
	leaq 0(%rsp), %rsi  # Output storage; secs&nanoseconds i64 pair.
	movq $1,   %rdi  # clockid_t clockid: CLOCK_MONOTONIC=1
	movq $228, %rax  # clock_gettime
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_second_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_writer_write_clock_gettime_second_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# end - begin into %r8 and %r9 for this frame's elapsed time.
	movq 0(%rsp), %rdi
	movq 8(%rsp), %rsi
	subq %r8, %rdi
	subq %r9, %rsi
	movq %rdi, %r8
	movq %rsi, %r9  # Now we have elapsed time, but we'll want to double check bounds.
	movq %r8, %rdi
	movq %r9, %rsi
	# Handle remaining nanoseconds < 0 or >= 1000000000.
	movq $0, %rdx
	movq %rsi, %rax
	movq $1000000000, %rcx
	divq %rcx  # Divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	movq %rdx, %rsi
	addq %rax, %rdi
	cmpq $0, %rsi
	jge 8f
	nop
	addq $1000000000, %rsi
	decq %rdi
8:
	# Now %rsi is set up.
	# Just ensure too that %rdi >= 0.
	cmpq $0, %rdi
	jge 8f
	movq $0, %rdi
8:
	# So now we have time elapsed for this frame.  Before we loop back, add it
	# to the total, cumulative time elapsed waiting for availability.
	addq 4232(%rsp), %rdi
	addq 4224(%rsp), %rsi

	# Handle remaining nanoseconds < 0 or >= 1000000000.
	movq $0, %rdx
	movq %rsi, %rax
	movq $1000000000, %rcx
	divq %rcx  # Divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	movq %rdx, %rsi
	addq %rax, %rdi
	cmpq $0, %rsi
	jge 8f
	nop
	addq $1000000000, %rsi
	decq %rdi
8:
	# Now %rsi is set up.
	# Just ensure too that %rdi >= 0.
	cmpq $0, %rdi
	jge 8f
	movq $0, %rdi
8:

	# Now write the results.
	addq %rdi, 4232(%rsp)
	addq %rsi, 4224(%rsp)

	# Loop back to try a write again.
	jmp 2b
	nop

0:
	# The write was successful, although it might have been partial.
	# The number of bytes successfully written is in %rax.
	#
	# So prepare for our return to *%r11 by setting up the arguments.
	movq %rax,     %r9   # Number of bytes written.
	movq $0, %r8   # %r8 is specified to be 0.
	leaq _ns_system_x86_64_linux_new_writer_write(%rip), %rcx  # Write request callback.
	leaq _ns_system_x86_64_linux_new_writer_query(%rip), %rdx  # Query status callback.
	leaq _ns_system_x86_64_linux_new_writer_close(%rip), %rsi  # Close callback.
	movq 88(%rsp), %rdi  # Original %rdi, user data (fd).
	movq 96(%rsp), %r11  # Original %rsi, successfull callback.

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
	addq $2048, %rsp
	addq $2048, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
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
	jmp _ns_system_x86_64_linux_exit_custom
	nop
5:
	# Cleanup.

	# Restore space for elapsed time.
	addq $16, %rsp

	# Restore both 2048-byte FD sets.
	addq $2048, %rsp
	addq $2048, %rsp

	# Restore space for use.
	addq $16, %rsp

	# Restore space for use.
	addq $16, %rsp

	# Restore space for use.
	addq $16, %rsp

	# Restore space for use.
	addq $16, %rsp

	# Restore space for use.
	addq $16, %rsp

	# (Leave all argument registers as we set them.  We could potentially
	# return back all 6, and we leave the return continuation address in the
	# 6th argument, %r11.)

	# Restore %r8 and %r9.
	#movq 0(%rsp), %r9
	#movq 8(%rsp), %r8
	addq $16, %rsp

	# Restore %rdx and %rcx.
	#movq 0(%rsp), %rcx
	#movq 8(%rsp), %rdx
	addq $16, %rsp

	# Restore %rdi and %rsi.
	#movq 0(%rsp), %rsi
	#movq 8(%rsp), %rdi
	addq $16, %rsp

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	# Return.
	jmpq *%r11
	nop

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
# Note: the ‘.read’ action implementation currently itself uses a stack to
# perform its procedure.
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
# 			%rdx: User data (under the hood, internally, this is the FD).
# 			%rcx: tuple callback to get options and timeout, with parameters
# 			      (we use a tuple instead of direct parameters to compress
# 			      parameters into fewer parameters (3 into 2) so we don't
# 			      depend on a stack as much):
# 				%rdi: Return (please return back to this address when done),
# 				      with parameters:
# 					%rdi: Options bitfield:
# 						Bit 0: enable timeout: 1 to enable timeout, 0 to block
# 						       indefinitely.
# 						Bit 1: enable user-handled timeout would-block (this
# 						       doesn't apply to other I/O errors), so that your
# 						       non-fatal error handler you supplied through
# 						       %rdi is returned to instead of failing with an
# 						       error.
# 					%rsi: Seconds for the timeout (i64).
# 					%rdx: Nanoseconds for the timeout (i64).
# 					%rcx: Nested tuple (accessor function) to store still more
# 					      arguments, with parameters:
# 						%rdi: Return (please return back to this address when
# 						      done), with parameters:
# 							%rdi: Number of bytes to request reading (i.e., the
# 							      size), and this is also the maximum number of
# 							      bytes to read into your output store.
# 							%rsi: As noted, memory for a successful read to
# 							      output to.
# 					%r8: User data supplied to %rcx as a closure.  (Doesn't
# 					     have to be an FD.)
# 					%r9: Please set to 0, to make future enhancements more
# 					     convenient.
# 				%rsi: User data.
# 			%r8: User data supplied to %rcx as a closure.  (You can pass
# 			     whatever you want; it doesn't have to be an FD.)
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
	testq $0xFFFFFFFFFFFFFFC0, %rcx
	jz 0f

	# Error: invalid arguments.

	# First, write to this instance's error message storage to print the
	# exit code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_u_offset(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_reader_unsupported_options_u_size(%rip), %rsi
	movq (%rsi), %rsi
	leaq ns_system_x86_64_linux_err_msg_new_reader_unsupported_options(%rip), %rdi
	addq %rdi, %rdx
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
	movq 40(%rsp), %rdx  # Data.
	movq 48(%rsp), %rsi  # Size.
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_is_null_terminated
9:
	testq %rdi, %rdi
	jnz 0f
1:
	# Error: the filepath is not null-terminated.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_path_not_null_terminated(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_new_reader_path_not_null_terminated_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Calculate the flags, into %rsi.
	movq $0, %rsi  # Clear flags.
	movq 32(%rsp), %rdi  # Original %rcx, options bitfield.

	# Base options.
	orq $0x40,  %rsi  # |= (O_CREAT=64 (0x40))
	orq $0x800, %rsi  # |= (O_NONBLOCK=2048 (0x800))
	orq $0x0,   %rsi  # |= (O_RDONLY=0 (0x0))

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
	movq 40(%rsp), %rdi  # We made sure it was null-terminated, so it's compatible with the syscall interface.
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

	movq 56(%rsp), %rdi  # Get the original return continuation.  Will eventually jump to this.
	movq %rax,     %r9   # Right before the jump to return, this will be %rdi.  This is the user data: the file handle.

	# Callbacks.
	leaq _ns_system_x86_64_linux_new_reader_close(%rip), %rsi
	leaq _ns_system_x86_64_linux_new_reader_query(%rip), %rdx
	leaq _ns_system_x86_64_linux_new_reader_read(%rip), %rcx
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

	leaq ns_system_x86_64_linux_err_msg_new_reader_close_close_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_close_close_failed_size(%rip), %rdx
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

	# Backup %r15 and %r14.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Elapsed seconds waitn  (4232)
	movq $0, 0(%rsp)    # Elapsed nanosecs watn  (4224)

	# ‘fd sets’: the kernel seems to use 1024 bytes, divided unto u64s, for a
	# single FD set.  (We'll reserve twice this for cushion.)  Then it's just
	# bitfield: fd 0 corresponds to bit 0, fd 1 to bit 1, fd 2 to bit 2, etc.,
	# e.g. if only fds 3 and 7 are enabled, the set is 0b000…000010001000=136.

	# So we'll reserve 2 2048-byte FD_SETs.  One is used for NULL input sets,
	# with all bits cleared (fds disabled).  The other one we'll use to provide
	# our own FD set for select.

	# All-null FD set:
	#                   #                        (4224)
	subq $2048, %rsp    # All-null FD set        (2176)

	# All-null FD set:
	subq $2048, %rsp    # User FD set            ( 128)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # select timeout musecs   (120)
	movq $0, 0(%rsp)    # select timeout secs     (112)

	# Backup %rdi and %rsi
	subq $16, %rsp
	movq %rdi, 8(%rsp)  # Enbld?  Timeout callbk  (104)
	movq %rsi, 0(%rsp)  # Normal return on succe   (96)

	# Backup %rdx and %rcx
	subq $16, %rsp
	movq %rdx, 8(%rsp)  # User data (intrnly FD)   (88)
	movq %rcx, 0(%rsp)  # Tuple.                   (80)

	# Backup %r8 and %r9
	subq $16, %rsp
	movq %r8, 8(%rsp)   # Tuple user data.         (72)
	movq %r9, 0(%rsp)   #                          (64)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Options.                 (56)
	movq $0, 0(%rsp)    # Timeout seconds.         (48)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Timeout nanoseconds.     (40)
	movq $0, 0(%rsp)    # Size of data to read.    (32)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Data read output pointr  (24)
	movq $0, 0(%rsp)    #                          (16)

	# Reserve space for working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)    # Space for clock_gettime  ( 8)
	movq $0, 0(%rsp)    # Space for clock_gettime  ( 0)

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	leaq 6f(%rip), %r14

	# Zero-init both FD sets.
	movq $4096, %rsi
	leaq 128(%rsp), %rdi
1:
	subq $8, %rsi
	testq %rsi, %rsi
	jz 0f
	movq $0, (%rdi)
	addq $8, %rdi
	jmp 1b
0:

	# For the user FD set, set the bit corresponding to our FD in the original
	# %rdx.
	leaq 128(%rsp), %rdi  # user FD set start.
	movq  88(%rsp), %rsi  # fd (original %rdx)
	movq %rsi, %rdx
	# Can divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	# But just shift and ‘and’ instead, since it's multiples of 2.  (shrq is
	# unsigned.)
	shrq $3, %rdx
	addq %rdx, %rdi  # Get the right byte.
	# Get the right bit within the byte.
	movq %rsi, %rcx
	andq $7, %rcx
	movq $1, %r8
	shlq %cl, %r8
	# Set the right bit within the byte.
	orb %r8b, (%rdi)

	# Last 5 bits.

	# TODO check original %r8 is 0.

	# Prepare to do the read.

	# Retrieve 2 values from the nested tuple callback, to get the size and
	# data pointer.

	# Call outer tuple.
	movq 80(%rsp), %rcx  # Restore original %rcx.
	movq 72(%rsp), %r8   # Restore original %r8.
	movq %r8, %rsi
	leaq 9f(%rip), %rdi
	jmp *%rcx
9:
	nop

	# TODO check %r9 is0.

	# While we're at it, just track options, seconds, and nanoseconds, before
	# we get into the inner tuple.
	movq %rdi, 56(%rsp)  # Options.
	movq %rsi, 48(%rsp)  # Seconds.
	movq %rdx, 40(%rsp)  # Nanoseconds.

	# Call inner tuple.
	movq %r8, %rsi
	leaq 9f(%rip), %rdi
	jmp *%rcx
9:
	nop

	# Get size and data start.
	movq %rdi, 32(%rsp)  # Size.
	movq %rsi, 24(%rsp)  # Data.

2:
	# Do the read.
	movq 32(%rsp), %rdx  # size_t size
	movq 24(%rsp), %rsi  # const uint8_t *data
	movq 88(%rsp), %rdi  # int fd (original %rdx)
	movq $0,       %rax  # read
	syscall

	# Check for EAGAIN=EWOULDBLOCK (11) with %rax of -11, in which case we skip verification.
	cmpq $-11, %rax
	jz 8f

1:
	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_read_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_read_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop
	jmp 0f  # Go to successful read block.
8:
	# The resource is not currently available for a read.

	# Reserve and initialize %r8 and %r9 as monotonic time elapsed waiting for
	# availability; seconds and nanoseconds.
	movq $0, %r8
	movq $0, %r9

	jmp 3f
	nop

	# 4 begins the timeout block.
4:
	# A timeout occurred.  We'll be returning either to a user-provided handler
	# if enabled or otherwise throwing an error.
	testq $0x2, 56(%rsp)  # Enable user-handled timeout would-block callback?
	jnz 7f
	nop
	# Not enabled.  Just error.
	movq $-11, %rax  # EAGAIN=11
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_read_timeout(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_read_timeout_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop  # Will not be reached.
	jmp _ns_system_x86_64_linux_exit_failure
	nop
	hlt
7:
	# Enabled.  Go back to the user-provided handler, then.
	movq $0,     %r9   # Status: 0 in general case, 1 if exhausted.  Just set to 0.  (Probably EDQUOT quotas would be most meaningful here in the documented man page returns, but just this to 0.)
	movq $0, %r8   # %r8 is specified to be 0.
	leaq _ns_system_x86_64_linux_new_reader_read(%rip),  %rcx  # Read request callback.
	leaq _ns_system_x86_64_linux_new_reader_query(%rip), %rdx  # Query status callback.
	leaq _ns_system_x86_64_linux_new_reader_close(%rip), %rsi  # Close callback.
	movq 88(%rsp), %rdi  # Original %rdi, user data (fd).
	movq 104(%rsp), %r11  # Original %rdi.
	jmp 5f

	# 3 begins the loop of attempting a read if not yet timed out.
3:
	# So first check whether we are in a timeout condition, where we had a
	# timeout enabled and time elapsed exceeded the timeout.
	testq $0x1, 56(%rsp)
	jz 8f  # jump if timeout not enabled, then treat timeout as infinite.
	nop
	movq 4232(%rsp), %r8  # Elapsed seconds.
	movq 4224(%rsp), %r9  # Elapsed nanoseconds.
	cmpq 48(%rsp), %r8
	jae 4b  # if elapsed >= timeout, then do a timeout.
	nop
	cmpq 40(%rsp), %r9
	jae 4b  # if elapsed >= timeout, then do a timeout.
	nop
8:

	# Just wait until we can read.
#0:
	# First sample the current time.
	leaq 0(%rsp), %rsi  # Output storage; secs&nanoseconds i64 pair.
	movq $1,   %rdi  # clockid_t clockid: CLOCK_MONOTONIC=1
	movq $228, %rax  # clock_gettime
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Setup timeout data.
	#
	# Null-initialize timeout to %r8, which the syscall treats as an infinite
	# timeout.
	movq $0, %r8

	# Timeout enabled?
	testq $0x1, 56(%rsp)
	jz 8f  # jump if timeout not enabled, then treat timeout as infinite.
	nop
	leaq  112(%rsp), %r8   # timeout
8:

	# Set the timeout values to original timeout minus elapsed.
	movq 4232(%rsp), %rdx  # Get elapsed seconds.
	movq 4224(%rsp), %rcx  # Get elapsed nanoseconds.
	movq   48(%rsp), %rdi  # Get original timeout seconds.
	movq   40(%rsp), %rsi  # Get original timeout nanoseconds.
	subq %rdx, %rdi  # Remaining timeout seconds.
	subq %rcx, %rsi  # Remaining timeout nanoseconds.
	# Handle remaining nanoseconds < 0 or >= 1000000000.
	movq $0, %rdx
	movq %rsi, %rax
	movq $1000000000, %rcx
	divq %rcx  # Divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	movq %rdx, %rsi
	addq %rax, %rdi
	cmpq $0, %rsi
	jge 8f
	nop
	addq $1000000000, %rsi
	decq %rdi
8:
	# Now %rsi is set up.
	# %rsi /= 1000
	movq $0, %rdx
	movq %rsi, %rax
	movq $1000, %rcx
	divq %rcx  # Divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	movq %rax, %rsi  # Now microseconds.
	movq %rdi, 112(%rsp)  # Seconds.
	movq %rsi, 120(%rsp)  # Microseconds.

	# (Note: ‘select’ works with seconds and microseconds, not seconds and
	# nanoseconds.)
	movq %r8,        %r8   # timeout
	leaq 2176(%rsp), %rcx  # exceptfds
	leaq 2176(%rsp), %rdx  # writefds
	leaq  128(%rsp), %rsi  # readfds
	movq   88(%rsp), %rdi
	incq %rdi              # int nfds: highest internal fd plus 1.
	movq $23, %rax         # select
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_select_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_select_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Successful ‘select’.

	# Add to time elapsed and loop back to try a read again!

	# Backup begin clock sample.
	movq 0(%rsp), %r8  # Seconds.
	movq 8(%rsp), %r9  # Microseconds.

	# Sample the current time.
	leaq 0(%rsp), %rsi  # Output storage; secs&nanoseconds i64 pair.
	movq $1,   %rdi  # clockid_t clockid: CLOCK_MONOTONIC=1
	movq $228, %rax  # clock_gettime
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_second_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_new_reader_read_clock_gettime_second_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# end - begin into %r8 and %r9 for this frame's elapsed time.
	movq 0(%rsp), %rdi
	movq 8(%rsp), %rsi
	subq %r8, %rdi
	subq %r9, %rsi
	movq %rdi, %r8
	movq %rsi, %r9  # Now we have elapsed time, but we'll want to double check bounds.
	movq %r8, %rdi
	movq %r9, %rsi
	# Handle remaining nanoseconds < 0 or >= 1000000000.
	movq $0, %rdx
	movq %rsi, %rax
	movq $1000000000, %rcx
	divq %rcx  # Divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	movq %rdx, %rsi
	addq %rax, %rdi
	cmpq $0, %rsi
	jge 8f
	nop
	addq $1000000000, %rsi
	decq %rdi
8:
	# Now %rsi is set up.
	# Just ensure too that %rdi >= 0.
	cmpq $0, %rdi
	jge 8f
	movq $0, %rdi
8:
	# So now we have time elapsed for this frame.  Before we loop back, add it
	# to the total, cumulative time elapsed waiting for availability.
	addq 4232(%rsp), %rdi
	addq 4224(%rsp), %rsi

	# Handle remaining nanoseconds < 0 or >= 1000000000.
	movq $0, %rdx
	movq %rsi, %rax
	movq $1000000000, %rcx
	divq %rcx  # Divide %rdx:%rax by OP into quotient %rax, remainder %rdx.
	movq %rdx, %rsi
	addq %rax, %rdi
	cmpq $0, %rsi
	jge 8f
	nop
	addq $1000000000, %rsi
	decq %rdi
8:
	# Now %rsi is set up.
	# Just ensure too that %rdi >= 0.
	cmpq $0, %rdi
	jge 8f
	movq $0, %rdi
8:

	# Now write the results.
	addq %rdi, 4232(%rsp)
	addq %rsi, 4224(%rsp)

	# Loop back to try a read again.
	jmp 2b
	nop

0:
	# The read was successful, although it might have been partial.
	# The number of bytes successfully read is in %rax.
	#
	# So prepare for our return to *%r11 by setting up the arguments.
	movq %rax,     %r9   # Number of bytes written.
	movq $0, %r8   # %r8 is specified to be 0.
	leaq _ns_system_x86_64_linux_new_reader_read(%rip),  %rcx  # Read request callback.
	leaq _ns_system_x86_64_linux_new_reader_query(%rip), %rdx  # Query status callback.
	leaq _ns_system_x86_64_linux_new_reader_close(%rip), %rsi  # Close callback.
	movq 88(%rsp), %rdi  # Original %rdi, user data (fd).
	movq 96(%rsp), %r11  # Original %rsi, successfull callback.

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
	addq $2048, %rsp
	addq $2048, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
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
	jmp _ns_system_x86_64_linux_exit_custom
	nop
5:
	# Cleanup.

	# Restore space for elapsed time.
	addq $16, %rsp

	# Restore both 2048-byte FD sets.
	addq $2048, %rsp
	addq $2048, %rsp

	# Restore space for use.
	addq $16, %rsp

	# Restore space for use.
	addq $16, %rsp

	# Restore space for use.
	addq $16, %rsp

	# Restore space for use.
	addq $16, %rsp

	# Restore space for use.
	addq $16, %rsp

	# (Leave all argument registers as we set them.  We could potentially
	# return back all 6, and we leave the return continuation address in the
	# 6th argument, %r11.)

	# Restore %r8 and %r9.
	#movq 0(%rsp), %r9
	#movq 8(%rsp), %r8
	addq $16, %rsp

	# Restore %rdx and %rcx.
	#movq 0(%rsp), %rcx
	#movq 8(%rsp), %rdx
	addq $16, %rsp

	# Restore %rdi and %rsi.
	#movq 0(%rsp), %rsi
	#movq 8(%rsp), %rdi
	addq $16, %rsp

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	# Return.
	jmpq *%r11
	nop

# TODO

# ################################################################
# Shell calls.
# ################################################################

# Interpret a string to represent a shell call.
#
# Currently, only the format of null-terminated arguments is supported: within
# the data range specified by size, each null byte is taken to terminate an
# argument.  The last argument must be null-terminated.  A standard shell call
# may look like ‘/bin/sh -c ‘echo test’’, with 3 arguments: ‘/bin/sh’, ‘-c’,
# and ‘echo test’, and the memory would have 3 null bytes after each argument.
#
# By default, a pipe is created for stdin, stdout, and stderr, and readers and
# writers with the same API as that created by ‘new_writer’ and ‘new_reader˚
# are returned; however, an option bit may be enabled to instead let the system
# / shell process inherit from the parent process in these things.
#
# A returned joiner with the same API as ‘fork_require’ creates (internally
# implemented as ‘_fork_join’) is returned.
#
# The new system shell will run in the background, and you normally should run
# the joiner after calling ‘shell’ to wait for the process to finish.
#
# To permit errors, either modify the shell statement to ignore errors, or use
# %r15 and %r14 to catch the errors you wish to ignore.
#
# Note: the ‘util’ module's wrapper may provide a slightry more user-friendly
# interface, as it defaults to automatically joining and also inheriting
# standard input and outputs pipes, but this low-level method permits more
# fine-grained control.
#
# Parameters:
# 	%rdi: Return, with parameters (too many to fit in 6, so some of them have
# 	      been put in a tuple, rather than using the stack, so you can access
# 	      more arguments than 6; the tuple is a like a function pointer paired
# 	      with arbitrary user data for the function pointer to have input or
# 	      state):
# 		%rdi: The continuation callback part of the tuple closure, to hold
# 		      extra arguments, with parameters:
# 			%rdi: User data for the tuple.
# 			%rsi: Return, with parameters:
# 				%rdi: Joiner callback return address, with the same API
# 				      (doesn't *directly* return the shell call's exit code; to
# 				      get the exit code, you'd need to either install a custom
# 				      exception handler or modify the shell statement / command
# 				      line, as noted in the procedure description docs), with
# 				      parameters (same as from ‘fork_require’:
# 					%rdi: Return after joining, blocking and waiting until the
# 					      thread finishes.  (And fail with an error instead if
# 					      the joined thread failed with an error.)
# 					%rsi: User data.  (Internally, the PID of the thread to
# 					      join to.)
# 				%rsi: stdin writer callback: pass it the stdin writer user data
# 				      to get API values, to return a tuple's contents:
# 					%rdi: Return (tuple of API methods to interface with
# 					      stdin), with parameters:
# 						[%rdi, %rsi, …, %r8]: (NON-tuple) user data and set of
# 						                      API callbacks equivalent to the
# 						                      API implementation created by
# 						                      ‘new_writer’.
# 					%rsi: User data (tuple data) fed into the stdin writer callback.
# 				%rdx: stdout reader callback: pass it the stdout reader user
# 				      data to get API values:
# 					%rdi: Return (tuple of API methods to interface with
# 					      stdout), with parameters:
# 						[%rdi, %rsi, …, %r8]: (NON-tuple) user data and set of
# 						                      API callbacks equivalent to the
# 						                      API implementation created by
# 						                      ‘new_reader’.
# 					%rsi: User data fed into the stdout reader callback.
# 				%rcx: stderr reader callback: pass it the stderr reader user
# 				      data to get API values:
# 					%rdi: Return (tuple of API methods to interface with
# 					      stderr), with parameters:
# 						[%rdi, %rsi, …, %r8]: (NON-tuple) user data and set of
# 						                      API callbacks equivalent to the
# 						                      API implementation created by
# 						                      ‘new_reader’.
# 					%rsi: User data fed into the stderr reader callback.
# 				%r8: This is currently set to 0.
# 		%rsi: The user data part of the tuple closure; pass it to %rdi as the
# 		      first argument.
# 		%rdx: The user data to be passed to the joiner callback (it's nested in
# 		      the tuple) (although it's the second parameter)
# 		%rcx: The user data to be passed to the stdin writer callback (it's
# 		      nested in the tuple), or undefined if the inherit option is
# 		      enabled.
# 		%r8: The user data to be passed to the stdout reader callback (it's
# 		     nested in the tuple), or undefined if the inherit option is
# 		     enabled.
# 		%r9: The user data to be passed to the stderr reader callback (it's
# 		     nested in the tuple), or undefined if the inherit option is
# 		     enabled.
# 	%rsi: Options bitfield:
# 		Bit 0: Specifies the encoding of the command line and environment
# 		       parameters.  Since only one format is currently supported, this
# 		       must be set to 0.
# 		Bit 1: If enabled, and Bit 2 is off, then instead of creating pipes,
# 		       just inherit from the caller's standard input, output, and error
# 		       pipes.
# 		Bit 2: If enabled, then close the shell command's stdin, stdout, and
# 		       sterr files.  Overrides bit 1.
# 		(Other bits should be 0; this may or may not be checked.)
# 	%rdx: Size of command line encoding.
# 	%rcx: Command line encoding.  (Pointer to start of it.)
# 	%r8: Size of environment encoding.
# 	%r9: Environment encoding.  (Pointer to start of it.)
#
# Clobbers:
# 	- %rax  (syscall-related, and working)
# 	- %r11  (syscall-related)
# 	- %rcx  (syscall-related)
# 	- %r10  (syscall-related)
# 	- %rdi  (parameter)
# 	- %rsi  (parameter)
# 	- %rdx  (parameter)
# 	- %rcx  (parameter)
# 	- %r8   (parameter)
# 	- %r9   (parameter)
# 	- %rip  (it does things)
.global ns_system_x86_64_linux_shell
.set ns_system_x86_64_linux_shell, (_ns_system_x86_64_linux_shell - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_shell:
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

	# First just make sure all bits in the options bitfield but Bit 1 are all
	# 0.
	testq $0xFFFFFFFFFFFFFFF9, %rsi
	jz 0f
1:
	# Error: the filepath is not null-terminated.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_invalid_options(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_shell_invalid_options_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Backup %r13.
	subq $16, %rsp
	movq %r13, 8(%rsp)  #                (152)
	movq $0,   0(%rsp)  #                (152)
	# 0(%rsp) is working storage space.  (144)

	# Backup %r15 and %r14.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	# Backup %rax and %r11
	subq $16, %rsp
	movq %rax, 8(%rsp)
	movq %r11, 0(%rsp)

	# Backup %rcx and %r10
	subq $16, %rsp
	movq %rcx, 8(%rsp)
	movq %r10, 0(%rsp)

	# Backup %rdi and %rsi
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)

	# Backup %rdx and %rcx
	subq $16, %rsp
	movq %rdx, 8(%rsp)
	movq %rcx, 0(%rsp)

	# Backup %r8 and %r9
	subq $16, %rsp
	movq %r8, 8(%rsp)
	movq %r9, 0(%rsp)

	# Working storage units for the pipe FDs.

	# stdin.
	subq $16, %rsp
	movq $0, 8(%rsp)  # Read end.
	movq $0, 0(%rsp)  # Write end.

	# stdout.
	subq $16, %rsp
	movq $0, 8(%rsp)  # Read end.
	movq $0, 0(%rsp)  # Write end.

	# stderr.
	subq $16, %rsp
	movq $0, 8(%rsp)  # Read end.
	movq $0, 0(%rsp)  # Write end.

	# We've backed up %r13.  Use it now to refer to the stack above the
	# dynamically-sized parts.  We'll restore %r13 when we're done.
	movq %rsp, %r13

	# Dynamically-sized pointer arrays for the ‘execve’ syscall.
	# How this works is the quad at %rsp refers to the size, so adding (%rsp)
	# to %rsp will get you the address to the write end of stderr, that is,
	# what would be the beginning of the stack with out.  Adding (%rsp) to %rsp
	# also will pop this dynamically-sized array.
	subq $16, %rsp
	movq $0,  8(%rsp)
	movq $16, 0(%rsp)  # Dynamic array size.

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	leaq 6f(%rip), %r14

	# Do a check on the command and environment strings.

	# Command line encoding check.
	testq %rdx, %rdx
	jz 0f  # Skip the check if the command data is empty.
	nop
1:
	movq %rdx, %rdi
	decq %rdi
	addq %rcx, %rdi
	movb (%rdi), %dil
	andq $0xFF, %rdi
	testq %rdi, %rdi
	jz 0f

	# Error: the last item in the command line encoding is not null terminated!
	# Catch this before we sent a string of unknown length to the kernel,
	# potentially resulting in unallocated or out-of-bounds memory access.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_invalid_argument_command(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_shell_invalid_argument_command_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# Environment encoding check.
	testq %r8, %r8
	jz 0f  # Skip the check if the command data is empty.
	nop
1:
	movq %r8, %rdi
	decq %rdi
	addq %r9, %rdi
	movb (%rdi), %dil
	andq $0xFF, %rdi
	testq %rdi, %rdi
	jz 0f

	# Error: the last item in the environment encoding is not null terminated!
	# Catch this before we sent a string of unknown length to the kernel,
	# potentially resulting in unallocated or out-of-bounds memory access.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_invalid_argument_environment(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_shell_invalid_argument_environment_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:
	# Done with encoding checks.
	# So restore %rdi and %rsi (redundant).
	#movq 88(%r13), %rdi  # Resore original %rdi.
	#movq 80(%r13), %rsi  # Resore original %rsi (options bitfield).

	# First, if not inheriting, create 3 pairs of pipes before forking (but set
	# up the rest of the communication after the fork).  Skip this pipe
	# creation if Bit 1 is off or if Bit 2 is on.
	testq $0x2, %rsi
	jnz 0f  # Skip if inheriting.
	nop
	testq $0x4, %rsi
	jnz 0f  # Skip if closing standard files.
	nop
1:

	# stdin
	movq $2048,    %rsi  # Flags (O_NONBLOCK=2048 (0x800)) (notably this does _not_ have O_CLOEXEC; we're about to fork and exec.)
	leaq 32(%r13), %rdi  # Fildes
	movq $293,     %rax  # pipe2 (22 is pipe)
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdin(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdin_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# stdout
	movq $2048,    %rsi  # Flags (O_NONBLOCK=2048 (0x800))
	leaq 16(%r13), %rdi  # Fildes
	movq $293,     %rax  # pipe2 (22 is pipe)
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdout(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_pair_stdout_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# stderr
	movq $2048,    %rsi  # Flags (O_NONBLOCK=2048 (0x800))
	leaq  0(%r13), %rdi  # Fildes
	movq $293,     %rax  # pipe2 (22 is pipe)
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_pair_stderr(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_pair_stderr_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

0:

	# Now in the parent cleanup, we will restore %r13.
	# But here just use it to back up the parent stack location.
	#movq 152(%r13), %r13  # We've now claimed %r13 in this procedure for a
	                       # pointer in the stack after the dynamic part, but
	                       # we will restore %r13 on cleanup.
	movq %r13, %r13

	# Now just fork, but use vfork and share the stack.
	movq $0,       %rcx  # Price.
	#movq $0x1,     %rdx  # Options bitfield; vfork instead of fork.  (If using
	                      # vfork-style, be sure to update the close syscall
	                      # error handlers below.)
	movq $0x0,     %rdx  # Options bitfield.
	leaq 1f(%rip), %rsi  # Child return.
	leaq 9f(%rip), %rdi  # Return.
	jmp _ns_system_x86_64_linux_fork_require
9:
	nop

	jmp 0f
1:
	# Child executor.

	movq %r13, %r13  # Parent stack is backed up in %r13, but we need to signal to it when we're done using the stack.

	# Test original %rsi to see if we skipped creating 3 pipes, in which case
	# we should also skip closing them.
	movq 80(%r13), %rsi  # Resore original %rsi (options bitfield).
	testq $0x2, %rsi
	jnz 4f  # Skip if inheriting.
	nop
	testq $0x4, %rsi
	jnz 4f  # Skip if closing standard files.
	nop

	# Close parent ends of the 3 pipe pairs now that we've forked.

	# Perform the ‘close’ syscall.
	movq 32(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdin(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdin_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘close’ syscall.
	movq 24(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdout(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stdout_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘close’ syscall.
	movq  8(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stderr(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_child_close_stderr_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

4:

	# Now handle standard input, output, and error.

	movq 80(%r13), %rsi  # Resore original %rsi (options bitfield).

	# If we're closing standard streams, start right away, regardless of
	# inheritance, which is overridden by it.
	testq $0x4, %rsi
	jnz 3f  # Don't skip if we're closing pipes.
	nop
	testq $0x2, %rsi
	jz 3f  # Don't skip if we're not inheriting.
	nop
	jmp 2f  # Skip closing if we're inheriting but not closing.
	nop
3:
	# Whichever is done, we'll first close stdin, stdout, and stderr.

	# Close standard file descriptors, somewhat like ‘ssh -n’.

	# Perform the ‘close’ syscall.
	movq $0,   %rdi  # int fd
	movq $0x3, %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_stdin(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_stdin_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘close’ syscall.
	movq $1,   %rdi  # int fd
	movq $0x3, %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_stdout(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_stdout_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘close’ syscall.
	movq $2,   %rdi  # int fd
	movq $0x3, %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_stderr(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_stderr_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Now skip the rest of the setup (don't ‘dup2’) if we're closing the file
	# descriptors rather than setting up a pipe (in which case, technically
	# doing these closes beforehand was redundant, since ‘dup2’ would do it for
	# us - actually, the man pages indicate that under certain conditions this
	# redundancy can introduce a race condition and so is recommended against
	# (TODO: probably implement this suggestion without breaking things)).

	movq 80(%r13), %rsi  # Resore original %rsi (options bitfield).
	testq $0x4, %rsi
	jnz 2f  # Skip if we're closing pipes.
	nop

	# Finally, just get:
	# 40(%r13) -> stdin  0
	# 16(%r13) -> stdout 1
	#  0(%r13) -> stderr 2

	# Perform the ‘dup2’ syscall.
	movq $0,       %rsi  # new
	movq 40(%r13), %rdi  # old
	movq $33,      %rax  # dup2
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_dup2_stdin(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_dup2_stdin_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘dup2’ syscall.
	movq $1,       %rsi  # new
	movq 16(%r13), %rdi  # old
	movq $33,      %rax  # dup2
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_dup2_stdout(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_dup2_stdout_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘dup2’ syscall.
	movq $2,       %rsi  # new
	movq  0(%r13), %rdi  # old
	movq $33,      %rax  # dup2
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_dup2_stderr(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_dup2_stderr_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Finally, close the old FDs now that they've been copied over.

	# Perform the ‘close’ syscall.
	movq 40(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# If using vfork, you'd need to add a section here instead of doing the
	# verification immediately afterwards, since it breaks things (basically,
	# just check %rax and then do a direct syscall to exit, as illustrated for
	# a failed execve() a little further down).  That is, directly check %rax
	# and directly syscall to exit here if using vfork.

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_post_stdin(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_post_stdin_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘close’ syscall.
	movq 16(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# If using vfork, you'd need to add a section here instead of doing the
	# verification immediately afterwards, since it breaks things (basically,
	# just check %rax and then do a direct syscall to exit, as illustrated for
	# a failed execve() a little further down).  That is, directly check %rax
	# and directly syscall to exit here if using vfork.

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_post_stdout(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_post_stdout_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘close’ syscall.
	movq  0(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# If using vfork, you'd need to add a section here instead of doing the
	# verification immediately afterwards, since it breaks things (basically,
	# just check %rax and then do a direct syscall to exit, as illustrated for
	# a failed execve() a little further down).  That is, directly check %rax
	# and directly syscall to exit here if using vfork.

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_post_stderr(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_close_post_stderr_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

2:

	# Before we execute the command, we need to parse the encoded commands and
	# environment strings.
	#
	# So what we'll do is count the number of null bytes in each and then
	# reserve room for that many pointers on the stack.

	# Add the null terminator to our array of null-terminated string pointers.
	movq (%rsp), %rdi
	addq $16, %rdi
	subq $16, %rsp
	movq %rdi, (%rsp)
	movq $0, 8(%rsp)  # Null terminator.
	leaq 8(%rsp), %r8  # The beginning of the command pointer array.

	movq 72(%r13), %rsi  # Command string size (from original %rdx).
	movq 64(%r13), %rdx  # Command string (from original %rcx).
	testq %rsi, %rsi
	jz 3f  # Do nothing if the command string is empty.
	decq %rsi
	testq %rsi, %rsi
	jz 7f
	decq %rsi  # Skip the last NULL byte.
	# Start near the end of the string.
8:
	# Break if we reached the beginning.
	testq %rsi, %rsi
	jz 7f
	nop
	decq %rsi

	# Get the next character.
	movq %rdx, %rdi
	addq %rsi, %rdi
	movb (%rdi), %dil
	andq $0xFF, %rdi

	# If it's not null, loop back.
	testq %rdi, %rdi
	jnz 8b
	nop

	# Reached a null.  Push addr + 1.
	subq $8, %r8
	cmpq %r8, %rsp
	jnz 9f
	nop
	# We've reached the bottom of our stack, so get 2 more u64's.
	movq (%rsp), %rdi
	addq $16, %rdi
	subq $16, %rsp
	movq %rdi, (%rsp)
	movq $0, 8(%rsp)  # Just free space.
9:
	movq %rdx, %rdi
	addq %rsi, %rdi
	incq %rdi
	movq %rdi, (%r8)  # Here we do the push to add the C-string pointer.

	# Loop back.
	jmp 8b
	nop
7:
	# First character in a non-empty string.  Push an arg, addr + 0.
	subq $8, %r8
	cmpq %r8, %rsp
	jnz 9f
	nop
	# We've reached the bottom of our stack, so get 2 more u64's.
	movq (%rsp), %rdi
	addq $16, %rdi
	subq $16, %rsp
	movq %rdi, (%rsp)
	movq $0, 8(%rsp)  # Just free space.
9:
	movq %rdx, %rdi
	addq %rsi, %rdi
	movq %rdi, (%r8)  # Here we do the push to add the C-string pointer.
3:
	# Done building the command pointer for ‘execv’.  Back it up in %r8.
	#leaq 8(%rsp), %r8
	movq %r8, %r8

	# Now do the same thing but for the environment and %r9.

	# Add the null terminator to our array of null-terminated string pointers.
	movq (%rsp), %rdi
	addq $16, %rdi
	subq $16, %rsp
	movq %rdi, (%rsp)
	movq $0, 8(%rsp)  # Null terminator.
	leaq 8(%rsp), %r9  # The beginning of the command pointer array.

	movq 56(%r13), %rsi  # Command string size (from original %r8).
	movq 48(%r13), %rdx  # Command string (from original %r9).
	testq %rsi, %rsi
	jz 3f  # Do nothing if the command string is empty.
	decq %rsi
	testq %rsi, %rsi
	jz 7f
	decq %rsi  # Skip the last NULL byte.
	# Start near the end of the string.
8:
	# Break if we reached the beginning.
	testq %rsi, %rsi
	jz 7f
	nop
	decq %rsi

	# Get the next character.
	movq %rdx, %rdi
	addq %rsi, %rdi
	movb (%rdi), %dil
	andq $0xFF, %rdi

	# If it's not null, loop back.
	testq %rdi, %rdi
	jnz 8b
	nop

	# Reached a null.  Push addr + 1.
	subq $8, %r9
	cmpq %r9, %rsp
	jnz 9f
	nop
	# We've reached the bottom of our stack, so get 2 more u64's.
	movq (%rsp), %rdi
	addq $16, %rdi
	subq $16, %rsp
	movq %rdi, (%rsp)
	movq $0, 8(%rsp)  # Just free space.
9:
	movq %rdx, %rdi
	addq %rsi, %rdi
	incq %rdi
	movq %rdi, (%r9)  # Here we do the push to add the C-string pointer.

	# Loop back.
	jmp 8b
	nop
7:
	# First character in a non-empty string.  Push an arg, addr + 0.
	subq $8, %r9
	cmpq %r9, %rsp
	jnz 9f
	nop
	# We've reached the bottom of our stack, so get 2 more u64's.
	movq (%rsp), %rdi
	addq $16, %rdi
	subq $16, %rsp
	movq %rdi, (%rsp)
	movq $0, 8(%rsp)  # Just free space.
9:
	movq %rdx, %rdi
	addq %rsi, %rdi
	movq %rdi, (%r9)  # Here we do the push to add the C-string pointer.
3:
	# Done building the command pointer for ‘execv’.  Back it up in %r9.
	#leaq 8(%rsp), %r9
	movq %r9, %r9

	# Execute the new command, to replace the current (child) one.
	#movq 48(%r13), %rsi  # environment (from original %r9).
	#movq 64(%r13), %rdi  # pathname (from original %rcx).

	# Back up what we need from the parent stack.
	movq 64(%r13), %rdi  # command line data pointer (from original %rcx)
	movq 48(%r13), %rsi  # environment data (from original %r9)

	# Give up access to the stack, and signal to the parent thread that it can
	# stop being susponded and free up the stack resources we were using.
	# (Edit: doesn't seem to be necessary, since the parent won't see this
	# write to the parent stack copy.)
	movq 152(%r13), %rdx
	movq $1, 144(%r13)  # Concurrency signal that parent can stop spinlocking.
	movq %rdx, %r13  # Restore original %r13.

	# ‘execve’.
	#movq %rsi, %rdx  # environment (from original %r9).
	#movq %rdi, %rsi  # argv (from original %rcx).
	#movq %rdi, %rdi  # pathname (from original %rcx).
	movq %r9, %rdx  # environment
	movq %r8, %rsi  # argv
	movq %r8, %rdi  # pathname
	movq (%rdi), %rdi  # argv[0]
	movq $59,      %rax  # execve (for 64)
	syscall

	# If execve has returned, then an error occurred; just set negated %rax
	# (negated ERRNO) to the exit code, and directly 

	# Uncomment if using vfork:
	## Directly exit, or else we may get undefined behaviour because we're in a
	## ‘vfork()’ and shouldn't change hardly anything.  Otherwise we'd probably
	## print a more helpful error message.
	#movq %rax, %rdi
	#notq %rdi
	#incq %rdi
	#movq $60, %rax  # exit
	#syscall

	## This shouldn't be reached.
	#hlt
	#nop
	## (End uncomment if using vfork.)

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_child_start_failed(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_start_failed_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# This shouldn't be reached.

	# Error: Return after a successful ‘exec’.
	movq $0, %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_child_exec_returned_on_success(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_shell_child_exec_returned_on_success_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt

	hlt
	nop

0:
	# Parent executor.

	# Now %rdi is the joiner and %rsi is the joiner user data.

	# Close the child ends of these 3 pipe end pairs on the parent side now that
	# we've forked, unless we didn't create those pipes in the first place.
	movq %rdi, %r8  # Backup %rdi (joiner).
	movq %rsi, %r9  # Backup %rsi (joiner data).

	movq 80(%r13), %rsi  # Resore original %rsi (options bitfield).
	testq $0x2, %rsi
	jnz 8f  # Skip if inheriting.
	nop
	testq $0x4, %rsi
	jnz 8f  # Skip if closing standard files.
	nop

	# Perform the ‘close’ syscall.
	movq 40(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdin(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdin_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘close’ syscall.
	movq 16(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdout(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stdout_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Perform the ‘close’ syscall.
	movq  0(%r13), %rdi  # int fd
	movq $0x3,     %rax  # close
	syscall

	# Verify.
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stderr(%rip), %rcx
	leaq ns_system_x86_64_linux_err_msg_shell_pipe_parent_close_stderr_size(%rip), %rdx
	movq (%rdx), %rdx
	movq %rax, %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# Before we return, and free the stack, we need to ensure that the child
	# setup is done using it and has either exited or handed off control to
	# ‘execve’.  Spinlock until the signal is sent or the thread terminated for
	# some other reason.
	#
	# (Don't clobber %r8 or %r9 here, or else change how you backup the
	# joiner and joiner user data.)
8:
	# Edit: actually, it looks like the thread gets a copy of the stack, so our
	# changes don't seem to affect it.  Otherwise:
	# TOD O: check if child thread terminated, and then break out of the loop if
	# so.  FIXM E: this needs to be done or else if the child thread is killed
	# at just right the right time (or a close or dup2 fails), then the parent
	# hangs forever!
	movq 144(%r13), %rdi
	testq %rdi, %rdi
	#jz 8b  # Spinlock.  Edit: no need to wait; the child has a copy of the stack, apparently.
	nop
7:

	# Restore.
	movq %r9, %rsi  # Restore %rsi (joiner data).
	movq %r8, %rdi  # Restore %rdi (joiner).

	# Setup registers to return, and will return to *%rax.

	movq  8(%r13), %r9   # stderr reader user data
	movq 24(%r13), %r8   # stdout reader user data
	movq 32(%r13), %rcx  # stdin writer user data
	movq %rsi,     %rdx  # joiner user data
	movq $0,       %rsi  # tuple user data
	leaq _ns_system_x86_64_linux_shell_tuple(%rip), %rdi  # tuple callback continuation
	# (Note: we'll exploit our internal knowledge of what the joiner is and
	# skip storing or backing it up here, since we can reproduce it knowing
	# just the internal implementation, and discard %rdi, the joiner.)

	# Restore original return (%rdi) to %rax.
	movq 88(%r13), %rax

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
	addq (%rsp), %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	addq $16, %rsp
	movq 0(%rsp), %r10
	addq $16, %rsp
	movq 0(%rsp), %r11
	addq $16, %rsp
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp
	movq 8(%rsp), %r13
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

	# Restore the working storage units and the stack, except don't preserve
	# the arguments we'll pass to the return continuation, as we specified; nor
	# don't preserve the temporary register we'll use to jump back to the
	# original %rdi, which we'll put as %rax:
	# 	- %rdi
	# 	- %rsi
	# 	- %rdx
	# 	- %rcx
	# 	- %r8
	# 	- %r9
	# 	- %rax

	# Restore dynamically sized portion.
	addq (%rsp), %rsp

	# TODO: assert %rsp == %r13 now.

	# Restore working storage for stderr.
	addq $16, %rsp

	# Restore working storage for stdout.
	addq $16, %rsp

	# Restore working storage for stdin.
	addq $16, %rsp

	# Restore %r8 and %r9.
	#movq 0(%rsp), %r9
	#movq 8(%rsp), %r8
	addq $16, %rsp

	# Restore %rdx and %rcx.
	#movq 0(%rsp), %rcx
	#movq 8(%rsp), %rdx
	addq $16, %rsp

	# Restore %rdi and %rsi.
	#movq 0(%rsp), %rsi
	#movq 8(%rsp), %rdi
	addq $16, %rsp

	# Restore %rcx and %r10.
	movq 0(%rsp), %r10
	#movq 8(%rsp), %rcx
	addq $16, %rsp

	# Restore %rax and %r11.
	movq 0(%rsp), %r11
	#movq 8(%rsp), %rax
	addq $16, %rsp

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	# Restore %r13.
	# 0(%rsp) is working storage space.
	movq 8(%rsp), %r13
	addq $16, %rsp

	# Return.
	#xchgq %rdi, %rax
	jmpq *%rax
	nop

# A tuple extractor suitable for use for every, since we don't need the tuple
# extractor to change on every instance, although we could; using the same
# tuple extractor in this context for all ‘shell’ tuples returned will work in
# our current implementation.
#
# (This is the tuple callback continuation that ‘shell’ uses and returns for
# the tuple callback continuation, under the hood.)
_ns_system_x86_64_linux_shell_tuple:
	# Ignore and discard the user data.
	movq $0, %rdi

	# Back up the return address.
	movq %rsi, %r9  # We'll return to %r9.

	# Now load the contents of the tuple, so the caller has access by having a
	# copy of the contents.

	movq $0, %r8  # This is specified to be set to 0.

	# Add similar tuple accessors to this one.
	leaq _ns_system_x86_64_linux_shell_tuple_stderr_tuple(%rip), %rcx
	leaq _ns_system_x86_64_linux_shell_tuple_stdout_tuple(%rip), %rcx
	leaq _ns_system_x86_64_linux_shell_tuple_stdin_tuple(%rip), %rcx

	# Joiner callback (we exploited internal details by skipping storing or
	# preserving this, since we know that the joiner would just be
	# ‘_ns_system_x86_64_linux_fork_join’):
	leaq _ns_system_x86_64_linux_fork_join(%rip), %rdi  # Joiner callback.

	# Return.
	jmp *%r9
	nop

_ns_system_x86_64_linux_shell_tuple_stdin_tuple:
	# We have return in %rdi and user data in %rsi.
	#
	# Again we exploit knowledge of internal implementation details in our
	# module here.

	# Back up the return address in %r9.  We'll return to %r9.
	movq %rdi, %r9

	# Assign user data from %rsi before %rsi gets overwritten.
	movq %rsi, %rdi

	# Assign the arguments.
	movq $0,   %r8
	leaq _ns_system_x86_64_linux_new_writer_close(%rip), %rcx
	leaq _ns_system_x86_64_linux_new_writer_query(%rip), %rdx
	leaq _ns_system_x86_64_linux_new_writer_write(%rip), %rsi
	movq %rdi, %rdi

	# Return.
	jmp *%r9
	nop

_ns_system_x86_64_linux_shell_tuple_stdout_tuple:
_ns_system_x86_64_linux_shell_tuple_stderr_tuple:
	# We have return in %rdi and user data in %rsi.
	#
	# Again we exploit knowledge of internal implementation details in our
	# module here.

	# Back up the return address in %r9.  We'll return to %r9.
	movq %rdi, %r9

	# Assign user data from %rsi before %rsi gets overwritten.
	movq %rsi, %rdi

	# Assign the arguments.
	movq $0,   %r8
	leaq _ns_system_x86_64_linux_new_reader_close(%rip), %rcx
	leaq _ns_system_x86_64_linux_new_reader_query(%rip), %rdx
	leaq _ns_system_x86_64_linux_new_reader_read(%rip),  %rsi
	movq %rdi, %rdi

	# Return.
	jmp *%r9
	nop

# Execute arbitrary shell code.
#
# (This is _not_ like the ‘exec’-like syscalls and shell builtins, despite the
# name.)
#
# The shell code must handle return back to the continuation where it needs to
# be, since we don't do that here.
#
# Parameters:
# 	%rdi: The size of the shell code.
# 	%rsi: A pointer to the beginning of the shell code to execute.
_ns_system_x86_64_linux_exec:
	jmp *%rsi
	nop

# ################################################################
# Exiting and process maangement.
# ################################################################

# Exit with a successful status code.
#
# Parameters:
# 	(None.)
.global ns_system_x86_64_linux_exit_success
.set ns_system_x86_64_linux_exit_success, (_ns_system_x86_64_linux_exit_success - ns_system_x86_64_linux_module_begin)
.global ns_system_x86_64_linux_exit_success_force
.set ns_system_x86_64_linux_exit_success_force, (_ns_system_x86_64_linux_exit_success_force - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_exit_success:
	# First, check if we've enabled a non-default error / non-zero-exit handler.
	testq $0x4, %r15
	jz _ns_system_x86_64_linux_exit_success_force

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
_ns_system_x86_64_linux_exit_success_force:
	movq $0,  %rdi
	movq $60, %rax  # exit
	syscall

	jmp _ns_system_x86_64_linux_exit_success
	nop

.global ns_system_x86_64_linux_exit_failure
.set ns_system_x86_64_linux_exit_failure, (_ns_system_x86_64_linux_exit_failure - ns_system_x86_64_linux_module_begin)
.global ns_system_x86_64_linux_exit_failure_force
.set ns_system_x86_64_linux_exit_failure_force, (_ns_system_x86_64_linux_exit_failure_force - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_exit_failure:
	# First, check if we've enabled a non-default error / non-zero-exit handler.
	testq $0x2, %r15
	jz _ns_system_x86_64_linux_exit_failure_force

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
_ns_system_x86_64_linux_exit_failure_force:
	movq $1,  %rdi
	movq $60, %rax  # exit
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
.global ns_system_x86_64_linux_exit_custom_force
.set ns_system_x86_64_linux_exit_custom_force, (_ns_system_x86_64_linux_exit_custom_force - ns_system_x86_64_linux_module_begin)
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
_ns_system_x86_64_linux_exit_custom_force:
	# Check the exit code in case this is forced.
	testq %rdi, %rdi
	jz _ns_system_x86_64_linux_exit_success_force

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

	movq %rdi, %rdi
	movq $60,  %rax  # exit
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

# TODO: a version for calling, too:
## Perform a call to a procedure using the call stack (and, if enabled, shadow
## stack) to store the return address.
##
## Parameters:
## 	%rdi: Return, after ‘call’ returns back to us.
## 	%rsi: The address to ‘call’ to with a more conventional calling convention.
## 	%rdx: Tuple continuation to obtain some arguments:
## 		%rdi: Return, with parameters:
## 			%rdi: Size of stack data (anything after 48 is copied onto our own
## 			      stack after the return address, like the more conventional
## 			      calling convention).
## 			%rsi: The stack data.  The first full 6 u64's are loaded into
## 			      [%rdi, …, %r9], and the rest are added to the stack.
## 		%rsi: User data.
## 	%rcx: User data for the tuple continuation.  (This user data isn't
## 	      essential; it's just a convenience.  But e.g. you could just make
## 	      copies of the machine code bundled together with slightly different
## 	      data.)
##
## This clobbers:
## 	- %rax
## 	- %rdi
## 	- %rsi
## 	- %rdx
## 	- %rcx
## 	- %r8
## 	- %r9
#.global ns_system_x86_64_linux_cc_call_stack_call
#.set ns_system_x86_64_linux_cc_call_stack_call, (_ns_system_x86_64_linux_cc_call_stack_call - ns_system_x86_64_linux_module_begin)
#_ns_system_x86_64_linux_cc_call_stack_call:
#	# (This will need special care with cleanup and error cleanup routines; we
#	# do use the stack.)
#
#	# TODO
#	hlt
#	nop

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

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

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
# 		%rdi: Number of written digits printed to encode the number, excluding written padding and leftover digits.
# 		%rsi: Number of digits or characters (bytes) written or skipped, which here should be equal to the input size.
# 		%rdx: Number of digits leftover for which there was not enough space to print
# 	%rsi: Size of the memory region for writing the string.
# 	%rdx: Pointer to memory to write ascii digits to; start address.
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

	# Swap %rsi and %rdx out of convenience, to change the parameter order
	# without otherwise updating the implementation.
	xchgq %rsi, %rdx

	movq %rdx, %r11  # Size.

	#movq %rsi, %rsi  # Base.
	movq %rdx, %r10  # Current size.
	movq %rcx, %r8   # Current dividend.

5:
	# Break if out of space.
	testq %r10, %r10
	jz 4f
	decq %r10

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
	incq %r10

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
	decq %rcx
	incq %rdi
	incq %r8

	jmp 7b
6:

	# Encode the error code (%rsi) into offsets 13 through 17 of %rdi.
	notq %rsi
	incq %rsi  # %rsi = -%rsi

	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rdi
	addq $13, %rdi  # Base.
	movq $4, %rcx  # Size.
	movq %rsi, %r8  # Current dividend.
5:
	# Break if out of space.
	testq %rcx, %rcx
	jz 4f
	decq %rcx

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
	decq %rsi
	incq %rdx
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
