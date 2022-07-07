# Utilities.

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

.global ns_util_module_begin  # This jumps to ‘route’ too if you call it.
ns_util_module_begin:
	.byte 0xEB, 0x36  # skip 54 bytes (jump) if execution begins here.
	.byte 0x90, 0xF4, 0x00, 0x0E  # Pad to 8 bytes, and 0x0E is like a version.
	.byte 0x00, 0x00
.global ns_util_module_size
.set ns_util_module_size, (ns_util_module_end - ns_util_module_begin)
.global ns_util_module_size_field  # Should be at offset 8 relative to ‘begin’!
ns_util_module_size_field:
	.quad (ns_util_module_end - ns_util_module_begin)  # Size.
	.quad 0x1324ABBC  # ABI.
	.quad 0  # Module hash, sha256sum of module string with a NULL hash field and NULL external module references.  TODO; just 0 until implemented.
	.quad (_module_name_end - _module_name)  # Size of the module name string.
	.quad (_module_name - ns_util_module_begin)  # Module-relative offset to the siz of the module name string.
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
ns_util_module_route:  # Must be at offset 54.
	leaq ns_util_module_begin(%rip), %r11
	addq %r11, %rax
	jmpq *%rax
	nop

# Module dependencies.  Run-time re-linking and relocation will need to handle
# this.
_system:
	jmp ns_system_x86_64_linux_module_begin
	nop
	hlt

_mod_dep_end:
.quad 0x12342345
.quad 0

# Now the .data stuffs.

_module_name:
	.ascii "util"
_module_name_end:

ns_util_err_msg_not_writeable_size:
	.quad (ns_util_err_msg_not_writeable_end - ns_util_err_msg_not_writeable)
ns_util_err_msg_not_writeable:
	.ascii "Error:\n"
	.ascii "	AHC machine code cannot change itself!  Verify compilation and system\n"
	.ascii "	configuration for self-modifying code.\n"
	.byte 0x00
ns_util_err_msg_not_writeable_end:

# A very simple environment for ‘shell_simple’.
ns_util_shell_simple_environment_size:
	.quad (ns_util_shell_simple_environment_end - ns_util_shell_simple_environment)
ns_util_shell_simple_environment:
	.ascii "PATH=/usr/bin:/bin\x00"
	#.byte 0x00
ns_util_shell_simple_environment_end:

.text

# Can we return?
# 	%rdi: return
#
# Clobbers nothing.
.global ns_util_can_cont
.set ns_util_can_cont, (_ns_util_can_cont - ns_util_module_begin)
_ns_util_can_cont:
	jmpq *%rdi

# Make sure that the code can rewrite itself.
#
# For segv handling, only call this once per process instantion (maybe this
# requirement may be removed with further enhancements in the future).
#
# Parameters:
# 	%rdi: return
#
# (I'm trying to keep dependence on the stack at a minimum, but %rsp is used
# here mainly because it was convenient to have more storage space, so this
# procedure assumes at least that %rsp maintains a valid stack.)
#
# Clobbers TODO.
.global ns_util_system_verify_writeable
.set ns_util_system_verify_writeable, (_ns_util_system_verify_writeable - ns_util_module_begin)
_ns_util_system_verify_writeable:
	# Backup return.
	subq $16, %rsp
	movq $0, 8(%rsp)  # Padding.
	movq %rdi, 0(%rsp)

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

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	leaq 6f(%rip), %r14

	# First, trap SEGV to print our error message instead just aborting with
	# SEGV.
	leaq 0f(%rip), %rsi
	movq $0, %rdx
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_trap_segv, %rax
	jmp _system
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
	movq $0, %rcx
	leaq ns_util_err_msg_not_writeable(%rip), %rdx
	leaq ns_util_err_msg_not_writeable_size(%rip), %rsi
	movq (%rsi), %rsi
	movq $2, %rdi
	movq $ns_system_x86_64_linux_exit_custom, %rax
	jmp _system
	nop
	hlt
1:

	# Okay, now restore the default SEGV trap.
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_restore_trap_segv, %rax
	jmp _system
9:
	nop

	# (Uncomment this to trigger a segfault to make sure the SEGV trap is
	# actually being restored:)
	#movq $0, %rdi
	#movq (%rdi), %rdi

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
	#movq 0(%rsp), %rdi
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

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	# Restore return.
	movq 0(%rsp), %rdi
	# 8(%rsp): padding.
	addq $16, %rsp

	# Return.
	jmpq *%rdi
	nop

# Utility function whose return value you can use as a tuple callback for
# ‘new_writer’'s ‘.write’ action, and then you can just point the user data to
# be a pointer to data you set up on the stack.
#
# This works like defining a struct to specify how to format the data.
#
# ‘ns_util_get_tuple_write’ should be called to return the absolute address of
# the tuple accessor continuation (‘_ns_util_tuple_write_accessor’).
#
# The data should be set up as follows (5 * 8 = 40 bytes):
# 	struct {
# 		uint64_t options_bitfield;  // enable timeout, etc.
# 		int64_t  timeout_seconds;
# 		int64_t  timeout_nanoseconds;
# 		uint64_t write_size;
# 		uint64_t data_pointer;
# 	};
#
# Parameters:
# 	%rdi: Return, with parameters:
# 		%rdi: An address you can pass as the outer tuple callback.
#
# Clobbers %rax.
.global ns_util_get_tuple_write
.set ns_util_get_tuple_write, (_ns_util_get_tuple_write - ns_util_module_begin)
_ns_util_get_tuple_write:
	leaq _ns_util_tuple_write_accessor_outer(%rip), %rax
	xchgq %rdi, %rax
	jmp *%rax
	nop

_ns_util_tuple_write_accessor_outer:
	movq %rdi, %rax  # Backup return address.
	movq %rsi, %r8   # Backup user data.

	movq $0,  %r9       # This should be ‘0’.
	movq %r8, %r8       # Nested tuple accessor user data (we'll just have it
	                    # be the same as our own user data.)
	leaq _ns_util_tuple_write_accessor_inner(%rip), %rcx  # Nested tuple accessor.
	movq 16(%r8), %rdx  # Read from 16(%base) into %rdx for nanoseconds.
	movq  8(%r8), %rsi  # Read from  8(%base) into %rsi for seconds.
	movq  0(%r8), %rdi  # Read from  0(%base) into %rdi for options_bitfield.

	jmp *%rax
	nop

_ns_util_tuple_write_accessor_inner:
	movq %rdi, %rax  # Backup return address.
	movq %rsi, %r8   # Backup user data.

	movq 24(%r8), %rdi  # Read from 24(%base) into %rdi for number of bytes.
	movq 32(%r8), %rsi  # Read from 32(%base) into %rsi for data start pointer.

	jmp *%rax
	nop

# Same as ‘get_tuple_write’ but for ‘new_reader’'s ‘.read’ action.
.global ns_util_get_tuple_read
.set ns_util_get_tuple_read, (_ns_util_get_tuple_read - ns_util_module_begin)
_ns_util_get_tuple_read:
	# The struct is in the same format, so just treat is like a write tuple.
	jmp _ns_util_get_tuple_write
	nop

# A wrapper around cli's ‘shell’.
#
# It's similar, except an automatic join is performed (like running it in the
# foreground), and the standard file descriptors are automatically inherited.
#
# Note: the environment is reset to a very minimal one.
#
# Parameters:
# 	%rdi: Return.
# 	%rsi: Size of command line encoding.
# 	%rdx: Command line encoding.  (Pointer to start of it.)
# 	%rcx: Must be 0.  Helps future enhacements.
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
.global ns_util_shell_simple
.set ns_util_shell_simple, (_ns_util_shell_simple - ns_util_module_begin)
_ns_util_shell_simple:
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

	# Extra working storage units.
	subq $16, %rsp
	movq $0, 8(%rsp)
	movq $0, 0(%rsp)

	# Push / add our own cleanup to our collection of cleanup requirements for
	# error handling (see the module documentation for more information).
	leaq 6f(%rip), %r14

	leaq ns_util_shell_simple_environment(%rip), %r9
	leaq ns_util_shell_simple_environment_size(%rip), %r8
	movq (%r8), %r8
	# ‘shell()’ and then immediately join afterward (we don't do anything else
	# before joining, or else we'd need to add to our cleanup requirements.
	# ‘join’ *is* our cleanup).
	movq %rdx,     %rcx  # Command line.
	movq %rsi,     %rdx  # Size.
	movq $0x2,     %rsi  # Options.
	leaq 9f(%rip), %rdi  # Return.
	movq $ns_system_x86_64_linux_shell, %rax
	jmp _system
9:
	nop

	# Backup joiner user data.
	movq %rdx, 8(%rsp)
	movq $0,   0(%rsp)

	# Call the tuple to extract its contents.
	movq %rdi    , %rdx  # We'll call %rdx, the tuple continuation.
	movq %rsi,     %rdi  # User data for the tuple.
	leaq 9f(%rip), %rsi  # Return address from the tuple extractor/accessor callback.
	jmp *%rdx
9:
	nop

	# The joiner callback was in the tuple.  Now it's in %rdi.  Call the joiner.
	movq %rdi,     %rdx  # Move the joiner to an available register.
	movq 8(%rsp),  %rsi  # Joiner user data.
	leaq 9f(%rip), %rdi  # Return address from the joiner.
	jmp *%rdx
9:
	nop

	# Setup registers to return, and will return to *%rax.
	movq 56(%rsp), %rax

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
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp
	movq 0(%rsp), %r9
	movq 8(%rsp), %r8
	addq $16, %rsp
	movq 0(%rsp), %rcx
	movq 8(%rsp), %rdx
	addq $16, %rsp
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp
	movq 0(%rsp), %r10
	movq 8(%rsp), %rcx
	addq $16, %rsp
	movq 0(%rsp), %r11
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
	movq $ns_system_x86_64_linux_exit_custom, %rax
	jmp _system
	nop
5:
	# Cleanup.

	# Restore the working storage units and the stack.

	# Restore space from extra storage units.
	addq $16, %rsp

	# Restore %r8 and %r9.
	movq 0(%rsp), %r9
	movq 8(%rsp), %r8
	addq $16, %rsp

	# Restore %rdx and %rcx.
	movq 0(%rsp), %rcx
	movq 8(%rsp), %rdx
	addq $16, %rsp

	# Restore %rdi and %rsi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Restore %rcx and %r10.
	movq 0(%rsp), %r10
	movq 8(%rsp), %rcx
	addq $16, %rsp

	# Restore %rax and %r11.
	movq 0(%rsp), %r11
	movq 8(%rsp), %rax
	addq $16, %rsp

	# Restore %r15 and %r14.
	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	# Return.
	#xchgq %rdi, %rax
	jmpq *%rax
	nop

.global ns_util_module_end
ns_util_module_end:
