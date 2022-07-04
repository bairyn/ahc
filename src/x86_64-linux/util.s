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

	# (Uncomment this to trigger a segfault to make sure it's actually being
	# restored:)
	#movq $0, %rdi
	#movq (%rdi), %rdi

	# Restore return.
	movq 0(%rsp), %rdi
	# 8(%rsp): padding.
	addq $16, %rsp

	# Return.
	jmpq *%rdi
	nop

.global ns_util_module_end
ns_util_module_end:
