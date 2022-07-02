# Utilities.

# Configure the platform.
.code64

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
	.quad 0  # Module hash.
	.quad 0  # Size of the module name string.
	.quad 0  # Module-relative offset to the siz of the module name string.
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
	movq $2, %rdi
	leaq ns_util_err_msg_not_writeable(%rip), %rsi
	leaq ns_util_err_msg_not_writeable_size(%rip), %rdx
	movq (%rdx), %rdx
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
