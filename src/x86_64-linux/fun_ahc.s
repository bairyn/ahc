# AHC function execution model.

# Module information:
#
# This module itself is untyped.  You can add at ype system such as Haskell's
# on top to exclude some or all invalid programs.  The type system establishes
# boundaries of correctness primary, but also helps to define structure
# (without which you'd need to define an alternative semantics) and also
# provides documentation.
#
# There are many ways to implement a function execution model on top of this
# platform that satisfies the requirements of the Haskell specification
# semantics.  Here is how we do it:
#
# First, under the hood, every top-level value (function, integer, whatever) is
# represented as data at a particular region.  Most support copying themselves
# and destroying themselves, although runtime linking needs to be appropriately
# handled, so references to dependencies and members of other modules are also
# taken care of.  A function that, for example, is defined as a lambda would
# likely, in order to be used, be copied somewhere else and then the copy
# mutated by being applied.  Alternatively, (and at a top-level, less commonly,
# and probably normally never without some sort of access management system in
# place, such as linear types), some lambdas may be mutated by being applied in
# place.  We entrust handling management of resources, including exclusive
# access to mutable resources via linear types, to be handled by the type
# system.  One way to think of it is as a layer on top.  At a low level,
# without the boundaries put up by the type system, we must manually by hand
# ensure requirements and specifications are conformed to, to avoid bug.

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

.global ns_fun_ahc_module_begin  # This jumps to ‘route’ too if you call it.
ns_fun_ahc_module_begin:
	.byte 0xEB, 0x36  # skip 54 bytes (jump) if execution begins here.
	.byte 0x90, 0xF4, 0x00, 0x0E  # Pad to 8 bytes, and 0x0E is like a version.
	.byte 0x00, 0x00
.global ns_fun_ahc_module_size
.set ns_fun_ahc_module_size, (ns_fun_ahc_module_end - ns_fun_ahc_module_begin)
.global ns_fun_ahc_module_size_field  # Should be at offset 8 relative to ‘begin’!
ns_fun_ahc_module_size_field:
	.quad (ns_fun_ahc_module_end - ns_fun_ahc_module_begin)  # Size.
	.quad 0x1324ABBC  # ABI.
	.quad 0  # Module hash, sha256sum of module string with a NULL hash field and NULL external module references.  TODO; just 0 until implemented.
	.quad (_module_name_end - _module_name)  # Size of the module name string.
	.quad (_module_name - ns_fun_ahc_module_begin)  # Module-relative offset to the siz of the module name string.
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
ns_fun_ahc_module_route:  # Must be at offset 54.
	leaq ns_fun_ahc_module_begin(%rip), %r11
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
	.ascii "fun_ahc"
_module_name_end:

# Example string.
#ns_fun_ahc_err_msg_placeholder_size:
#	.quad (ns_fun_ahc_err_msg_placeholder_end - ns_fun_ahc_err_msg_placeholder)
#ns_fun_ahc_err_msg_placeholder:
#	.ascii "Error: example.\n"
#	.byte 0x00
#ns_fun_ahc_err_msg_placeholder_end:

.text

## Placeholder procedure.
##
## Parameters:
## 	%rdi: return
##
## Clobbers the following registers:
## 	- %rdi
#.global ns_fun_ahc_placeholder
#.set ns_fun_ahc_placeholder, (_ns_fun_ahc_placeholder - ns_fun_ahc_module_begin)
#_ns_fun_ahc_placeholder:
#	# TODO
#	hlt
#	nop
#
#	jmpq *%rdi
#	nop

.global ns_fun_ahc_module_end
ns_fun_ahc_module_end:
