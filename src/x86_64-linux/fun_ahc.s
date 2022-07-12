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

# TODO: how to handle:
# - linking?
# 	- The modules so far have been designed to be mostly position independent relative to the *module* (just some module pointers near the beginning would need to be updated for relocation), but aren't well structured yet to be easily relocatable individual procedures (e.g. by copying the function; which would need additional special care beyond just copying the module, because many procedures carry module-relative references).
# 	- Aha!  Just add a little pointer to the internal module right before each
# 	  individual function.  Whoa!  Just treat it like closure data: pointer to
# 	  the beginning of - like a router! or like DNS! - a sequence or GADT or
# 	  record of routes to third-party dependencies.  So similar to what you
# 	  have now, except just add a little pointer (like parameter) right before
# 	  (or near right before) the code for a router for external dependencies.
# 	  The type system can help to ensure consistency, e.g. that the offsets are
# 	  the same.
# 	  TODO: apply these ideas to the other modules, but probably let me test
# 	  some of these ideas with my own implementation and play around with them
# 	  first here.
# - memory allocation?
# 	- My current thinknig is along the lines of this mainly being a
# 	  type-system-level concern, though of course low-level defintions can e.g.
# 	  simply allocate another base allocation of memory, and we can tell the
# 	  type system that this action allocates a memory resource.

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

# TODO: once you have some more fully developed instances, turn other modules'
# values into the same format and encoding here, so that you can copy and
# relocate individual methods without having to copy the whole module (or do it
# on an ad-hoc basis).

# Here is a low-level, manual implementation similar to have I imagine the
# compiler will compile top-level declarations, although I have yet to work out
# the details.
#
# The next bit, the very first bit, is set to ‘1’ (currently unsupported!) if
# we're using a universal, variable-width (unbounded width!) format for numbers
# e.g. the size encoding.  But it's ‘0’, and in fact the rest of the quad (u64)
# encodes the size of the value in memory.
#
# This is like:
# example_3 :: Word64
# example_3 = 3
#
# A summary of the format looks like this:
# 	struct value_s {
# 		// Header region.   // (Byte offset.)
# 		u64 prefix;         // (  0) Specially crafted constant.  (Byte offset 0.)
# 		u64 size_bits;      // (  8) Size of the whole value, including this header, in bits.
# 		u64 magic;          // ( 16) Magic FunAHC tag, a constant value.
# 		u64 version;        // ( 24) Encoding format.
# 		u64 machine;        // ( 32) Platform encoding; set to ‘9' for ‘x86_64-system’.
# 		u64 options;        // ( 40) Options bitfield.
# 		i64 linker_table;   // ( 48) Relative to the beginning of this value, pointer to a table in memory that can point to external references outside this value.  (Often this table can be inside or outside this Value, but check the executor specifications.)  Access external to Values should be only through this pointer, which may be updated in case the Value is copied and relocated.
# 		u64 metadata_size;  // ( 56) Size of metadata.
# 		i64 metadata;       // ( 64) Value-relative metadata pointer (embedded in this value).
# 		i64 type;           // ( 72) Value-relative type pointer (embedded in this value).
# 		i64 impl;           // ( 80) Value-relative value implementation pointer (embedded in this value), normally $0x80, right after the header.
# 		u64 receiver_size;  // ( 88) Receiver region size (e.g. input parameters for the next execution frame; unspecified here; maybe the type system can specify encoding etc.; generally others can write to this, but details are for the executor or other components)
# 		i64 receiver;       // ( 96) Receiver region Value-relative base pointer
# 		u64 reserved0;      // (104) Reserved for future use.
#
# 		// Receiver region.
# 		// We'll suggest normally 2 u64s bytes here (e.g. status u64 bitfield
# 		// and then Value-relative pointer u64), but it can be any size.
# 		//
# 		// This can go anywhere in the Value, but we suggest putting it right
# 		// after the header.
# 		u64 receiver0;      // (112) Optionally typed receiver data.
# 		u64 receiver1;      // (120) Optionally typed receiver data.
#
# 		// Implementation.  Everything else.  Machine code.  Context like type
# 		// system information can help inform how to interpret it.  Parts of
# 		// the implementation region can represent output, parts can represent
# 		// code for an executor (thread) to execute to perform a frame advance
# 		// from one frame at a particular point in time to the next frame at
# 		// another time, parts can represent state, and parts can represent
# 		// working storage units as memory.  (Again, can go anywhere; we
# 		// suggest putting it right after the receiver region.)
# 		//
# 		// If you are using a type system, probably the type system and
# 		// (perhaps more relevantly) the executor may add constraints to what
# 		// can go in here.  But we suggest starting off the implementation
# 		// region with an IP-relative jump to the currently (that is, the last)
# 		// finalized buffer, which can hold machine code (for the executor, for
# 		// evaluating a frame or for copying or for just returning the
# 		// finalized buffer offset), output, etc.
# 		u8 code[];          // (Byte offset 128.)
# 	};
# 	typedef struct value_s value_t;
.global ns_fun_ahc_example_3
.set ns_fun_ahc_example_3, (_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin)
.set _ns_fun_ahc_example_3_size, (_ns_fun_ahc_example_3_end - _ns_fun_ahc_example_3)
_ns_fun_ahc_example_3:
# Okay, the beginning of the entire value.
# First, insert our magical quad that must be at the beginning of each value in
# our own little function execution model spec, which also serves as an 8-byte
# prefix for our universal variable-width encoder to specify that the length of
# the length part of the whole thing is 2^(number of 1s before the 0, that is,
# 7), so that the length is specified by the u64 that appears after the first
# (this is explained better later on in this file) (where the next 3 bits are
# 0).  Also, BE (big-endian).
# So far, at byte offset 0.
.byte 0xFE, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x00  # Header u64 prefix.
# Header BE u64 size (the whole thing, so starting from the header prefix).
# But since the platform we are on now is little-endian, just swap the byte
# order.  Add ‘<< 3’ since the size here is in bits, not bytes.
# So far, at byte offset 8.
.byte (((_ns_fun_ahc_example_3_size << 3) >> 56) & 0xFF)
.byte (((_ns_fun_ahc_example_3_size << 3) >> 48) & 0xFF)
.byte (((_ns_fun_ahc_example_3_size << 3) >> 40) & 0xFF)
.byte (((_ns_fun_ahc_example_3_size << 3) >> 32) & 0xFF)
.byte (((_ns_fun_ahc_example_3_size << 3) >> 24) & 0xFF)
.byte (((_ns_fun_ahc_example_3_size << 3) >> 16) & 0xFF)
.byte (((_ns_fun_ahc_example_3_size << 3) >>  8) & 0xFF)
.byte (((_ns_fun_ahc_example_3_size << 3) >>  0) & 0xFF)
# This came from
# 	ghci> printf "0x%08X\n" =<< (randomRIO (0x0, 0xFFFFFFFFFFFFFFFF))
# 	0x83F18A2457AA2B4B
# So far, at byte offset 16:
.byte 0x83, 0xF1, 0x8A, 0x24,  0x57, 0xAA, 0x2B, 0x4B  # Magic FunAHC tag to indicate the format.  (Even though it's the third u64, not first.)
# So far, at byte offset 24:
.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x01  # Flat encoding format and version.  Must be 1.
# So far, at byte offset 32:
# Encoding of the Value structure.  We use $9 for our own platform,
# ‘x86_64-linux’.  ‘1’ is reserved for a yet-to-be-implemented encoding that is
# based on unbounded variable-width bit encodings from bits rather than
# aribtrarily-sized bytes with fixed widths.  Or just leave it as ‘version’.
.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x09  # This one, however, is big-endian.
# So far, at byte offset 40:
.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x00  # Options bitfield.  Currently all 0.
# So far, at byte offset 48:
# Again, do a BSWAP (or nop if the assembler would already put it big-endian).
#.byte (((_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin) >> 56) & 0xFF)
#.byte (((_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin) >> 48) & 0xFF)
#.byte (((_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin) >> 40) & 0xFF)
#.byte (((_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin) >> 32) & 0xFF)
#.byte (((_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin) >> 24) & 0xFF)
#.byte (((_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin) >> 16) & 0xFF)
#.byte (((_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin) >>  8) & 0xFF)
#.byte (((_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin) >>  0) & 0xFF)
# Note that at this point forward, the native endianness can be and is used,
# and byte-based values are again used.
# Linker table pointer, relative to the very beginning of the value (that is,
# the header u64 prefix).  When copying, this must be updated!  The encoding,
# offset, format, and size requirements are left specified by the type system.
# (Under the hood, the default and initial value; but if you're copying this
# value and relocating, you must update or manage this pointer as needed, e.g.
# by copying other dependencies or creating a new routing table like a pointer
# to route messages to a copy stored somewhere else.)  For referencing anything
# external to the value, even within the same module, this linker table must be
# referenced.  It serves as a common gateway with the external world.  i64.
#
# Also this attribute must be referenced to access the linker table.  Then
# there is a common gateway or access point, greatly aiding in relocation:
# Values then can merely be copied and relocated where the linker table is
# copied and then this pointer is just updated.  (Moreover, the type system and
# executor may give us information that enables us to save resources for better
# performance in our internal implementation.)
.quad (_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin)

# So far, at byte offset 56:
.quad 0   # Size (in bytes) of value metadata.  Optionally the implementation
          # may make this non-zero to mean something.  If desired inside the
          # type system, this can include additional type system information.
# So far, at byte offset 64:
.quad 0   # Value-relative pointer (in bytes) of value metadata.  Optionally the implementation may make this non-zero to mean something.  Points to a region within the Value.
# So far, at byte offset 72:
.quad 0   # Value-relative pointer to the type, which is a nested Value embedded within this one.
# So far, at byte offset 80:
.quad (_ns_fun_ahc_example_3_impl - _ns_fun_ahc_example_3)  # Value-relative pointer to the implementation-specific/defined data, code, or encoding of the Value, conveniently right after the header ends.  Often this is machine code.  This is equal to $128.
# So far, at byte offset 88:
.quad 0   # Size of the receiver region, where other Values can write for
          # whatever signalling purposes may be supported or specified.
# So far, at byte offset 96:
.quad 0   # Value-relative pointer to the receiver region, which can contain
          # input values for function application.
# So far, at byte offset 104:
.quad 0   # Reserved for future use; set to 0.  Do note the space cost this
          # adds to values.  But I would guess this isn't too difficult to
          # adapt to.
# So far, at byte offset 112:
# The header is now done.  It had 14 u64s (uint64_t)s.

# Receiver region.  If the type system and executor permits, external Values
# can write here, except normally this is not used for other Values to write to
# (but if this Value is meant to support writes from elsewhere and from
# external sources, this is where they would go), but instead to use as space
# for, after the top-level value is copied, this region in the copy is changed
# to supply an input value along with signalling the right bits in the bitfield
# to indicate function application, along with the input to be supplied.

_ns_fun_ahc_example_3_receiver_status:
# Options bitfield:
# 	Bit 0: Version / interpretation bits; set to 0.
# 	Bit 1: Currently unused.  Set to 0.
# 	Bit 2: Whether the next frame will have input supplied to do function
# 	       application.
# 	Bit 3: Currently unused.  Set to 0.
.quad 0
_ns_fun_ahc_example_3_receiver_input:
# If there is an input supplied for function application, this is a
# Value-relative pointer to that input.
.quad 0

# The receiver region is now done.  We've had 16 u64s so far.

# The implementation region.  It's probably a good idea for the implementation
# to put a block in itself, perhaps near its beginning, to signal that it's not
# a finalized state, so that anybody else who copies us, our Value in whole,
# can resume a frame from our last finalized state.  An alternative approach is
# to actually have the implementation region start with a simple pointer or
# wrapper to a region later on in the Value, and to build up a non-frozen
# state, and then perform an atomic write to that u64 pointer to finalize it,
# like double buffering in OpenGL: the buffer is switched (the pointer near the
# beginning of the implementation region) is switched to the other buffer
# that's being built up or constructed.

_ns_fun_ahc_example_3_impl:
	# Just do a jump to current finalized buffer (local-relative!)
	jmp _ns_fun_ahc_example_3_impl_buffer0
	nop

# (The locations of non-finalized buffers might move around a bit.)
_ns_fun_ahc_example_3_impl_buffer0:
	# Word64 implementation:
	# Parameters:
	# 	%rdi: Return to executor when frame is finalized, with parameters:
	# 		%rdi: The Word64 in the finalized state given the implicit time value consumed.
	movq %rdi, %rsi
	movq $3, %rdi
	jmp *%rsi
	nop

# (The locations of non-finalized buffers might move around a bit.)
_ns_fun_ahc_example_3_impl_buffer1:
	jmp *%rdi
	nop

_ns_fun_ahc_example_3_end:

# Okay, cool, so I think you have the foundation in place!  Now it's just a
# matter of putting everything together.  Fun puzzle or challenge to design and
# engineer this thing.  Cool!  I think the rest will just fall into place.

# Oh, another thing to contemplate: GADTs?  Maybe look at HoTT for inspiration.
# Because the ‘movq $3, %rdi’ thing is just an ad-hoc, hardware-specific
# optimization.  The executor maybe could say that the machine code can do
# hardware-specific optimizations with essentially equivalent observability.

# Simple example, though:
# Now try lists.
#
# (Or first do a simple function application example, like swaps.)

# A Value.  16 u64s (128 bytes) followed by the implementation, often machine
# code for an executor to advance a frame in time by one frame.
_ns_fun_ahc_example_swap:
.global ns_fun_ahc_example_swap
.set ns_fun_ahc_example_swap, (_ns_fun_ahc_example_swap - ns_fun_ahc_module_begin)
.set _ns_fun_ahc_example_swap_size, (_ns_fun_ahc_example_swap_end - _ns_fun_ahc_example_swap)
_ns_fun_ahc_example_swap:
	.byte 0xFE, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x00  # (  0) u64 prefix, bytes (BE)
	.byte (((_ns_fun_ahc_example_swap_size << 3) >> 56) & 0xFF)  # (  8) u64 bit size, BE
	.byte (((_ns_fun_ahc_example_swap_size << 3) >> 48) & 0xFF)
	.byte (((_ns_fun_ahc_example_swap_size << 3) >> 40) & 0xFF)
	.byte (((_ns_fun_ahc_example_swap_size << 3) >> 32) & 0xFF)
	.byte (((_ns_fun_ahc_example_swap_size << 3) >> 24) & 0xFF)
	.byte (((_ns_fun_ahc_example_swap_size << 3) >> 16) & 0xFF)
	.byte (((_ns_fun_ahc_example_swap_size << 3) >>  8) & 0xFF)
	.byte (((_ns_fun_ahc_example_swap_size << 3) >>  0) & 0xFF)
	.byte 0x83, 0xF1, 0x8A, 0x24,  0x57, 0xAA, 0x2B, 0x4B  # ( 16) u64 magic, bytes (BE)
	.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x01  # ( 24) u64 version, BE
	.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x09  # ( 32) u64 machine, BE
	.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x00  # ( 40) u64 options, BE
	.quad (_ns_fun_ahc_example_swap - ns_fun_ahc_module_begin)  # ( 48) i64 linker_table, host endianness (jumps to ‘module_route’)
	.quad 0                                                # ( 56) u64 metadata_size, host
	.quad 0                                                # ( 64) i64 metadata, host
	.quad 0                                                # ( 72) i64 type, host (Value-relative pointer to embedded Value, or possibly external Value to save space)
	.quad (_ns_fun_ahc_example_swap_impl - _ns_fun_ahc_example_swap)  # ( 80) i64 impl pointer, host
	.quad (_ns_fun_ahc_example_swap_impl - _ns_fun_ahc_example_swap_receiver)  # ( 88) u64 receiver_size, host
	.quad (_ns_fun_ahc_example_swap_receiver - _ns_fun_ahc_example_swap)  # ( 96) i64 receiver
	.quad 0                                                # (104) u64 reserved0
_ns_fun_ahc_example_swap_receiver:
	.quad 0                                                # (112) Status bitfield.  Bit 2 for funapp.
	.quad 0                                                # (120) Data / pointer (Value-relative).
_ns_fun_ahc_example_swap_impl:
	# (128) Implementation, swappable (a la OpenGL's double buffering) pointer.
	jmp _ns_fun_ahc_example_swap_impl_buffer0
	nop
_ns_fun_ahc_example_swap_impl_buffer0:
	# TODO
	hlt
	nop

	# swap f x y = f y x
	# swap = \f x y -> f y x
	# swap = \f -> \x -> \y -> (f y) x
	# (Note: no dups or dels are used here.  Just reordering arguments.)

	# Until all values are input, essentially we're mutating ourselves to fill
	# in ‘holes’ until we have the normalized expression, ‘(f y) x’.  Here,
	# depending on how much run-time QA we want to do during execution (the QA
	# has costs, mainly performance-related), we can verify or validate
	# attributes and sizes to various degrees, or we can simply assume sizes,
	# pointers, and attributes are correct and directly write to the ‘f’
	# Value-relative Value pointer's application status bit signals and input
	# value; and when the next frame is needed for lazy evaluation, jump to the
	# ‘f’ value to execute it - that is, in the normal case, where ‘f’ has been
	# copied to the local Value memory region.

	# (TODO: I think I have a good gist of how this can be done up until the
	# normalization, but I'll probably need to think through the linking and
	# external communication parts?  I guess if an external Value wants to
	# supply us with a complex Value, it can supply us a pointer as input,
	# which must pointer either to a region inside our own Value, or the linker
	# table, but the linker table doesn't give it much flexibility beyond
	# referring ot other modules perhaps, so maybe it could just stash its
	# value somewhere in our region and then supply its input pointer.  It can
	# read our linker table data if needed.  Same for us.)
	#
	# (TODO: If we could only write to an external Value's 2 quads in its
	# receiver region, then we can only refer to other bits of data in that
	# Value or (in typical cases) only the beginning of other modules, but not
	# long references to Values within other modules, since we'd need to encode
	# more data, e.g. our base offset relative to the beginning of the module
	# in which we reside.
	#
	# And I guess we can work with that.  If we write externally, we need to
	# tell the external thing how to reach us, and to do that we may need to
	# access its linker table and get access to additional storage in its
	# Value.)
	#
	# (TODO: also, this means that under typical circumstances, it is probably
	# commonly needed to access extra storage for external writes, but only if,
	# which is probably not typical, since probably - I imagine - copying
	# externally locally and then evaluating is probably more common.
	#
	# TODO: Now if we copy from externally, then we should only need that Value
	# and its linker table, which is its bridge or interface with the external
	# world, as accesses with external locations should only be done through
	# the linker table, e.g. to lookup Values external (that is, not embedded
	# inside the same) to a Value.  NOTE: in particular, the linker table
	# should be accessed only through the linker table pointer inside the
	# Value.
	#
	# TODO: so in order to copy external Values, we can just copy its linker
	# table and then update the linker table value.
	#
	# TODO: Okay, I *think* that's enough to get started.  Perhaps if needed for
	# improvements or outright fixes, we can evolve it as we go if we want.)

	# TODO: maybe note that Values should only communicate with the external
	# world through its designated linker table / region.

	# TODO: Hmm, so maybe what we could do here is assume the supplied ‘f’ is a
	# nested value and the external source already handled linking for us
	# (caller's responsibility or whatever).  And then if the ‘f’ reference is
	# evaluated, it just means we replace the ‘f’ reference with the value of
	# ‘f’, the data.  We just copy it over into ourselves 20:42.

	# The executor hands over control to us to advance a frame, with
	# parameters:
	# 	%rdi: Return after the frame is advanced and finalized.

	# Test the application bit.  We're done (we don't change anything in us) if
	# there's no application; just leave the lambda unchanged.
	leaq _ns_fun_ahc_example_swap_receiver(%rip), %rsi
	movq (%rsi), %rsi
	testq $0x4, %rsi
	jnz 9f
	nop
	jmp *%rdi
9:
	nop

	# We're performing function application, and we have an argument available.
	# Build up ‘buffer1’ so we can swap the buffer pointer to activate it.

	# TODO
	hlt
	nop
_ns_fun_ahc_example_swap_impl_buffer1:
	# Just get a bunch of nops to make this the same size (a little larger if
	# needed to make things fit) as buffer0.
	hlt
	nop
	.skip ((_ns_fun_ahc_example_id_impl_buffer1 - _ns_fun_ahc_example_id_impl_buffer0 + 1)/2), 0x90
_ns_fun_ahc_example_swap_end:

# (So when you signal a bit, you are telling the executor to treat the frame
# advancement as performing function application.  So if you copy an unapplied
# lambda, just apply it by supplying the input value (type&executor-defined,
# but often just a Value-relative pointer) and then setting the application bit
# the options bitfield.)
#
# (Then you could even have multiple function arrow types (type system
# feature), perhaps that the executor treats differently.  Or perhaps by
# supplying our machine code executor, we are already being part of the
# executor's machine code execution.)

# TODO:
#_ns_fun_ahc_example_list:

# TODO:
# A value that functions like a minimal pointer, although here I guess a
# pointer is not 1 u64 but 16 u64s plus machine code.

# TODO: after a few examples, probably the plan is to make as needed what may
# be handy for a compiler, e.g. starting with option parsing for the CLI
# interface.  (Could even have additional front-ends!)

# (TODO Note: oh, brilliant; then function application can be modeled as a mutation.
# Normally what you do is copy a top-level declaration locally and then mutate
# your local, exclusive copy.  (Detail: is it the top-level value that
# primarily handles the copying, or the caller of that value?).  Probably how I
# imagine it'd work is in the normal case (but you can probably still just copy
# the bytes manually) the caller writes or invokes a copy request from the
# top-level value, and then the top-level value does a copy of itself to the
# requested location.  Brilliant!  (BTW, the compiler can note where a lambda
# actually implies a ‘dup’: that is, something like \x -> f x x, where the ‘x’
# appears more than once on the right-hand side (or zero, I guess for
# destruction)).  Brilliant!  I think this can work brilliantly!  Woo!
# 2022-07-08.)

# (TODO then I guess you have have a psuh-based computational step model, where
# an executor can be pointed at a Value, and one step of execution or mutation
# occurs.  Writes are a primitive in this model.  Reads outside the local Value
# don't need to be primitive (e.g. continuously write to a queue until the
# target Value signals back to us with its own write that it received our
# request, and then we can stop our write request), but a non-primitive read
# can be (and is) provided as a convenience.)

# TODO unary meta len (2^ number of ones before the 0), then length.  (Length
# includes the unary meta len (len of len).

# Notes: so function application is a mutation.  While technically you could
# apply a top-level function in-place, if it's top-level normally you'd just
# get a copy of the top-level function to yourself and then mutate / apply
# in-place.  However, by a linear type model, ownership is preserved, and
# primitives are supported to support duplication and destruction.  Perhaps it
# may be thought of as the internal state, or observability, or value in time
# is preserved, where it is used exactly once (dup and del can branch and end);
# access to excluded to non-owners.  Ownership may be integrated with an FRP
# framework with an interpretation that a time value for a time-varying value
# can only be supplied once, and also resources must be cleaned.
#
# Wow, so then, if you have ‘\x -> f (g x) h (i 8 x j)’, then actually there
# must be implied ‘dup’s in there, that consume and then produce 2 outputs,
# sort of like ‘x -> (x, x)’.  (The implicit and unused inputs can be
# interpreted as being consumed but producing exactly the same output,
# unchanged.)
#
# So then I guess with linear types, probably a common operation is probably
# just to move around or reorient inputs and resources, either directly or with
# references or pointers to them, and dup and cleanup operations can handle
# those specific operations.
#
# So to start out encoding a particular function or example or whatever, say a
# simple application of \x f -> f x, basically the value should be such that
# when you copy it, and you apply one computational step, the value becomes ‘f
# x’, once you apply it.  (And the machine code of a value means that when you
# execute it, exactly one computational step is performed.  (This may also be
# helpful for real-time computational analysis.  (!).))
#
# So a Value is something you can apply a computational step for.  When it
# returns, one computational step has been performed.  A constant value can be
# executed but it just doesn't change itself when it is executed.  Probably a
# Value unapplied just leaves itself (the state) unchanged when executed, and
# when a value is specified to apply, it performs the application.  The details (e.g. how to encode function application, how to
# encode _) can be left to the implementation, but this is the general idea.
# And technically a Value can take a more strict-evaluation approach by jumping
# to another Value, telling it where to return when done, blocking until its
# execution step is performed (and thus I think ‘frame’ is a better term than
# ‘step’, becausea frame is composable and can be highly large and complex and
# take a long time to execute or evaluate, whereas a step or cycle is meant to
# be predictably trivial and not composable or nestable.).
#
# Now this function application model here itself doesn't specify (that can be
# left up to other components in the ABI, or the executor to specify) how
# access is managed, although probably rarely does a Value directly write to
# other Values, except for a special region within the external Value
# designated specifically for other Values to write to as signals, for when
# that Value is executed.  Instead normally a Value just copies into itself.

# Okay, so to sort of ‘test’ out this design, maybe try to challenge it with
# potential critiques.  So then is this just factories?  I mean, copying other
# Values is a big part of it, but I think it's a little different because
# you're doing more direct copying than having specially crafted ways of
# constructing things.  Are Values just Java objects?  I mean, if I were to
# take what I have so far, what's the closest Java equivalent?  (Or C++?)
# Probably as classes with static methods whose only constructor (at least by
# default) is to do a direct copy of the whole class.  Reproduce itself, and
# then further mutation is possible.
#
# BTW, the system can provide concurrency primitives like our own fork and join
# procedures, and an executor can do something analagous to a ‘dup’ except for
# executors (threads) instead of certificates of exclusive access control
# (like titles of ownership).
#
# And BTW, if you have a pointer in linear types, maybe you could just have its
# own dup and del operations, where dup lets the pointer be duplicated
# (separately from what is pointed to), which increments some counter to track
# how many references there are, and del decrements it to perform a del when
# the counter reaches 0.  Like a C++ shared pointer.
#
# So while you can probably implement this model I'm proposing here - its
# general idea - in a language like Java, the typical pattern design patterns
# I'm imagining is probably not quite the same as how OOP is typicall done,
# although of coruse you may think of bridges or isomorphisms that let you
# reduce one to the other, or so it seems to me right now, anyway.

# Now to get started, maybe I'll just come up with a simpe function application
# encoding, and while I'm getting started, maybe evolve it as we go.
#
# So for a _frame_ (I'm calling it a ‘frame’ now, not a ‘step’, since it can be
# composed to become quite large: in a frame, a Value evolves itself until it
# ‘finalizes’ its new state possibly from output, although the executor,
# depending on how the executor is specified, may or may not be able to observe
# non-finalized states, intermediate states between certain FRP time inputs),
# let's go with this: check the Value's input zone for an input.  If there's no
# input, the lambda remains unchanged like a constant and the executor just
# returns in order to finalize the frame.  But if there's input, the Value
# changes and mutates itself before being ‘finalized’.

# TODO: clean up these docs when you have time:

# TODO unary meta len, but on this build platform only the following is
# supported (only length):
# 0b1110 XXXX = 0xEX, for up to 7 bits of data after the length header (limited in u8).
# 0b1111 0XXX  XXXX XXXX = 0xF_ 0xXX, for up to 11 bits of data after the length header. (u8 length in u16).
# 0b1111 10XX  XXXX XXXX   XXXX XXXX  XXXX XXXX = 0xF_ 0xX…, for up to 26 bits of data after the length header. (u16 length in u32).
# 0b1111 110X  XXXX XXXX   XXXX XXXX  XXXX XXXX  (32x X) = 0xF_ 0xX…, for up to 57 bits of data after the length header. (u32 length in u64).
# 0b1111 1110  XXXX XXXX   XXXX XXXX  XXXX XXXX  (32x X) (64x X) = 0xFE 0xX…, for up to 121 bits of data after the length header. (u64 length in u128).
#
# (Yes, this is bit-level, not byte-level.  But with just the u64 in u128
# version, it's also just the byte-level one we support.)
#
# So only u64 length in a u128 header is supported, 0xFE 00 00 00  00 00 00 00,
# then u64 length, including header.

# A basic identity function Value: \x -> x
#
# A Value.  16 u64s (128 bytes) followed by the implementation, often machine
# code for an executor to advance a frame in time by one frame.
_ns_fun_ahc_example_id:
.global ns_fun_ahc_example_id
.set ns_fun_ahc_example_id, (_ns_fun_ahc_example_id - ns_fun_ahc_module_begin)
.set _ns_fun_ahc_example_id_size, (_ns_fun_ahc_example_id_end - _ns_fun_ahc_example_id)
_ns_fun_ahc_example_id:
	.byte 0xFE, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x00  # (  0) u64 prefix, bytes (BE)
	.byte (((_ns_fun_ahc_example_id_size << 3) >> 56) & 0xFF)  # (  8) u64 bit size, BE
	.byte (((_ns_fun_ahc_example_id_size << 3) >> 48) & 0xFF)
	.byte (((_ns_fun_ahc_example_id_size << 3) >> 40) & 0xFF)
	.byte (((_ns_fun_ahc_example_id_size << 3) >> 32) & 0xFF)
	.byte (((_ns_fun_ahc_example_id_size << 3) >> 24) & 0xFF)
	.byte (((_ns_fun_ahc_example_id_size << 3) >> 16) & 0xFF)
	.byte (((_ns_fun_ahc_example_id_size << 3) >>  8) & 0xFF)
	.byte (((_ns_fun_ahc_example_id_size << 3) >>  0) & 0xFF)
	.byte 0x83, 0xF1, 0x8A, 0x24,  0x57, 0xAA, 0x2B, 0x4B  # ( 16) u64 magic, bytes (BE)
	.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x01  # ( 24) u64 version, BE
	.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x09  # ( 32) u64 machine, BE
	.byte 0x00, 0x00, 0x00, 0x00,  0x00, 0x00, 0x00, 0x00  # ( 40) u64 options, BE
	.quad (_ns_fun_ahc_example_id - ns_fun_ahc_module_begin)  # ( 48) i64 linker_table, host endianness (jumps to ‘module_route’)
	.quad 0                                                # ( 56) u64 metadata_size, host
	.quad 0                                                # ( 64) i64 metadata, host
	.quad 0                                                # ( 72) i64 type, host (Value-relative pointer to embedded Value, or possibly external Value to save space)
	.quad (_ns_fun_ahc_example_id_impl - _ns_fun_ahc_example_id)  # ( 80) i64 impl pointer, host
	.quad (_ns_fun_ahc_example_id_impl - _ns_fun_ahc_example_id_receiver)  # ( 88) u64 receiver_size, host
	.quad (_ns_fun_ahc_example_id_receiver - _ns_fun_ahc_example_id)  # ( 96) i64 receiver
	.quad 0                                                # (104) u64 reserved0
_ns_fun_ahc_example_id_receiver:
	.quad 0                                                # (112) Status bitfield.  Bit 2 for funapp.
	.quad 0                                                # (120) Data / pointer (Value-relative).
_ns_fun_ahc_example_id_impl:
	# (128) Implementation, swappable (a la OpenGL's double buffering) pointer.
	jmp _ns_fun_ahc_example_id_impl_buffer0
	nop
_ns_fun_ahc_example_id_impl_buffer0:
	# TODO
	hlt
	nop

	# Test the application bit.  We're done (we don't change anything in us) if
	# there's no application; just leave the lambda unchanged.
	leaq _ns_fun_ahc_example_swap_receiver(%rip), %rsi
	movq (%rsi), %rsi
	testq $0x4, %rsi
	jnz 9f
	nop
	jmp *%rdi
9:
	nop

	# We're performing function application, and we have an argument available.
	# Build up ‘buffer1’ so we can swap the buffer pointer to activate it.

	# TODO: perhaps just copy the ‘x’ pointed-to data ... what now?  Either
	# jump to an embedded Value's implementation on buffer1, or probably better would be this, can we just replace ourselves with ‘x’?  The former seems poorly efficient.

	# TODO So we'd like to have a technique in place to replace it, and
	# probably at least the structure should be modified (if not going further
	# than this) such that it's just size and magic identifier followed by a
	# Value-relative pointer to the beginning of the rest of the header, so
	# that a Value can be updated to a new Value by writing the rest of the
	# header in another location, (we may need to expand (we can shrink
	# afterwards sometimes)), and then simply updating this u64 pointer.  Then
	# we _.  (If we need to expand such that the parent Value memory region
	# needs to itself relocate, we shouldn't need to do anything special,
	# since probably we are agnostic to locations of anything outside our Value
	# except for the linker/router table.)
	#
	# TODO In this manner, a Value doesn't need to be relocated, rather than
	# consuming a Value and producing a new one, destroying the old one,
	# freeing up the new Value from the constraint of having to be located in
	# the same location as the old one.
	#
	# TODO In fact either style of Value would probably work: a Value when mutating
	# itself to provide the next iteration may end up with the new finalized
	# Value with the same base Value pointer, or it may end up in a new
	# location - probably the executor calls the machine code in the
	# implementation section of a Value, and the implementation returns with
	# the location of the new Value right after destroying the old (which
	# sometimes may be only conceptual, as actually the location is the same
	# and it just updated itself in-place).  The type system (e.g.  with linear
	# types) can provide further mechanisms to coordinate relocation.
	#
	# TODO lots of good ideas here.  Let's hope the progress continues.

	# TODO: another type: Values that work like pointers or references to other
	# Values, to look them up, returning a Value-relative, but external,
	# reference?  But there are a few issue here that would need to be worked
	# out: with linear types, a consumed Value could easily be destroyed,
	# invalidating pointers to it.  I guess conventionally it's the caller's
	# responsibility to ensure that the mutated Value's new location is
	# appropriately managed.

	# TODO I guess another scenario to consider is there being multiple
	# top-level references, and other modules to refer to module A's top-level
	# Values, which is equivalent to pointers through it, albeit pointers
	# relative to module A.  (Ignore module A's access management for now.)  If
	# indeed you mutate A's top-level attribute / definition by jumping the
	# executor to it, then (while it can also do an in-place update, e.g.
	# especially if no change is made to save on resources), there's a risk and
	# possibility that the new top-level attribute could be at a new location,
	# invalidating other effective pointers to A's top-level definition.
	# (Probably the design is better if the callee doesn't manage external
	# references to it, but referencers manage it?, e.g. a caller handles an
	# updated reference, but let's think through this.)
	#
	# I guess a few approaches could work:
	# - The type system could add a constraint that certain Values may not be
	# executed or mutated, however these Values may be copied.
	# - A's top-level Values normally should not be reached directly, but only
	# through A's top-level interface (sort of like our current module
	# routers), and then A tracks its definitions' relocations when they
	# happen, and presumably you just have basically an enumeration or string
	# table like an associative map of strings to Value locations relative to
	# the module.
	# - If neither approach is taken, then ultimately the caller of A's
	# definitions will need to add or integrate its own mechanisms to ensure
	# that its pointer is valid, so that conventionally this is the
	# responsibility of the pointer Values, not A's definitions.

	# Probably also one major mechanism to contemplate designs for then is
	# memory allocation, e.g. how can a Value's implementation know what memory
	# is available.

	# Probably what would be helpful is to just have a parent (the module
	# Value) interface with Values, and probably the first feature for this is
	# managing memory, e.g. if more memory needs to be allocated.

	# TODO
	hlt
	nop
_ns_fun_ahc_example_id_impl_buffer1:
	# Just get a bunch of nops to make this the same size (a little larger if
	# needed to make things fit) as buffer0.
	hlt
	nop
	.skip ((_ns_fun_ahc_example_id_impl_buffer1 - _ns_fun_ahc_example_id_impl_buffer0 + 1)/2), 0x90
_ns_fun_ahc_example_id_end:

.global ns_fun_ahc_module_end
ns_fun_ahc_module_end:
