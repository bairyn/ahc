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
# 		// Header.          // (Byte offset.)
# 		u64 prefix;         // ( 0) Specially crafted constant.  (Byte offset 0.)
# 		u64 size_bits;      // ( 8) Size of the whole value, including this header, in bits.
# 		u64 magic;          // (16) Magic FunAHC tag, a constant value.
# 		u64 version;        // (24) Encoding format.
# 		u64 machine;        // (32) Platform encoding; set to ‘9' for ‘x86_64-system’.
# 		u64 options;        // (40) Options bitfield.
# 		i64 linker_table;   // (48) Relative to the beginning of this value, pointer to a table in memory that can point to external references outside this value.
# 		u64 metadata_size;  // (56) Size of metadata.
# 		i64 metadata;       // (64) Value-relative metadata pointer (embedded in this value).
# 		i64 type;           // (72) Value-relative type pointer (embedded in this value).
# 		i64 impl;           // (80) Value-relative value implementation pointer (embedded in this value), normally $0x80, right after the header.
# 		u64 reserved0;      // (88) Reserved for future use.
# 		// Implementation.
# 		u8 code[];          // (Byte offset 96.)
# 	};
# 	typedef struct value_s value_t;
.global ns_fun_ahc_example_3
.set ns_fun_ahc_example_3, (_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin)
.set _ns_fun_ahc_example_3_size, (_ns_fun_ahc-example_3_end - _ns_fun_ahc_example)
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
.quad (_ns_fun_ahc_example_3 - ns_fun_ahc_module_begin)

# So far, at byte offset 56:
.quad $0  # Size (in bytes) of value metadata.  Optionally the implementation
          # may make this non-zero to mean something.  If desired inside the
          # type system, this can include additional type system information.
# So far, at byte offset 64:
.quad $0  # Value-relative pointer (in bytes) of value metadata.  Optionally the implementation may make this non-zero to mean something.  Points to a region within the Value.
# So far, at byte offset 72:
.quad $0  # Value-relative pointer to the type, which is a nested Value embedded within this one.
# So far, at byte offset 80:
.quad $0x80  # Value-relative pointer to the implementation-specific/defined data, code, or encoding of the Value, conveniently right after the header ends.  Often this is machine code.
# So far, at byte offset 88:
.quad $0  # Reserved for future use; set to 0.  Do note the space cost this
          # adds to values.  But I would guess this isn't too difficult to
          # adapt to.
# So far, at byte offset 96:
# The header is now done.  It had 12 u64s (uint64_t)s.

_ns_fun_ahc_example_3_impl:
# TODO
	hlt
	nop
_ns_fun_ahc_example_3_end:

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

.global ns_fun_ahc_module_end
ns_fun_ahc_module_end:
