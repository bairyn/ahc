# The implementation of Values is implementation-defined.  There are many ways
# to design an implementation.
#
# In our implementation, we make it part of the platform, executor, and types.
#
# Essentially, a Value works like an object, where each Value is basically a
# region in memory.  It can be uniquiely identifed by its position in memory,
# normally identified relative to its parent value.  Each Value has a size for
# the region it spans.  A value can have embedded Values, and normally each
# Value (in our own implementation we chose) has exactly one parent, normally
# referring to the nearest Value with overlapping memory coverage.
#
# A Value, in our implementation, and in the context of our own platform,
# executor, and types that we deal with, has data that follows the following
# format:
# - A u64 at the very beginning that represents the size of the region in
#   bytes.
# - An i64 frame vpointer (if you add this to the Value base address, you get
#   the start of the last/current finalized frame region normally entirely
#   contained in the Value), where the platform, type, and executor can specify
#   the format and encoding of the frame except we specify a minimum format
#   here, which those can extend (this can be implemented like double buffering
#   in OpenGL):
#   - u64 frame size
#   - i64 linker table vpointer, same lifetime as Value:
#     - Type (and executor and platform) specified, but normally it starts with
#       the following:
#       - u64 size
#       - i64 vpointer (not linker table) parent
#       - i64 vpointer - the value's malloc, as specified later.  (Unless
#         otherwise specified and configured by e.g. arguments, a new
#         allocation is made within the Value's region, where the Value may be
#         expanded if necessary, but the the later documentation for more
#         information.  This might delegate to a parent malloc API.)
#       - i64 vpointer - mfree, as specified later.  (Can free the Value this
#         ‘mfree’ belongs to.)
#       - i64 vpointer - mrealloc, as specified later.
#       - Optionally, if the Type (and executor and build platform) specify it,
#         an i64 vpointer to a destructor procedure, to free the Value.
#       - Optionally, an i64 vpointer to a dup procedure, to copy the Value (if
#         it's copy-once, then the copied value will not have its own dup/copy
#         action.)
#   - i64 implementation vpointer; the type and platform and executor determine
#     the format of the region.  Often it starts with a size u64, but sometimes
#     this isn't so.
#
# At runtime, we do not by default require types, but metadata including type
# information can be added to a Value's implementation region according to
# however the type system and executor wishes to specify.  We don't specify it
# here.
#
# Now other components may add their own requirements to this structure, e.g.
# the malloc probably will need to keep track of allocations.  Probably it'll
# work with initial, say, 4KiB allocations where, as they grow, if they run out
# of capacity, a new allocation twice the size is made for it, it is copied
# over, and the old one freed.  (This can just be data embedded like a closure
# in the malloc API.)

# Module type:
#
# Here we document the Module type.
#
# Essentially, it means the Value's primary purpose is to provide a region in
# memory containing a bunch of Values (basically everything that the module
# exports or that supports the module), and the implementation (unlikely
# implementations for most other types) is directly an embedded Value, of type
# RawModuleIndex, of a more primitive form (so that we can more conveniently
# bootstrap up modules, although you can also add your own module index format
# with a better type).  The RawModuleIndex type means that the implementation
# follows the format of u64 size in bytes, u64 length, null-terminated ordered array of module
# vpointer offsets to each module member, null-terminated ordered array of
# null-terminated c-strings of module-local symbols that name that member, u64
# null.

# Configure the platform.
.code64

# Put all code and data in the same section.
.text

# TODO: maybe for our own Prelude to get started?:
# - Nat
# - List
# - Word8
# - various functional utilities
#
# - Fin
# - Either
# - ByteString
# - Word64
# - Integer
# - PropEq






# OLD


# Placeholder for a root value.
.global ns_root_root_value
ns_root_root_value:
	# Platform-specific u64 byte size and Value-relative offset to the header.
	.quad (_ns_root_root_value_end - ns_root_root_value)
	.quad (_ns_root_root_value_header - ns_root_root_value)

_ns_root_root_value_header:
	# Linker table, Value-relative offset (vpointer).
	.quad (_ns_root_value_ltable - ns_root_root_value)
	# Optional metadata and annotations vpointer (itself a Value).
	.quad (_ns_root_value_metadata - ns_root_root_value)
	# Type vpointer (itself a Value).
	.quad 1  # TODO (Module type.)
	# Executor vpointer.
	.quad (_ns_root_root_value_executor - ns_root_root_value)

_ns_root_root_value_metadata:
	.quad 8  # Size (we have nothing else).

# TODO: in ltable, probably the parent of the root value is itself.

_ns_root_root_value_end:
