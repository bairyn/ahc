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
# - An i64 parent vpointer (if you add this to the Value base address, you get
#   the Value's parent base address).
# - An i64 implementation vpointer (if you add this to the Value base address,
#   you get the start of the implementation region normally entirely contained
#   in the Value, and normally the implementation starts with a size u64 of the
#   how big the implementation region is in bytes; normally the type and
#   executor and platform combination specifies the meaning of the
#   implementation region; often you can set up the executor state
#   appropriately (e.g. set the right registers appropriately) and then jump to
#   a region within the implementation region to advance the Value by one frame
#   like a State monad, construct the new state in a new implementation region
#   that can be finalized by updating the impl pointer afterwards, and
#   optionally produciiing additional output in a format specified by the same
#   3-tuple combination).
# - A vpointer to an implementation-defined malloc API: basically, the
#   beginning of a record of several function pointers (like a struct of
#   several i64s), where the function pointers are themselves vpointers,
#   relative to the Value (not the record).  The record has fixed size and its
#   lifetime must be the same as the Value's lifetime.  (Essentially it's 3
#   i64s.  They may be short and delegate to a parent Value's API.  The type
#   and executor spec will probably include a base Value register or other
#   working storage unit in the state or environment that can be used to obtain
#   a parent API.  The 3 actions are malloc, mfree, and mrealloc.)
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

# TODO: oops, you forgot linker tables.  Probably just say the type (and
# platform) determines the linker table format, e.g. what the struct fields
# are, how they're formatted, and what they mean).

# Configure the platform.
.code64

# Put all code and data in the same section.
.text






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
