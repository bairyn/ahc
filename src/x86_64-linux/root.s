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
# - A value-static malloc API.  TODO: choose details and an implementation here.
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
# in the malloc API; we'll fill in the details (TODO).)

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
