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
