# Utilities.

# Configure the platform.
.code64

# Can we return?
.global ns_ahc_can_cont
ns_ahc_can_cont:
	jmp %rdi

# Make sure that the code can rewrite itself.
.global ns_ahc_system_verify_writeable
ns_ahc_system_verify_writeable:
	# TODO
	jmp %rdi
