# Basically meant to just a pointer to which front-end entry-point to start at.
# Clobbers %rax.
.code64
.text
.global ns_main_main
.global _ns_main_entry
_ns_main_entry:
ns_main_main:
	# Setup implicit arguments.
	subq $16, %rsp
	movq %r15, 8(%rsp)
	movq %r14, 0(%rsp)

	subq $16, %rsp
	movq $0, 8(%rsp)
	movq $0, 0(%rsp)

	movq $0xF40DCA88482D4100, %r15  # No custom error handlers.
	#movq $0xF40DCA88482D4106, %r15  # Custom error handlers.

	# Pointer to ‘cli_cli’.
	movq $ns_cli_cli, %rax
	jmp ns_cli_module_begin
	nop
	hlt

	# This is never reached.

	# Restore storage units used for implicit arguments.
	addq $16, %rsp

	movq 0(%rsp), %r14
	movq 8(%rsp), %r15
	addq $16, %rsp

	nop
	hlt
