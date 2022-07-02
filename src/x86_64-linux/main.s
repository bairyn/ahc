# Basically meant to just a pointer to which front-end entry-point to start at.
# Clobbers %rax.
.code64
.text
.global ns_main_main
.global _ns_main_entry
_ns_main_entry:
ns_main_main:
	movq $ns_cli_cli, %rax
	jmp ns_cli_module_begin
	nop
	hlt
