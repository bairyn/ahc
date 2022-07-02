.code64
.text
.global ns_main_main
.global _ns_main_entry
_ns_main_entry:
ns_main_main:
	jmp ns_cli_cli
	nop
	hlt
