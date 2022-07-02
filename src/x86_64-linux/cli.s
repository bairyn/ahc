# CLI front-end.

# Configure the platform.
.code64

# (See note in ‘system’ for having a module router instead of a separate
# ‘.data’ section.)
#.data
.text

.global ns_cli_module_begin  # This jumps to ‘route’ too if you call it.
ns_cli_module_begin:
	.byte 0xEB, 0x36  # skip 54 bytes (jump) if execution begins here.
	.byte 0x90, 0xF4, 0x00, 0x0E  # Pad to 8 bytes, and 0x0E is like a version.
	.byte 0x00, 0x00
.global ns_cli_module_size
.set ns_cli_module_size, (ns_cli_module_end - ns_cli_module_begin)
.global ns_cli_module_size_field  # Should be at offset 8 relative to ‘begin’!
ns_cli_module_size_field:
	.quad (ns_cli_module_end - ns_cli_module_begin)  # Size.
	.quad 0x1324ABBC  # ABI.
	.quad 0  # Module hash, sha256sum of module string with a NULL hash field and NULL external module references.  TODO; just 0 until implemented.
	.quad (_module_name_end - _module_name)  # Size of the module name string.
	.quad (_module_name - ns_cli_module_begin)  # Module-relative offset to the siz of the module name string.
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
ns_cli_module_route:  # Must be at offset 54.
	leaq ns_cli_module_begin(%rip), %r11
	addq %r11, %rax
	jmpq *%rax
	nop

# Module dependencies.  Run-time re-linking and relocation will need to handle
# this.
_system:
	jmp ns_system_x86_64_linux_module_begin
	nop
	hlt
_util:
	jmp ns_util_module_begin
	nop
	hlt

_mod_dep_end:
.quad 0x12342345
.quad 0

# Now the .data stuffs.

_module_name:
	.ascii "cli"
_module_name_end:

todo:
#.ascii "It starts.\n"  # 16 offset, size 11.
.ascii "It starts (code     ).\n"  # 16 offset, size 23.
.byte 0x00

ns_cli_err_msg_memory_preliminary_checks_size:
	.quad (ns_cli_err_msg_memory_preliminary_checks_end - ns_cli_err_msg_memory_preliminary_checks)
ns_cli_err_msg_memory_preliminary_checks:
	.ascii "Error: cli startup error: prelimary base malloc and mfree checks failed; could not validate we have a working memory management system.\n"
	.byte 0x00
ns_cli_err_msg_memory_preliminary_checks_end:

.text
# A front-end: the CLI.
#
# This doesn't return.
.global ns_cli_cli
.set ns_cli_cli, (_ns_cli_cli - ns_cli_module_begin)
_ns_cli_cli:
	# Make sure we can return.
	# Load the return address (continuation) into ‘rdi’.
	leaq 9f(%rip), %rdi
	movq $ns_util_can_cont, %rax
	jmp _util
9:
	nop

	# Make sure we can write.
	leaq 9f(%rip), %rdi
	movq $ns_util_system_verify_writeable, %rax
	jmp _util
9:
	nop

	# Preliminarily check we can malloc and free.
	movq $0, %rcx
	movq $0, %rdx
	movq $8388608, %rsi  # Get 1MiB.
	#movq $8388607, %rsi  # Uncomment to a not-divisible-by-8 error.
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_base_malloc_require, %rax
	jmp _system
9:
	nop

	movq %rdi, %rdi  # size, in bits
	movq %rsi, %rsi  # pointer

	# Validate size of the allocation.
	cmpq $8388608, %rdi
	jz 1f
0:
	# Error: prelimiary checks failed!
	movq $2, %rdi
	leaq ns_cli_err_msg_memory_preliminary_checks(%rip), %rsi
	leaq ns_cli_err_msg_memory_preliminary_checks_size(%rip), %rdx
	movq (%rdx), %rdx
	movq $ns_system_x86_64_linux_exit_custom, %rax
	jmp _system
	nop
	hlt
1:

	# Validate we can write and read to and from our allocation.
	movb $0x03, 0(%rsi)        # First byte.
	movb $0xE7, 1(%rsi)        # Second byte.
	movb $0xD9, 1048575(%rsi)  # Last, 1,048,576-th byte.
	#movb $0xD9, 1048576(%rsi)  # Uncomment for what should commonly be a segfault error.

	movb 0(%rsi), %dil
	cmpb $0x03, %dil
	jnz 0b

	movb 1(%rsi), %dil
	cmpb $0xE7, %dil
	jnz 0b

	movb 1048575(%rsi), %dil
	cmpb $0xD9, %dil
	jnz 0b

	# Free.
	movq $0, %r8
	movq $0, %rcx
	movq %rsi, %rdx
	movq $8388608, %rsi  # Free our 1MiB allocation.
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_base_malloc_require, %rax
	jmp _system
9:
	nop

	jmp 0f
1:
	# A child thread will run this briefly.
	#jmp 1b  # Uncomment to instead let the thread join hang forever.
	movq $ns_system_x86_64_linux_exit_success, %rax
	jmp _system
	nop
0:

	# Test we can start and join threads.
	movq $0, %rdx
	leaq 1b(%rip), %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_fork_require, %rax
	jmp _system
9:
	nop

	# Now join.
	leaq 9f(%rip), %rdx
	xchgq %rdi, %rdx
	jmpq *%rdx
9:
	nop

	# Make sure we can sleep for a nanosecond.
	movq $1, %rdx  # 1ns
	movq $0, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	jmp _system
9:
	nop

	# Make sure we can sleep for 0 nanoseconds.
	movq $0, %rdx  # 0ns
	movq $0, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	jmp _system
9:
	nop

	# Uncomment the jmp to sleep for 2.5 seconds.
	movq $500000000, %rdx  # 500ms
	movq $2, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_monotonic_nanosleep, %rax
	#jmp _system
9:
	nop

	# TODO test ns_system_x86_64_linux_print_u64
	movq $1324, %rcx
	movq $4, %rdx
	leaq todo(%rip), %rsi
	addq $16, %rsi
	leaq 9f(%rip), %rdi
	movq $ns_system_x86_64_linux_print_u64, %rax
	jmp _system
9:
	nop

	# TODO: don't bypass system.s; use the layer to handle syscalls.
	movq $1, %rax  # write
	movq $1, %rdi
	leaq todo(%rip), %rsi
	#movq $11, %rdx
	movq $23, %rdx
	syscall

	movq $ns_system_x86_64_linux_exit_success, %rax
	jmp _system
	nop

	jmp ns_cli_cli
	nop

.global ns_cli_module_end
ns_cli_module_end:
