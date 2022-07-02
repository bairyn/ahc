# This module provides procedures to interface with the kernel through
# syscalls.

# See ‘arch/x86/entry/syscalls/syscall_64.tbl’ (thanks,
# https://unix.stackexchange.com/a/499016).

# See also the Intel(R) 64 and IA-32 Architectures Software Developer's Manual,
# Combined Volumes: 1, 2A, 2B, 2C, 2D, 3A, 3B, 3C, 3D, and 4 PDF document.

# Configure the platform.
.code64

# We could have ‘.data’ here, but for modularity and portability, put all of
# this module in the same section, so it can be e.g. copied and referenced from
# a common base point, ‘module_begin’, with ‘module_size’.  (E.g. possibly use
# this as part of a compiled RTS.)
#.data
.text
.global ns_system_x86_64_linux_module_begin  # This jumps to ‘route’ too if you call it.
ns_system_x86_64_linux_module_begin:
	.byte 0xEB, 0x1E  # skip 30 bytes (jump) if execution begins here.
	.byte 0x90, 0xF4, 0x00, 0x0E  # Pad to 8 bytes, and 0x0E is like a version.
	.byte 0x00, 0x00
	.quad 0
	.quad 0
.global ns_system_x86_64_linux_module_size  # Should be at offset 8 relative to ‘begin’!
ns_system_x86_64_linux_module_size:
	.quad (ns_system_x86_64_linux_module_end - ns_system_x86_64_linux_module_begin)
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
ns_system_x86_64_linux_module_route:
	leaq ns_system_x86_64_linux_module_begin(%rip), %r11
	addq %r11, %rax
	jmpq *%rax
	nop

# Module dependencies.  Run-time re-linking and relocation will need to handle
# this.
# (‘system’ depends on no other module.)

_mod_dep_end:
.quad 0

# Now the .data stuffs.

# 152 bytes of data to restore segv trap.
ns_system_x86_64_linux_sigaction_restore_segv:
	.quad 0x0  # SIG_DFL = 0
	.rept (152-8)
	.byte 0x00
	.endr
# 152 bytes of data (per-process-instantation) to set segv trap.
ns_system_x86_64_linux_sigaction_static_set_segv:
	.quad 0x0  # Will be overwritten exactly once per process instantiation.
	.rept (152-8)
	.byte 0x00
	.endr
# (per-process-instantation) to set segv trap.
ns_system_x86_64_linux_sigaction_static_set_segv_cont:
	.quad 0x0
# (per-process-instantation) to set segv trap.
ns_system_x86_64_linux_sigaction_static_set_segv_data:
	.quad 0x0

ns_system_x86_64_linux_err_msg_segv_trap_set_size:
	.quad (ns_system_x86_64_linux_err_msg_segv_trap_set_end - ns_system_x86_64_linux_err_msg_segv_trap_set)
ns_system_x86_64_linux_err_msg_segv_trap_set:
	.ascii "segv trap set error: rt_sigaction failed!  Could not trap SIGSEGV.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_segv_trap_set_end:

ns_system_x86_64_linux_err_msg_segv_trap_restore_size:
	.quad (ns_system_x86_64_linux_err_msg_segv_trap_restore_end - ns_system_x86_64_linux_err_msg_segv_trap_restore)
ns_system_x86_64_linux_err_msg_segv_trap_restore:
	.ascii "segv trap set error: rt_sigaction failed!  Could not restore SIGSEGV handler.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_segv_trap_restore_end:

# Note: sizes:
# 	λ length $ show (maxBound :: Word8)
# 	3
# 	λ length $ show (maxBound :: Word16)
# 	5
# 	λ length $ show (maxBound :: Word32)
# 	10
# 	λ length $ show (maxBound :: Word64)
# 	20

# Mutate at most once, static only for ns_system_x86_64_linux_verify_errno, to
# be set before quitting with an error.
ns_system_x86_64_linux_syscall_verify_builder_size:
	.quad (ns_system_x86_64_linux_syscall_verify_builder_end - ns_system_x86_64_linux_syscall_verify_builder)
ns_system_x86_64_linux_syscall_verify_builder:
	.rept 1024
	.byte 0x00
	.endr
ns_system_x86_64_linux_syscall_verify_builder_end:

ns_system_x86_64_linux_err_msg_fork_join_unknown_code_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_unknown_code_end - ns_system_x86_64_linux_err_msg_fork_join_unknown_code)
ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_offset:
	.quad 22
ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_size:
	.quad 10
ns_system_x86_64_linux_err_msg_fork_join_unknown_code:
	.ascii "fork join error (code           ): the ‘waitid’ syscall gave us a ‘code’ siginfo_t attribute that we don't recognize.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_unknown_code_end:

ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch_end - ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch)
ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch:
	.ascii "fork join error: the return address or %rip-based location appears to be clobbered after the ‘waitid’ syscall!\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch_end:

ns_system_x86_64_linux_err_msg_fork_join_exited_failure_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_exited_failure_end - ns_system_x86_64_linux_err_msg_fork_join_exited_failure)
ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_offset:
	.quad 22
ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_size:
	.quad 3
ns_system_x86_64_linux_err_msg_fork_join_exited_failure:
	.ascii "fork join error (code    ): the ‘waitid’ syscall revealed a child process exited with a non-zero exit code!\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_exited_failure_end:

ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_end - ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo)
ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_offset:
	.quad 23
ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_size:
	.quad 3
ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo:
	.ascii "fork join error (signo    ): the ‘waitid’ syscall gave us a signo we don't recognize!  Should be SIGCHLD = 17.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_end:

ns_system_x86_64_linux_err_msg_fork_join_waitid_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_join_waitid_end - ns_system_x86_64_linux_err_msg_fork_join_waitid)
ns_system_x86_64_linux_err_msg_fork_join_waitid:
	.ascii "fork join error: the ‘waitid’ syscall failed!\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_join_waitid_end:

ns_system_x86_64_linux_err_msg_clock_nanosleep_size:
	.quad (ns_system_x86_64_linux_err_msg_clock_nanosleep_end - ns_system_x86_64_linux_err_msg_clock_nanosleep)
ns_system_x86_64_linux_err_msg_clock_nanosleep:
	.ascii "monotonic_nanosleep error: the ‘clock_nanosleep’ syscall failed!  Failed to sleep.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_clock_nanosleep_end:

# Utility: digits
ns_system_x86_64_linux_util_digits:
.ascii "0123456789ABCDEF"
.byte 0x00

# Memory management metadata.

# CURRENTLY UNUSED.
#
# This is a root reference into the graph of base allocations of memory.  Each
# allocation begins with a fixed size header that just references up to 3 other
# allocations.  These pointers are after the size of this allocation.
ns_system_x86_64_linux_memory_pointer:
	.quad 0  # size
	.quad 0  # ref0
	.quad 0  # ref1
	.quad 0  # ref2

# (We could just write to machine code that has these error messages, but since
# we're at a low level, we may want to wait until we have higher level
# abstractions before we attempt that.)
ns_system_x86_64_linux_err_msg_base_malloc_error_size:
	.quad (ns_system_x86_64_linux_err_msg_base_malloc_error_end - ns_system_x86_64_linux_err_msg_base_malloc_error)
ns_system_x86_64_linux_err_msg_base_malloc_error:
	.ascii "base malloc error: mmap() failed!  Could not allocate memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_malloc_error_end:

ns_system_x86_64_linux_err_msg_base_mfree_error_size:
	.quad (ns_system_x86_64_linux_err_msg_base_mfree_error_end - ns_system_x86_64_linux_err_msg_base_mfree_error)
ns_system_x86_64_linux_err_msg_base_mfree_error:
	.ascii "base mfree error: munmap() failed!  Could not free memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_mfree_error_end:

ns_system_x86_64_linux_err_msg_base_malloc_4th_size:
	.quad (ns_system_x86_64_linux_err_msg_base_malloc_4th_end - ns_system_x86_64_linux_err_msg_base_malloc_4th)
ns_system_x86_64_linux_err_msg_base_malloc_4th:
	.ascii "base malloc argument error: the 4th argument must be zero!  Could not allocate memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_malloc_4th_end:

ns_system_x86_64_linux_err_msg_base_mfree_5th_size:
	.quad (ns_system_x86_64_linux_err_msg_base_mfree_5th_end - ns_system_x86_64_linux_err_msg_base_mfree_5th)
ns_system_x86_64_linux_err_msg_base_mfree_5th:
	.ascii "base mfree argument error: the 5th argument must be zero!  Could not free memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_mfree_5th_end:

ns_system_x86_64_linux_err_msg_base_malloc_enomem_size:
	.quad (ns_system_x86_64_linux_err_msg_base_malloc_enomem_end - ns_system_x86_64_linux_err_msg_base_malloc_enomem)
ns_system_x86_64_linux_err_msg_base_malloc_enomem:
	.ascii "Error: base malloc error: not enough memory, so mmap() failed with ENOMEM (12, %rax -12)!  Could not allocate memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_malloc_enomem_end:

ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_size:
	.quad (ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_end - ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible)
ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible:
	.ascii "base malloc error: invalid base_malloc arguments: the requested bits must be divisible by 8; be sure to specify length in bits, not bytes!  Could not allocate memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_end:

ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible_size:
	.quad (ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible_end - ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible)
ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible:
	.ascii "base mfree error: invalid base_mfree arguments: the requested bits must be divisible by 8; be sure to specify length in bits, not bytes!  Could not free memory.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible_end:

ns_system_x86_64_linux_err_msg_fork_size:
	.quad (ns_system_x86_64_linux_err_msg_fork_end - ns_system_x86_64_linux_err_msg_fork)
ns_system_x86_64_linux_err_msg_fork:
	.ascii "fork error: the ‘fork()’ syscall failed!  Could not fork.\n"
	.byte 0x00
ns_system_x86_64_linux_err_msg_fork_end:

# For our compiler, we only require the ability to read and write files and to
# exit.  We can work out the rest.  But we also add some concurrency support
# and shell support.  Also add a few other utilities as needed.
.text

# ################################################################
# Base.
# ################################################################

# Do nothing but return (or call an alternative continuation).
#
# Parameters:
# 	%rdi: return
.global ns_system_x86_64_linux_nop
.set ns_system_x86_64_linux_nop, (_ns_system_x86_64_linux_nop - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_nop:
	jmpq *%rdi
	nop

# ################################################################
# Memory management.
# ################################################################

# Instead of taking the time to think about alternative paradigms, I just
# followed a simple memory allocator that claims ranges of unclaimed virtual
# memory.  (If I had more time, it would be interesting to look for alternative
# approaches; but let's just get this going first.)

# On our platform, we already have at least a few options:
# - Pre-allocated, fixed-size memory.
# - ‘sbrk’
# - ‘mmap’

# We basically just make our own heap here.

# TODO flesh out; going to bed; but 2022-06-30 I had an insight, after thinking
# this through: I know you were trying to think of a quick and simple
# implementation (rather than spending extra time trying to design it *well*
# right now), e.g. allocations with the base allocations, but actually try to
# keep the platform-specific memory handling stuff as simple as possible, e.g.
# just have base allocations of large e.g. 1GiB allocations, and then
# optionally put a layer above it that's less tied to the platform that can
# make managing lots of small and big allocations and freeing easier.  (Plus,
# memory is a resource; it might be interesting to think about economic
# principles or maybe feedback to help restrict memory allocation as memory
# usage increases enough.)

# Get a chunk of memory, of largeish size, e.g. ~1GiB per allocation.
#
# You must track the metadata (especially the bytes reserved to pass onto the
# ‘free’ action) as the API is meant to be a minimal API layer around the
# platform.
#
# This is probably also unsuitable for making many small allocations, so for
# normal applications, you probably want to add an abstraction or layer on top
# of this one to handle many variable-size allocations.
#
# This call will exit with failure if memory cannot be allocated except when
# there is a shortage of memory to be allocated.  (In the future, support for
# overriding error handling is probably a good idea (TODO).)
#
# Allocations are anonymous and shared.
#
# Parameters:
# 	%rdi: Return with args:
# 		%rdi: Zero on non-fatal error (no available memory), one on success.
# 		%rsi:
# 			If non-fatal error, pointer to static data containing information
# 			about the error, currently just a null-terminated string.  If
# 			success, number of bits allocated (currently, equal to requested
# 			size).
# 		%rdx: Pointer in virtual memory to the beginning of the allocation.
# 			I recommend having the size of the allocation as the first uint64,
# 			then 3 uint64 pointers to other allocations, then your own base
# 			allocation header to customize how you want to handle and track
# 			allocations at runtime, but how you use this allocation usp to you,
# 			but since we don't track the allocations and metadata ourselves
# 			(although we might be able to get information from the kernel),
# 			this must be handled at a higher level.
#
# 			If non-fatal error, unused.
# 	%rsi: Requested size in bits.
# 	%rdx:
# 		Offered ‘price’, where a higher price may be more likely to result in
# 		allocation if resources are scarce.
# 	%rcx:
# 		Please set to 0.  Future versions may want to add flags or other
# 		options for base allocation.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
# 	- %rax
#
# (However, currently %rsi does happen to be preserved, but this is for the
# current implementation.)
.global ns_system_x86_64_linux_base_malloc
.set ns_system_x86_64_linux_base_malloc, (_ns_system_x86_64_linux_base_malloc - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_base_malloc:
	# Verify the 4th argument is 0.
	testq %rcx, %rcx
	jz 1f
0:
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_4th(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_4th_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	# The next two lines are equivalent to the jump.  The intel manual says a
	# 16-byte relative is ‘N.S.’ in 64-bit mode, but 32-bit relative jumps are
	# ‘Valid’ in 64-bit mode.
	#.byte 0xE9
	#.long _ns_system_x86_64_linux_exit_custom - . - 4
	nop
	hlt
1:

	# Now backup the return in the storage unit used for the ‘int fd’
	# parameter, which is ignored.  This is at the cost that in some
	# implementations (probably not typical for our own platform), it is
	# required to be ‘-1’.  5th Parameter.
	movq %rdi, %r8

	# Convert %rsi from bit units to byte quantities.  Ensure it is divisible
	# by 8.
	testq $0x7, %rsi
	jz 1f
0:
	# Error: bits not divisible by 8!
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:
	shr $3, %rsi

	# So just ignore the price in %rdx for now.  Cool idea, though, I guess.
	movq %rdx, %rdx

	# Request an allocation of memory via ‘mmap()’.
	movq $0, %r9
	movq %r8, %r8
	movq $33, %r10  # MAP_ANONYMOUS (0x20) | MAP_SHARED (0x01) = 33; requires linux >= 2.4.
	movq $7, %rdx
	movq %rsi, %rsi
	movq $0, %rdi
	movq $9, %rax  # mmap
	syscall

	# Check for ENOMEM (12) with %rax of -12, in which case we skip verification.
	cmpq $-12, %rax
	jnz 1f
0:
	# We'll just return with an error message indicating we don't have enough
	# memory.

	# Set up continuation arguments.
	leaq ns_system_x86_64_linux_err_msg_base_malloc_enomem(%rip), %rsi
	movq $0, %rdx
	movq %rsi, %rsi
	movq $0, %rdi

	movq %r8, %rcx
	jmpq *%rcx
	nop
1:

	# Back up %rsi (number of bits requested and allocated) and %rax
	# (pointer).
	movq %rsi, %r9
	movq %rax, %rax

	# Verify, unless it was due to insufficient resources.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_error(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_error_size(%rip), %rcx
	movq (%rcx), %rcx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Return.
	# Set up continuation arguments.
	movq %rax, %rdx  # allocation
	movq %r9, %rsi  # bits allocated
	shl $3, %rsi
	movq $1, %rdi

	movq %r8, %rcx
	jmpq *%rcx
	nop

# At a low-level, free an allocation made by
# ‘ns_system_x86_64_linux_base_malloc’.
#
# You must keep track of the allocations, and its size, and must handle
# resource management and freeing on top of what uses this memory.  This API is
# meant to be a minimal wrapper around the platform, a base upon which a higher
# level allocation or memory management API can be written.
#
# Parameters:
# 	%rdi: Return.
# 	%rsi: Size of the allocation to free *in bits*.
# 	%rdx:
# 		Base address: pointer to the virtual memory address of the beginning of
# 		the allocation.
# 	%rcx:
# 		Offered ‘price’, where a lower price may be more likely to result in
# 		the allocation being freed if resources are scarce.
# 	%r8:
# 		Please set to 0.  Future versions may want to add flags or other
# 		options for base memory freeing.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers except %r9:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %rax
#
# (Although %rdi happens to be preserved.)
.global ns_system_x86_64_linux_base_mfree
.set ns_system_x86_64_linux_base_mfree, (_ns_system_x86_64_linux_base_mfree - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_base_mfree:
	# Verify the 5th argument is 0.
	testq %r8, %r8
	jz 1f
0:
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_base_mfree_5th(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_base_mfree_5th_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# Now backup the return in the storage unit used for %r8, since we
	# otherwise don't use it.
	movq %rdi, %r8

	# Convert %rsi from bit units to byte quantities.  Ensure it is divisible
	# by 8.
	testq $0x7, %rsi
	jz 1f
0:
	# Error: bits not divisible by 8!
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_base_mfree_byte_divisible_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:
	shr $3, %rsi

	# So just ignore the price in %rcx for now.  Cool idea, though, I guess.
	movq %rcx, %rcx

	# Free the memory via ‘munmap()’.
	movq %rsi, %rsi
	movq %rdx, %rdi
	movq $11, %rax  # munmap
	syscall

	# Verify.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_base_mfree_error(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_mfree_error_size(%rip), %rcx
	movq (%rcx), %rcx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Return.
	movq %r8, %rdi
	jmpq *%rdi
	nop

# Simple utility to wrap around base_malloc but require successful allocation,
# failing also if there is not enough memory available, treating ENOMEM as a
# fatal error, so that we crash with an error message when this happens.
#
# (TODO: allow callbacks and greater configurability for such errors, to let
# higher levels cleanup and such.)
#
# Takes almost the same arguments as base_malloc, except the %rdi is updated:
# 	%rdi: Return with args:
# 		%rdi:
# 			Number of bits allocated (currently, equal to requested size).
# 		%rsi: Pointer in virtual memory to the beginning of the allocation.
# 			I recommend having the size of the allocation as the first uint64,
# 			then 3 uint64 pointers to other allocations, then your own base
# 			allocation header to customize how you want to handle and track
# 			allocations at runtime, but how you use this allocation usp to you,
# 			but since we don't track the allocations and metadata ourselves
# 			(although we might be able to get information from the kernel),
# 			this must be handled at a higher level.
# 	%rsi: Requested size in bits.
# 	%rdx:
# 		Offered ‘price’, where a higher price may be more likely to result in
# 		allocation if resources are scarce.
# 	%rcx:
# 		Please set to 0.  Future versions may want to add flags or other
# 		options for base allocation.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
# 	- %rax
.global ns_system_x86_64_linux_base_malloc_require
.set ns_system_x86_64_linux_base_malloc_require, (_ns_system_x86_64_linux_base_malloc_require - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_base_malloc_require:
	# (Technically, internally, we would do a real wrapper, but since the mmap
	# syscall leaves us with limited working storage space from registers
	# alone, we'll just copy it and modify it under the hood, to avoid
	# needlessly adding a stack %rsp dependency.)

	# Verify the 4th argument is 0.
	testq %rcx, %rcx
	jz 1f
0:
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_4th(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_4th_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# Now backup the return in the storage unit used for the ‘int fd’
	# parameter, which is ignored.  This is at the cost that in some
	# implementations (probably not typical for our own platform), it is
	# required to be ‘-1’.  5th Parameter.
	movq %rdi, %r8

	# Convert %rsi from bit units to byte quantities.  Ensure it is divisible
	# by 8.
	testq $0x7, %rsi
	jz 1f
0:
	# Error: bits not divisible by 8!
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_byte_divisible_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:
	shr $3, %rsi

	# So just ignore the price in %rdx for now.  Cool idea, though, I guess.
	movq %rdx, %rdx

	# Request an allocation of memory via ‘mmap()’.
	movq $0, %r9
	movq %r8, %r8
	movq $33, %r10  # MAP_ANONYMOUS (0x20) | MAP_SHARED (0x01) = 33; requires linux >= 2.4.
	movq $7, %rdx
	movq %rsi, %rsi
	movq $0, %rdi
	movq $9, %rax  # mmap
	syscall

	# Back up %rsi (number of bits requested and allocated) and %rax
	# (pointer).
	movq %rsi, %r9
	movq %rax, %rax

	# Verify.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_base_malloc_error(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_base_malloc_error_size(%rip), %rcx
	movq (%rcx), %rcx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Return.
	# Set up continuation arguments.
	movq %rax, %rsi  # allocation
	movq %r9, %rdi  # bits allocated
	shl $3, %rdi

	movq %r8, %rdx
	jmpq *%rdx
	nop

# ################################################################
# Concurrency.
# ################################################################

# TODO: like ns_system_x86_64_linux_fork_require, except insufficient resources
# is a non-fatal error.
#
# This clobbers TODO.
#.global ns_system_x86_64_linux_fork
ns_system_x86_64_linux_fork:
	# TODO
	hlt

# Spawn another executor.
#
# The new executor (thread) can exit with exit_success.
#
# Note: as this spawns a new OS thread, it may be desireable to add a layer of
# abstraction for threads on top of this, e.g. by gradually increasing and
# decreasing the pool of OS threads as needed, where the higher-level
# abstraction has less bookkeeping costs than creating and destroying OS
# threads.
#
# Note: the joiner does use the thread's stack space in order to give the
# kernel a place to give us information about the child thread.
#
# Parameters:
# 	%rdi: Return with arguments:
# 		%rdi: joiner: continuation callback with the following parameters:
# 			%rdi: Return after joining, blocking and waiting until the thread finishes.  (And fail with an error instead if the joined thread failed with an error.)
# 			%rsi: User data, to make this callback a closure.
# 		%rsi: user data that must be provided as the second argument (%rsi) to the joiner.
# 	%rsi: Where the new executor starts executing.
# 	%rdx:
# 		Offered ‘price’, where a higher price may be more likely to result in
# 		allocation if resources are scarce.
#
# Without any error, clobbers %rcx (4th argument), %rax, and %r11 (from the syscall) and all argument registers except r9:
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %rax
#
# (Technically, the clobbering can be exploited in the current implementation
# such that %r9 can pass user data to the child thread.)
.global ns_system_x86_64_linux_fork_require
.set ns_system_x86_64_linux_fork_require, (_ns_system_x86_64_linux_fork_require - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_fork_require:
	# Note: I would use the clone syscall (56) to be more explicit but only use
	# the fork syscall (57) for convenience because it requires fewer storage
	# units.

	# So just ignore the price in %rdx for now.  Cool idea, though, I guess.
	movq %rdx, %rdx

	# Clear out the first 32 bits of the PID, which we store as the user data.
	movq $0, %rax

	# Backup input arguments.
	movq %rdi, %r8
	#movq %rsi, %r9  # Just use xchngq at beginning of the Verify step instead.  Saves us a register.

	# Fork.
	movq $57, %rax  # fork  #  (First 32 bits are 0.)
	syscall

	# Verify.
	xchgq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_fork(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_size(%rip), %rcx
	movq (%rcx), %rcx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (If this doesn't exit, it doesn't clobber %r8.)
9:
	nop
	xchgq %rax, %rsi

	# If we're the child, go to the child continuation.
	testq %rax, %rax
	jnz 0f
	jmpq *%rsi
	nop
0:

	# We're the parent.  Set up arguments to return and then return.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_fork_join(%rip), %rdi
	movq %r8, %rdx
	jmpq *%rdx
	nop

# Used by ‘ns_system_x86_64_linux_fork_join’; the action part of the closure.
#
# Parameters:
# 	%rdi: Return after joining, blocking and waiting until the thread finishes.  (And fail with an error instead if the joined thread failed with an error.)
# 	%rsi: User data.  (Internally, the PID of the thread to join to.)
#
# Without any error, this clobbers no registers; it uses the stack to preserve
# them.  (This is optional, as in the x64 linux calling convention, these are
# caller preserved, not callee-preserved.)
#
# This uses 336 bytes of stack space (equivalent to 42 ‘uint64_t’s).
ns_system_x86_64_linux_fork_join:
	# According to the man pages, the ‘waitid’ (247 for x86_64-linux; there's
	# an ‘x32’ 529 listin in syscall_64.tbl) is preferred for new applications
	# over the nonstandard ‘wait4’ (61) syscall, although the former takes 5
	# arguments and the latter takes 4.

	# Since we're already using stack space here as a convenience to interface
	# with the kernel so it has a place to write the information to, we'll also
	# just use the stack to backup the input parameters, rather than taking the
	# 4-parameter variant.
	#
	# Also backup %rax, %r11, and the first 4 standard parameter registers.
	subq $16, %rsp
	movq $0,   8(%rsp)  # Padding for a 16-byte-aligned stack.
	movq %rdi, 0(%rsp)

	subq $16, %rsp
	movq %rsi, 8(%rsp)
	movq %rax, 0(%rsp)

	subq $16, %rsp
	movq %r11, 8(%rsp)
	movq %rdx, 0(%rsp)

	subq $16, %rsp
	movq %rcx, 8(%rsp)

	# For an extra check, explained below, backup our
	# ns_system_x86_64_linux_fork_join instance.
	leaq ns_system_x86_64_linux_fork_join(%rip), %r11
	movq %r11, 0(%rsp)

	# siginfo_t looks to be uint32_t signo, uint32_t errno, uint32_t code,
	# padding, fields union.  (Apparently some architectures swap the order of
	# errno and code).  sizeof() on my system tells me it's 128 bytes long.
	# Just give _.
	#
	# However, in case the kernel thinks this struct is longer, we'll perform
	# an extra check to make sure our recovered return address (!) is the same
	# as an extra backup of the return address that we put below this stack.
	# Also for extra validation, get a copy of the thread's current execution
	# pointer and put it after and before to make sure it checks out.
	subq $256, %rsp
	# Track a pointer to this siginfo_t struct we just made.
	movq %rsp, %rdx

	# For an extra check, explained below, backup our
	# ns_system_x86_64_linux_fork_join instance.
	leaq ns_system_x86_64_linux_fork_join(%rip), %r11
	subq $16, %rsp
	movq %r11, 8(%rsp)

	# Back up an extra %rdi before, not just after.
	movq %rdi, 0(%rsp)

0:
	# ‘waitid’ syscall (linux >= 2.6.9).
	movq $0, %r8  # struct rusage *
	movq $4, %r10  # int options - WEXITED (4), but not WSTOPPED (2) or WCONTINUED (8).
	movq %rdx, %rdx  # siginfo_t * - we allocated space on our thread's stack for this struct.
	movq %rsi, %rsi  # id_t (probably int / u32) - the PID (just the user data)
	movq $1, %rdi  # idtype_t (probably enum / int): P_PID = 1, so ‘id’ is the PID.
	movq $247, %rax  # waitid
	syscall

	# Verify.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_fork_join_waitid(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_waitid_size(%rip), %rcx
	movq (%rcx), %rcx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Double check our 2 values, to make sure they still match.
	# Check original %rdi.
	movq 0(%rsp), %rdi
	movq 320(%rsp), %rsi
	cmpq %rsi, %rdi
	jne 2f

	# Check the %rip-based location, for fork_join.
	movq 8(%rsp), %rdi
	movq 272(%rsp), %rsi
	cmpq %rsi, %rdi
	jne 2f

	jmp 3f
2:
	# Error: We don't recognize the code.
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_fork_join_stack_mismatch_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
3:

	# Sanity check: just make sure signo is SIGCHLD (17).
	movq $0, %rdi
	movl 16(%rsp), %edi  # signo
	cmpq $17, %rdi
	jz 1f
	# We don't recognize the signo.

	# First, write to this instance's error message storage to print the
	# unknown code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_print_u64
9:
	nop

	# Error: We don't recognize the code.
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_fork_join_unrecognized_signo_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# See if the process terminated yet, rather than us receiving a process
	# status change for some other reason, in which case repeat the ‘waitid’
	# syscall.
	#
	# uint32_t signo (always CLD_EXITED), uint32_t status/errno, uint32_t code
	# (we're only interested in CLD_EXITED=1, CLD_KILLED=2, and CLD_DUMPED=3;
	# not CLD_STOPPED=5, CLD_TRAPPED=4, or CLD_CONTINUED=6), padding, union.
	movq $0, %rdi
	movl 24(%rsp), %edi  # code
	cmpq $5, %rdi
	jz 0b
	cmpq $4, %rdi
	jz 0b
	cmpq $6, %rdi
	jz 0b

	cmpq $1, %rdi
	jz 1f
	cmpq $2, %rdi
	jz 1f
	cmpq $3, %rdi
	jz 1f
2:
	# We don't recognize the code.

	# First, write to this instance's error message storage to print the
	# unknown code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_print_u64
9:
	nop

	# Error: We don't recognize the code.
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_fork_join_unknown_code_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
1:

	# The process terminated.
	# But now see if the process exited succesfully.  If it didn't, abort /
	# exit with error.
	movq $0, %rdi
	movl 20(%rsp), %edi  # status/errno
	testq %rdi, %rdi
	jz 0f
1:
	# A thread failed.

	# First, write to this instance's error message storage to print the
	# exit code, so that the last code gets written.
	movq %rdi, %rcx
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_size(%rip), %rdx
	movq (%rdx), %rdx
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure_u_offset(%rip), %rsi
	movq (%rsi), %rsi
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_print_u64
9:
	nop

	# Error: a thread failed.
	movq $2, %rdi
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure(%rip), %rsi
	leaq ns_system_x86_64_linux_err_msg_fork_join_exited_failure_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt
0:

	# The thread didn't fail.

	# Restore the stack and registers.
	movq 0(%rsp), %rdi
	# 8(%rsp): Extra fork_join reference.
	addq $16, %rsp

	addq $256, %rsp  # struct siginfo_t

	# 0(%rsp): Extra fork_join reference.
	movq 8(%rsp), %rcx
	addq $16, %rsp

	movq 0(%rsp), %rdx
	movq 8(%rsp), %r11
	addq $16, %rsp

	movq 0(%rsp), %rax
	movq 8(%rsp), %rsi
	addq $16, %rsp

	# 0(%rsp): Another %rdi.
	# 8(%rsp): Padding.
	addq $16, %rsp  # Another %rdi.

	# Return.
	jmpq *%rdi
	nop

# TODO

# ################################################################
# Reading and writing.
# ################################################################

# TODO

# Write to a file.  The FRP time context is an implicit parameter.
# 	%rdi: AHC runtime environment pointer.
# 	%rsi: return
# 	%rdx: pointer to filepath
# 	%rcx: size of filepath
# 	%r8:  pointer to data to write
# 	%r9:  size of data to write
#
# 	Callback called with:
# 	%rdi: Type of write result (success or failure; 0: failure; 1: success)
# 	%TODO: bytes blah
# 	TODO: futures?
# 	TODO
.global ns_system_x86_64_linux_write
.set ns_system_x86_64_linux_write, (_ns_system_x86_64_linux_write - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_write:
	# TODO
	nop
	jmp _ns_system_x86_64_linux_exit_failure
	nop

# ################################################################
# Shell calls.
# ################################################################

# TODO

# ################################################################
# Exiting and process maangement.
# ################################################################

.global ns_system_x86_64_linux_exit_success
.set ns_system_x86_64_linux_exit_success, (_ns_system_x86_64_linux_exit_success - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_exit_success:
	movq $60, %rax  # exit
	movq $0, %rdi
	syscall

	jmp _ns_system_x86_64_linux_exit_success
	nop

.global ns_system_x86_64_linux_exit_failure
.set ns_system_x86_64_linux_exit_failure, (_ns_system_x86_64_linux_exit_failure - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_exit_failure:
	movq $60, %rax  # exit
	movq $1, %rdi
	syscall

	jmp _ns_system_x86_64_linux_exit_failure
	nop

# Exit with a code and error message.
# 	%rdi: Error code.
# 	%rsi: Error message.
# 	%rdx: Error message size.
#
# (Note: this brings into play Linux's blocking mechanisms.)
#
# (TODO: allow callbacks and greater configurability for such errors, to let
# higher levels cleanup and such.)
.global ns_system_x86_64_linux_exit_custom
.set ns_system_x86_64_linux_exit_custom, (_ns_system_x86_64_linux_exit_custom - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_exit_custom:
	movq %rdi, %r10

	movq %rdx, %rdx
	movq %rsi, %rsi
	movq $2, %rdi
	movq $1, %rax  # write
	syscall

	movq %r10, %rdi

	movq $60, %rax  # exit
	movq %rdi, %rdi
	syscall

	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt

# ################################################################
# Misc.
# ################################################################

# Return to the last function on the call stack.
#
# OLD variant:
#
# This clobbers %rdi.
#
# If you have the shadow stack enabled on your system, you will need to use
# ‘net; nop’ variant instead, which doesn't clobber %rdi.
.global ns_system_x86_64_linux_cc_call_stack_return
.set ns_system_x86_64_linux_cc_call_stack_return, (_ns_system_x86_64_linux_cc_call_stack_return - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_cc_call_stack_return:
	ret
	nop
	#movq (%rsp), %rdi
	#addq $8, %rsp
	#jmpq *%rdi
	#nop

# clock_nanosleep the given number of nanoseconds against the CLOCK_MONOTONIC=1
# clock.
#
# Parameters:
# 	%rdi: Return.
# 	%rsi: i64 seconds.
# 	%rdx: i64 nanoseconds.
#
# Note: this depends on a working stack and uses stack space to give the kernel
# a place to store timespec output we need.
#
# For convenience, since the stack is used, it is also used to preserve all
# registers.  (Not required, since conventionally the registers we use here are
# all caller-preserved.)
#
# This clobbers nothing.
.global ns_system_x86_64_linux_monotonic_nanosleep
.set ns_system_x86_64_linux_monotonic_nanosleep, (_ns_system_x86_64_linux_monotonic_nanosleep - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_monotonic_nanosleep:
	# timespec is a pair of signed longs: seconds and nanoseconds.

	# Backup %rax.
	subq $16, %rsp
	# 8(%rsp): padding.
	movq %rax, 0(%rsp)

	# Backup %rcx and %r11.
	subq $16, %rsp
	movq %rcx, 8(%rsp)
	movq %r11, 0(%rsp)

	# Backup %rdi and %rsi.
	subq $16, %rsp
	movq %rdi, 8(%rsp)
	movq %rsi, 0(%rsp)

	# Backup %rdx and %r10.
	subq $16, %rsp
	movq %rdx, 8(%rsp)
	movq %r10, 0(%rsp)

	# Backup %r8.
	subq $16, %rsp
	# 8(%rsp): padding.
	movq %r8, 0(%rsp)

	# Get non-register working storage units for the request timespec struct
	# too.
	subq $16, %rsp
	movq %rdx, 8(%rsp)  # i64 nanoseconds.
	movq %rsi, 0(%rsp)  # i64 seconds.
	movq %rsp, %r8      # Request.

	# Clobber buffer cushion.
	subq $16, %rsp

	# Reserve space for the syscall's time remaining output.
	subq $16, %rsp
	movq %rsp, %r10  # Track struct to hold remaining sleep.

1:
	# Perform the syscall.
	movq %r10, %r10  # struct timespec *remain
	movq %r8, %rdx  # const struct timespec *request
	movq $0, %rsi  # int flags = 0
	movq $1, %rdi  # clockid_t clockid = CLOCK_MONOTONIC
	movq $230, %rax  # clock_nanosleep
	syscall

	# Verify.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_clock_nanosleep(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_clock_nanosleep_size(%rip), %rcx
	movq (%rcx), %rcx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (Clobbers nothing on success.)
9:
	nop

	# The syscall returns 0 when it's successfully completed the duration of
	# sleep.  Otherwise it was interrupted and we'll need to resume the sleep.
	#
	# So check %rax.
	testq %rax, %rax
	jz 0f

	# Okay, copy ‘remain’ into ‘request’ and resume the sleep.
	movq 0(%r10), %rdi
	movq 8(%r10), %rsi
	movq %rdi, 0(%r8)
	movq %rsi, 8(%r8)

	jmp 1b
0:

	# Restore stack and storage units.

	# Clear time remaining space.
	addq $16, %rsp

	# Pop the clobber buffer.
	addq $16, %rsp

	# Pop request.
	addq $16, %rsp

	# Restore %r8.
	# 8(%rsp): padding.
	movq 0(%rsp), %r8
	addq $16, %rsp

	# Restore %r10 and %rdx.
	movq 0(%rsp), %r10
	movq 8(%rsp), %rdx
	addq $16, %rsp

	# Restore %rsi and %rdi.
	movq 0(%rsp), %rsi
	movq 8(%rsp), %rdi
	addq $16, %rsp

	# Restore %r11 and %rcx.
	movq 0(%rsp), %r11
	movq 8(%rsp), %rcx
	addq $16, %rsp

	# Restore %rax.
	# 8(%rsp): padding.
	movq 0(%rsp), %rax
	addq $16, %rsp

	# Return.
	jmpq *%rdi
	nop

# Utility to write a u64 in ascii.
#
# Right padded.
#
# Parameters:
# 	%rdi: Return with arguments:
# 		%rdi: Number of written digits printed to encode the number, excluding writen padding and leftover digits.
# 		%rsi: Number of digits or characters (bytes) written or skipped, which here should be equal to the input size.
# 		%rdx: Number of digits leftover for which there was not enough space to print
# 	%rsi: Pointer to memory to write ascii digits to; start address.
# 	%rdx: Size of the memory region for writing the string.
# 	%rcx: The u64 to print.
#
# This clobbers %r11, %rax, %r10, and all argument registers:
# 	- %r11
# 	- %rax
# 	- %r10
# 	- %rdi
# 	- %rsi
# 	- %rdx
# 	- %rcx
# 	- %r8
# 	- %r9
.global ns_system_x86_64_linux_print_u64
.set ns_system_x86_64_linux_print_u64, (_ns_system_x86_64_linux_print_u64 - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_print_u64:
	movq %rdx, %r11  # Size.

	#movq %rsi, %rsi  # Base.
	movq %rdx, %r10  # Current size.
	movq %rcx, %r8   # Current dividend.

5:
	# Break if out of space.
	testq %r10, %r10
	jz 4f
	dec %r10

	# Divide by 10 to get the least significant digit.
	movq $0, %rdx
	movq %r8, %rax
	movq $10, %r9
	divq %r9
	# mod (remainder) is %rdx, quotient is %rax.
	# Use %rdx to write another digit, starting from the right.
	addq $'0, %rdx
	addq %r10, %rsi
	movb %dl, (%rsi)
	subq %r10, %rsi
	# The next dividend will be %rax.
	movq %rax, %r8

	# Loop back unless %r8 is 0.
	testq %r8, %r8
	jnz 5b
4:

	# Discard the original dividend and use this storage unit for the return
	# continuation.
	movq %rdi, %rcx

	# %r11 - %r10, that is, size - current size, tells us the number of printed
	# digits.
	movq %r10, %rdi
	subq %r11, %rdi

	# %r11 is the number of digits skipped or written (original size).
	movq %r11, %rsi

	# Now just do a shorter loop to see how many iterations we need before we
	# get to 0.  Find %rdx; we'll count with %r10 before putting 5r10 into %rdx.
	movq $0, %r10
5:
	# Already finished dividing the number to 0?
	testq %r8, %r8
	jz 4f

	# Nope, it's non-zero.  Increment number of divisions (digits), divide, and
	# try again.
	inc %r10

	# Divide by 10 to get the least significant digit.
	movq $0, %rdx
	movq %r8, %rax
	movq $10, %r9
	divq %r9
	# mod (remainder) is %rdx, quotient is %rax.
	# Use %rdx to write another digit, starting from the right.
	#addq $'0, %rdx
	# The next dividend will be %rax.
	movq %rax, %r8

	jmp 5b
4:
	movq %r10, %rdx

	# Return.
	movq %rcx, %rcx
	jmpq *%rcx
	nop

# Simple utility to verify a syscall was successful or else fail with an error
# message.
# 	%rdi: return
# 	%rsi: the errno to check (syscall return %rax value, actually)
# 	%rdx: an error message to print after our prefix of ‘Error (errno %d): ’
# 	%rcx: size of the error message
#
# If this procedure action returns with no error detected, no storage units are
# clobbered.
.global ns_system_x86_64_linux_verify_errno
.set ns_system_x86_64_linux_verify_errno, (_ns_system_x86_64_linux_verify_errno - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_verify_errno:
	# Return normally if the syscall return value is not in [-4095, -1] (https://stackoverflow.com/a/2538212).
	cmpq $-4095, %rsi  # if %rsi < -4095
	jl   0f            # return

	cmpq $-1, %rsi  # if %rsi > -1
	jg   0f         # return

	# We have an error.  Prepare the message.
	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rdi
# 	%rdx: an error message to print after our prefix of ‘Error (errno %d): ’
	movb $'E, 0(%rdi)
	movb $'r, 1(%rdi)
	movb $'r, 2(%rdi)
	movb $'o, 3(%rdi)
	movb $'r, 4(%rdi)
	movb $' , 5(%rdi)
	movb $'(, 6(%rdi)
	movb $'e, 7(%rdi)
	movb $'r, 8(%rdi)
	movb $'r, 9(%rdi)
	movb $'n, 10(%rdi)
	movb $'o, 11(%rdi)
	movb $' , 12(%rdi)
	movb $' , 13(%rdi)
	movb $' , 14(%rdi)
	movb $' , 15(%rdi)
	movb $' , 16(%rdi)
	movb $'), 17(%rdi)
	movb $':, 18(%rdi)
	movb $' , 19(%rdi)

	movq %rdx, %r8

	# If %rcx > ns_system_x86_64_linux_syscall_verify_builder_size + 20, %rcx = … + 20.
	leaq ns_system_x86_64_linux_syscall_verify_builder_size(%rip), %rdx
	movq (%rdx), %rdx
	addq $20, %rdx
	cmpq %rdx, %rcx
	jna 8f
	movq %rdx, %rcx  # %rcx <- …_size + 20
8:

	# Append the error message.
	addq $20, %rdi
7:
	# Break if size reaches 0.# or we reach a null byte.
	testq %rcx, %rcx
	jz 6f
	#movq (%rdi), %rdx
	#testq %rdx, %rdx
	#jz 6f

	movb (%r8), %r9b
	movb %r9b, (%rdi)
	dec %rcx
	inc %rdi
	inc %r8

	jmp 7b
6:

	# Encode the error code (%rsi) into offsets 13 through 17 of %rdi.
	not %rsi
	inc %rsi  # %rsi = -%rsi

	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rdi
	addq $13, %rdi  # Base.
	movq $4, %rcx  # Size.
	movq %rsi, %r8  # Current dividend.
5:
	# Break if out of space.
	testq %rcx, %rcx
	jz 4f
	dec %rcx

	# Divide by 10 to get the least significant digit.
	movq $0, %rdx
	movq %r8, %rax
	movq $10, %r9
	divq %r9
	# mod (remainder) is %rdx, quotient is %rax.
	# Use %rdx to write another digit, starting from the right.
	addq $'0, %rdx
	addq %rcx, %rdi
	movb %dl, (%rdi)
	subq %rcx, %rdi
	# The next dividend will be %rax.
	movq %rax, %r8

	# Loop back unless %r8 is 0.
	testq %r8, %r8
	jnz 5b
4:

	# Hand over control to exit_custom().
	movq $3, %rdi
	leaq ns_system_x86_64_linux_syscall_verify_builder(%rip), %rsi
	leaq ns_system_x86_64_linux_syscall_verify_builder_size(%rip), %rdx
	movq (%rdx), %rdx
	jmp _ns_system_x86_64_linux_exit_custom
	nop
	hlt

0:
	# Return normally.
	jmpq *%rdi
	nop

# An optional segv trap handler that hands off execution to what was statically
# set in ‘ns_system_x86_64_linux_sigaction_static_set_segv_cont’ and
# ‘ns_system_x86_64_linux_sigaction_static_set_segv_data’
ns_system_x86_64_linux_trap_middleman:
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_data(%rip), %rdi
	movq (%rdi), %rdi
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_cont(%rip), %rsi
	movq (%rsi), %rsi
	jmpq *%rsi
	nop

	# Redundantly after a jump (never executed), adapt to the signal handler's
	# expectation of a conventional return.
	movq (%rsp), %rdi
	addq $8, %rsp
	jmpq *%rdi
	nop

# Call a callback on segfault.  Only use this once per process instantiation.
#
# Note this is called before self-modifying-code support is verified.
# 	%rdi: return
# 	%rsi: callback continuation, where to jump.
# 	%rdx: callback pointer / user data.
.global ns_system_x86_64_linux_trap_segv
.set ns_system_x86_64_linux_trap_segv, (_ns_system_x86_64_linux_trap_segv - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_trap_segv:
	# Backup the continuation.
	movq %rdi, %r8

	# Set the first 8 bytes in our static data.
	#leaq ns_system_x86_64_linux_sigaction_static_set_segv(%rip), %rdi
	#ns_system_x86_64_linux_sigaction_static_set_segv
	# Use a wrapper layer to track user data.
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_cont(%rip), %rdi
	movq %rsi, (%rdi)
	leaq ns_system_x86_64_linux_sigaction_static_set_segv_data(%rip), %rdi
	movq %rdx, (%rdi)
	# Set the first 8 bytes in our static data.
	leaq ns_system_x86_64_linux_trap_middleman(%rip), %rdi
	leaq ns_system_x86_64_linux_sigaction_static_set_segv(%rip), %rsi
	movq %rdi, (%rsi)
	#movq $ns_system_x86_64_linux_trap_middleman, (%rsi)

	# rt_sigaction(…)
	movq $11, %rdi  # signum for SEGV is 11.  (INT is 2, and USR1 is 10.)
	leaq ns_system_x86_64_linux_sigaction_static_set_segv(%rip), %rsi
	movq $0, %rdx
	movq $8, %r10  # sizeof sigset_t: 128, but we'll just use 0.  Weird, disas glibc .so has this set to 8, and that made it progress further.  Whatever, so it's 8, not 0 or 128.
	movq $13, %rax  # rt_sigaction
	syscall

	# Verify.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_segv_trap_set(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_segv_trap_set_size(%rip), %rcx
	movq (%rcx), %rcx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno  # (If this doesn't exit, it doesn't clobber %r8.)
9:
	nop

	# TODO: FIXME: have:
	# 	5:
	# 		jmp 5b
	# here (optionally also at the beginning of trap_middleman()), adjust
	# SIGSEGV1 to USR1, then start ahc, then ‘pkill -USR1 ahc’, and then ahc
	# will segfault.  It seems correct to me: glibc seems to do it as I do and
	# set it up here (objdump -d libc.so), and a C ‘rt_sigaction’ experiment
	# with a null-initialized buffer except the first 8 bytes point to the
	# handler worked on my sistem.  I don't know why this thing is segfaulting.
	# Oh well, we can worry about this later.  Until this gets fixed, the user
	# will see a segfault instead of an error message if code sections are
	# unwritable.

	# Return.
	movq %r8, %rdi
	jmpq *%rdi
	nop

# Restore default segfault trap behaviour, but override any previous
# non-default setting with the default handler.
# 	%rdi: return
.global ns_system_x86_64_linux_restore_trap_segv
.set ns_system_x86_64_linux_restore_trap_segv, (_ns_system_x86_64_linux_restore_trap_segv - ns_system_x86_64_linux_module_begin)
_ns_system_x86_64_linux_restore_trap_segv:
	# Backup the continuation.
	movq %rdi, %r8

	# rt_sigaction(…)
	movq $11, %rdi  # signum for SEGV is 11.
	leaq ns_system_x86_64_linux_sigaction_restore_segv(%rip), %rsi
	movq $0, %rdx
	movq $8, %r10  # sizeof sigset_t: 128, but we'll just use 0.  Weird, disas glibc .so has this set to 8, and that made it progress further.  Whatever, so it's 8, not 0 or 128.
	movq $13, %rax  # rt_sigaction
	syscall

	# Verify.
	movq %rax, %rsi
	leaq ns_system_x86_64_linux_err_msg_segv_trap_restore(%rip), %rdx
	leaq ns_system_x86_64_linux_err_msg_segv_trap_restore_size(%rip), %rcx
	movq (%rcx), %rcx
	leaq 9f(%rip), %rdi
	jmp _ns_system_x86_64_linux_verify_errno
9:
	nop

	# Return.
	movq %r8, %rdi
	jmpq *%rdi
	nop

.global ns_system_x86_64_linux_module_end
ns_system_x86_64_linux_module_end:
