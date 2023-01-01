# vim: ft=make noet

# An alternative way to build ‘ahc-core’'s programs.
#
# To run only a minimal build, e.g. one that requires only a Haskell2010 stage
# 0 implementation, run ‘make BUILD_MINIMAL=1’.
#
# By default, this Makefile bypasses ‘cabal’ and invokes only a stage 0
# Haskell2010 implementation directly (this can be ‘ghc’ or an ‘ahc’ compiler
# cross-compiled on another platform), but this Makefile can also build with
# ‘cabal-install’.

# ################################################################
# Makefile configuration.
# ################################################################

# The default target.
.PHONY: default
default: all

# make all
.PHONY: all
all: build

# make help
.PHONY: help
help:
	@printf '%s\n' "Makefile for ‘ahc-core’."
	@printf '%s\n' ""
	@printf '%s\n' "Take a look at the Makefile source for more information."
	@printf '%s\n' ""
	@printf '%s\n' "Useful targets:"
	@printf '%s\n' "	default           : Default target."
	@printf '%s\n' "	all               : make all"
	@printf '%s\n' "	help              : Show this help text."
	@printf '%s\n' "	clean             : Clean."
	@printf '%s\n' "	build             : Run a full build."
	@printf '%s\n' "	build-prevalidate : Prevalidation (just top-level prevalidation)."
	@printf '%s\n' ""
	@printf '%s\n' "Useful configuration:"
	@printf '%s\n' "	BUILD_DIR=…            :"
	@printf '%s\n' "		specify an alternative build directory"
	@printf '%s\n' "	BUILD_MINIMAL=1        :"
	@printf '%s\n' "		run a minimal build"
	@printf '%s\n' "	BUILD_DEBUG=1          :"
	@printf '%s\n' "		prefer debugging settings where available"
	@printf '%s\n' "	BUILD_SYS_TOOLS=1      :"
	@printf '%s\n' "		prefer building with system tools like ‘cabal’"
	@printf '%s\n' "	BUILD_STAGE2=1         :"
	@printf '%s\n' "		build compilers twice, the second time by themselves."
	@printf '%s\n' "	DEBUG_SHOW_PREVALIDATOR_SCRIPTS=1 :"
	@printf '%s\n' "		print rather than run, where possible, prevalidator scripts"
	@printf '%s\n' "	HC_STAGE0=ghc          :"
	@printf '%s\n' "		specify an alternative bootstrapping compiler if not BUILD_SYS_TOOLS."
	@printf '%s\n' "	HC_STAGE0_FLAGS=…      :"
	@printf '%s\n' "		specify alternative bootstrapping compiler flags if not"
	@printf '%s\n' "		BUILD_SYS_TOOLS."
	@printf '%s\n' "	HC_STAGE0_LINK=ghc     :"
	@printf '%s\n' "		specify an alternative bootstrapping compiler for the final program"
	@printf '%s\n' "		build if not BUILD_SYS_TOOLS."
	@printf '%s\n' "	HC_STAGE0_LINK_FLAGS=… :"
	@printf '%s\n' "		specify alternative bootstrapping compiler flags for the final program"
	@printf '%s\n' "		build if not BUILD_SYS_TOOLS."
	@printf '%s\n' ""
	@printf '%s\n' "End of help text."

# Clean.
.PHONY: clean
clean:
	@# (We'll hard-code ‘./_build’ here for safety.)
	@# (Skip this removal if BUILD_DIR is non-default.)
	test "x$(BUILD_DIR)" != "x./_build" || rm -rf -- "./_build"

# Basic configuration.
BUILD_DIR       ?= $(DEFAULT_BUILD_DIR)
BUILD_MINIMAL   ?= $(DEFAULT_BUILD_MINIMAL)
BUILD_DEBUG     ?= $(DEFAULT_BUILD_DEBUG)
BUILD_SYS_TOOLS ?= $(DEFAULT_BUILD_SYS_TOOLS)
BUILD_STAGE2    ?= $(DEFAULT_BUILD_STAGE2)

DEFAULT_BUILD_DIR       = ./_build
DEFAULT_BUILD_MINIMAL   = 0
DEFAULT_BUILD_DEBUG     = 0
DEFAULT_BUILD_SYS_TOOLS = 0
DEFAULT_BUILD_STAGE2    = 1

# Run a full build.
.PHONY: build
build: build-prevalidate build-programs build-dist

# Alias for ‘build-dist’.
.PHONY: dist
dist: build-dist

# Ensure the build directories are set up.
.PHONY: build-dirs build-dirs-dist
build-dirs: | $(BUILD_DIR)
build-dirs-dist: | build-dirs $(BUILD_DIR)/dist $(BUILD_DIR)/dist/bin $(BUILD_DIR)/dist/share

# Ensure the build directory exists.
$(BUILD_DIR):
	install -d -m 0775 -- "$@"
$(BUILD_DIR)/dist: | $(BUILD_DIR)
	install -d -m 0775 -- "$@"
$(BUILD_DIR)/dist/bin: | $(BUILD_DIR)/dist
	install -d -m 0775 -- "$@"
$(BUILD_DIR)/dist/share: | $(BUILD_DIR)/dist
	install -d -m 0775 -- "$@"

# After regular builds, organize everything together into a more neatly
# distributable hierarchy like ‘bin/’.
.PHONY: build-dist
build-dist: build-dist_$(BUILD_MINIMAL) $(BUILD_DIR)/dist/bin/ahc-minimal | build-dirs build-dirs-dist build-prevalidate

# Additional ‘build-dist’ targets if this is not a minimal build.
# (Currently, this is empty, but probably more will be added later on.)
.PHONY: build-dist_0 build-dist_
build-dist_0 build-dist_:

# Additional ‘build-dist’ targets if this is a minimal build (none).
.PHONY: build-dist_1
build-dist_1:

# Install ‘ahc-minimal’ into ‘dist/’.  Part of ‘make dist’.
$(BUILD_DIR)/dist/bin/ahc-minimal: $(BUILD_DIR)/ahc-minimal | build-dirs-dist
	install -m 0775 -- "$<" "$@"

# Alias for ‘build-programs’.
.PHONY: build-executables
build-executables: build-programs

# Build the ‘ahc-core’ programs.
.PHONY: build-programs
build-programs: build-programs_$(BUILD_MINIMAL) build-ahc-minimal | build-prevalidate

# Additional ‘build-programs’ targets if this is not a minimal build.
.PHONY: build-programs_0 build_programs_
build-programs_0 build-programs_:

# Additional ‘build-programs’ targets if this is a minimal build (none).
.PHONY: build-programs_1
build-programs_1:

# Build ‘ahc-core‛s ‘ahc-minimal’.
.PHONY: build-ahc-minimal
build-ahc-minimal: $(BUILD_DIR)/ahc-minimal | build-dirs

# Prevalidation.
.PHONY: build-prevalidate
build-prevalidate: build-prevalidate-import-consistency

# Configuration for ‘build-prevalidate’
DEBUG_SHOW_PREVALIDATOR_SCRIPTS ?= 0

# Configuration for ‘build-prevalidate-version-consistency’.
BASH ?= bash

# Use ‘bash -c’ or ‘echo -nE’?
_CMD_BUILD_PREV_SHOW   = $(_CMD_BUILD_PREV_SHOW_$(DEBUG_SHOW_PREVALIDATOR_SCRIPTS))
_CMD_BUILD_PREV_SHOW_0 = bash -c
_CMD_BUILD_PREV_SHOW_  = $(_CMD_BUILD_PREV_SHOW_0)
_CMD_BUILD_PREV_SHOW_1 = echo -nE

_PREVALIDATE_IMPORT_CONSISTENCY_DEPS = \
	$(_DIRECT_BUILD_SRCS) \
 \
	$(EMPTY)

# ################################################################
# Main build section.
# ################################################################

# Convenience.
EMPTY =

# Source configuration.
SRC_DIR ?= $(DEFAULT_SRC_DIR)

DEFAULT_SRC_DIR = ./src

# Build configuration
HC_STAGE0            ?= $(DEFAULT_HC_STAGE0)
HC_STAGE0_FLAGS      ?= $(DEFAULT_HC_STAGE0_FLAGS)
HC_STAGE0_LINK       ?= $(DEFAULT_HC_STAGE0_LINK)
HC_STAGE0_LINK_FLAGS ?= $(DEFAULT_HC_STAGE0_LINK_FLAGS)

HC_STAGE0_FLAGS_BASE       ?= $(DEFAULT_HC_STAGE0_FLAGS_BASE)
HC_STAGE0_FLAGS_EXTRA      ?= $(DEFAULT_HC_STAGE0_FLAGS_EXTRA)
HC_STAGE0_LINK_FLAGS_BASE  ?= $(DEFAULT_HC_STAGE0_LINK_FLAGS_BASE)
HC_STAGE0_LINK_FLAGS_EXTRA ?= $(DEFAULT_HC_STAGE0_LINK_FLAGS_EXTRA)

DEFAULT_HC_STAGE0            = $(CONDITIONAL_HC_STAGE0)
DEFAULT_HC_STAGE0_FLAGS      = $(HC_STAGE0_FLAGS_BASE) $(HC_STAGE0_FLAGS_EXTRA)
DEFAULT_HC_STAGE0_LINK       = $(CONDITIONAL_HC_STAGE0_LINK)
DEFAULT_HC_STAGE0_LINK_FLAGS = $(HC_STAGE0_LINK_FLAGS_BASE) $(HC_STAGE0_LINK_FLAGS_EXTRA)

DEFAULT_HC_STAGE0_FLAGS_BASE       = $(CONDITIONAL_HC_STAGE0_FLAGS_BASE)
DEFAULT_HC_STAGE0_FLAGS_EXTRA      =
DEFAULT_HC_STAGE0_LINK_FLAGS_BASE  = $(CONDITIONAL_HC_STAGE0_LINK_FLAGS_BASE)
DEFAULT_HC_STAGE0_LINK_FLAGS_EXTRA =

CONDITIONAL_HC_STAGE0                 = $(CONDITIONAL_HC_STAGE0_$(BUILD_MINIMAL))
CONDITIONAL_HC_STAGE0_FLAGS_BASE      = $(CONDITIONAL_HC_STAGE0_FLAGS_BASE_$(BUILD_MINIMAL))
CONDITIONAL_HC_STAGE0_LINK            = $(CONDITIONAL_HC_STAGE0_LINK_$(BUILD_MINIMAL))
CONDITIONAL_HC_STAGE0_LINK_FLAGS_BASE = $(CONDITIONAL_HC_STAGE0_LINK_FLAGS_BASE_$(BUILD_MINIMAL))

CONDITIONAL_HC_STAGE0_                 = $(CONDITIONAL_HC_STAGE0_0)
CONDITIONAL_HC_STAGE0_FLAGS_BASE_      = $(CONDITIONAL_HC_STAGE0_FLAGS_BASE_0)
CONDITIONAL_HC_STAGE0_LINK_            = $(CONDITIONAL_HC_STAGE0_LINK_0)
CONDITIONAL_HC_STAGE0_LINK_FLAGS_BASE_ = $(CONDITIONAL_HC_STAGE0_LINK_FLAGS_BASE_0)

CONDITIONAL_HC_STAGE0_0                 = $(REGULAR_HC_STAGE0)
CONDITIONAL_HC_STAGE0_FLAGS_BASE_0      = $(REGULAR_HC_STAGE0_FLAGS_BASE)
CONDITIONAL_HC_STAGE0_LINK_0            = $(REGULAR_HC_STAGE0_LINK)
CONDITIONAL_HC_STAGE0_LINK_FLAGS_BASE_0 = $(REGULAR_HC_STAGE0_LINK_FLAGS_BASE)

CONDITIONAL_HC_STAGE0_1                 = $(MINIMAL_HC_STAGE0)
CONDITIONAL_HC_STAGE0_FLAGS_BASE_1      = $(MINIMAL_HC_STAGE0_FLAGS_BASE)
CONDITIONAL_HC_STAGE0_LINK_1            = $(MINIMAL_HC_STAGE0_LINK)
CONDITIONAL_HC_STAGE0_LINK_FLAGS_BASE_1 = $(MINIMAL_HC_STAGE0_LINK_FLAGS_BASE)

MINIMAL_HC_STAGE0                 = ghc
MINIMAL_HC_STAGE0_FLAGS_BASE      = -package base -i$(SRC_DIR) -outputdir $(HC_STAGE0_WORKDIR) -Wno-tabs -c
MINIMAL_HC_STAGE0_LINK            = ghc
MINIMAL_HC_STAGE0_LINK_FLAGS_BASE = -package base -i$(SRC_DIR) -outputdir $(HC_STAGE0_WORKDIR) -Wno-tabs

REGULAR_HC_STAGE0                 = ghc
REGULAR_HC_STAGE0_FLAGS_BASE      = -package base -i$(SRC_DIR) -outputdir $(HC_STAGE0_WORKDIR) -Wno-tabs $(REGULAR_HC_STAGE0_FLAGS_BASE_QA) -c $(REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF)
REGULAR_HC_STAGE0_LINK            = ghc
REGULAR_HC_STAGE0_LINK_FLAGS_BASE = -package base -i$(SRC_DIR) -outputdir $(HC_STAGE0_WORKDIR) -Wno-tabs $(REGULAR_HC_STAGE0_FLAGS_BASE_QA) $(REGULAR_HC_STAGE0_LINK_FLAGS_BASE_DEBUGPROF)

REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF ?= $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF)
DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF = $(REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_$(BUILD_DEBUG))

REGULAR_HC_STAGE0_LINK_FLAGS_BASE_DEBUGPROF ?= $(DEFAULT_REGULAR_HC_STAGE0_LINK_FLAGS_BASE_DEBUGPROF)
DEFAULT_REGULAR_HC_STAGE0_LINK_FLAGS_BASE_DEBUGPROF = $(REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF)

REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_0 ?= $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_0)
REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_  ?= $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_)
REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_1 ?= $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_1)

DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_  = $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_0)

DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_0 =
DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_DEBUGPROF_1 = -debug -g -O0

# (‘WARN’: alias for ‘QA’.)
REGULAR_HC_STAGE0_FLAGS_BASE_WARN ?= $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_WARN)
DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_WARN = $(REGULAR_HC_STAGE0_FLAGS_BASE_QA)

REGULAR_HC_STAGE0_LINK_FLAGS_BASE_WARN ?= $(DEFAULT_REGULAR_HC_STAGE0_LINK_FLAGS_BASE_WARN)
DEFAULT_REGULAR_HC_STAGE0_LINK_FLAGS_BASE_WARN = $(REGULAR_HC_STAGE0_LINK_FLAGS_BASE_QA)

# QA: warning, error, etc. flags.
REGULAR_HC_STAGE0_FLAGS_BASE_QA ?= $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_QA)
DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_QA = $(REGULAR_HC_STAGE0_FLAGS_BASE_QA_BASE) $(REGULAR_HC_STAGE0_FLAGS_BASE_QA_BASE_EXTRA)

REGULAR_HC_STAGE0_LINK_FLAGS_BASE_QA ?= $(DEFAULT_REGULAR_HC_STAGE0_LINK_FLAGS_BASE_QA)
DEFAULT_REGULAR_HC_STAGE0_LINK_FLAGS_BASE_QA = $(REGULAR_HC_STAGE0_FLAGS_BASE_QA)

REGULAR_HC_STAGE0_FLAGS_BASE_QA_BASE ?= $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_QA_BASE)
DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_QA_BASE = -Wall -Wno-tabs

REGULAR_HC_STAGE0_FLAGS_BASE_QA_BASE_EXTRA ?= $(DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_QA_EXTRA)
DEFAULT_REGULAR_HC_STAGE0_FLAGS_BASE_QA_BASE_EXTRA =

HC_STAGE0_WORKDIR ?= $(DEFAULT_HC_STAGE0_WORKDIR)

DEFAULT_HC_STAGE0_WORKDIR = $(BUILD_DIR)/direct-builddir/ghc-outputdir

HC_STAGE0_OUT_PREFIX_FLAGS   ?= $(DEFAULT_HC_STAGE0_OUT_PREFIX_FLAGS)
HC_STAGE0_OUT_SUFFIX_FLAGS   ?= $(DEFAULT_HC_STAGE0_OUT_SUFFIX_FLAGS)
HC_STAGE0_IN_PREFIX_FLAGS    ?= $(DEFAULT_HC_STAGE0_IN_PREFIX_FLAGS)
HC_STAGE0_IN_SUFFIX_FLAGS    ?= $(DEFAULT_HC_STAGE0_IN_SUFFIX_FLAGS)
HC_STAGE0_DEPIN_PREFIX_FLAGS ?= $(DEFAULT_HC_STAGE0_DEPIN_PREFIX_FLAGS)
HC_STAGE0_DEPIN_SUFFIX_FLAGS ?= $(DEFAULT_HC_STAGE0_DEPIN_SUFFIX_FLAGS)
HC_STAGE0_DEPINS_HIDE        ?= $(DEFAULT_HC_STAGE0_DEPINS_HIDE)

HC_STAGE0_LINK_OUT_PREFIX_FLAGS   ?= $(DEFAULT_HC_STAGE0_LINK_OUT_PREFIX_FLAGS)
HC_STAGE0_LINK_OUT_SUFFIX_FLAGS   ?= $(DEFAULT_HC_STAGE0_LINK_OUT_SUFFIX_FLAGS)
HC_STAGE0_LINK_IN_PREFIX_FLAGS    ?= $(DEFAULT_HC_STAGE0_LINK_IN_PREFIX_FLAGS)
HC_STAGE0_LINK_IN_SUFFIX_FLAGS    ?= $(DEFAULT_HC_STAGE0_LINK_IN_SUFFIX_FLAGS)
HC_STAGE0_LINK_DEPIN_PREFIX_FLAGS ?= $(DEFAULT_HC_STAGE0_LINK_DEPIN_PREFIX_FLAGS)
HC_STAGE0_LINK_DEPIN_SUFFIX_FLAGS ?= $(DEFAULT_HC_STAGE0_LINK_DEPIN_SUFFIX_FLAGS)
HC_STAGE0_LINK_DEPINS_HIDE        ?= $(DEFAULT_HC_STAGE0_LINK_DEPINS_HIDE)

DEFAULT_HC_STAGE0_OUT_PREFIX_FLAGS   = -o
DEFAULT_HC_STAGE0_OUT_SUFFIX_FLAGS   =
DEFAULT_HC_STAGE0_IN_PREFIX_FLAGS    =
DEFAULT_HC_STAGE0_IN_SUFFIX_FLAGS    =
DEFAULT_HC_STAGE0_DEPIN_PREFIX_FLAGS =
DEFAULT_HC_STAGE0_DEPIN_SUFFIX_FLAGS =
DEFAULT_HC_STAGE0_DEPINS_HIDE        = 1

DEFAULT_HC_STAGE0_LINK_OUT_PREFIX_FLAGS   = -o
DEFAULT_HC_STAGE0_LINK_OUT_SUFFIX_FLAGS   =
DEFAULT_HC_STAGE0_LINK_IN_PREFIX_FLAGS    =
DEFAULT_HC_STAGE0_LINK_IN_SUFFIX_FLAGS    =
DEFAULT_HC_STAGE0_LINK_DEPIN_PREFIX_FLAGS =
DEFAULT_HC_STAGE0_LINK_DEPIN_SUFFIX_FLAGS =
DEFAULT_HC_STAGE0_LINK_DEPINS_HIDE        = 1

# BUILD_SYS_TOOLS=1 build configuration.
SYS_TOOLS_CABAL ?= $(DEFAULT_SYS_TOOLS_CABAL)

DEFAULT_SYS_TOOLS_CABAL = $(CABAL)

CABAL ?= $(DEFAULT_CABAL)
DEFAULT_CABAL = cabal

CABAL_FLAGS ?= $(DEFAULT_CABAL_FLAGS)
DEFAULT_CABAL_FLAGS =

CABAL_USE_LOCAL_STORE ?= $(DEFAULT_CABAL_USE_LOCAL_STORE)

DEFAULT_CABAL_USE_LOCAL_STORE = 1

CABAL_CONFIGURE_COMMAND ?= $(DEFAULT_CABAL_CONFIGURE_COMMAND)
DEFAULT_CABAL_CONFIGURE_COMMAND = v2-configure

# (Note that the Makefile configuration assumes paths are correct.)
CABAL_CONFIGURE_BASE_FLAGS ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS)
DEFAULT_CABAL_CONFIGURE_BASE_FLAGS = \
	$(CABAL_CONFIGURE_BASE_FLAGS_COMPILER) \
	$(CABAL_CONFIGURE_BASE_FLAGS_BASE_DIRS) \
	$(CABAL_CONFIGURE_BASE_FLAGS_EXTRA_DIRS) \
	$(CABAL_CONFIGURE_BASE_FLAGS_SETTINGS) \
	$(CABAL_CONFIGURE_BASE_FLAGS_EXTRA) \
	$(EMPTY)

CABAL_CONFIGURE_BASE_FLAGS_COMPILER   ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_COMPILER)
CABAL_CONFIGURE_BASE_FLAGS_BASE_DIRS  ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_BASE_DIRS)
CABAL_CONFIGURE_BASE_FLAGS_EXTRA_DIRS ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_EXTRA_DIRS)
CABAL_CONFIGURE_BASE_FLAGS_SETTINGS   ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS)
CABAL_CONFIGURE_BASE_FLAGS_EXTRA      ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_EXTRA)

DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_COMPILER   = \
	--with-compiler="$(HC_STAGE0)" \
 \
	$(EMPTY)

DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_BASE_DIRS  = \
	--builddir="$(BUILD_DIR)/cabal-builddir" \
	--prefix="$(BUILD_DIR)/cabal-builddir/install" \
 \
	$(EMPTY)

DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_EXTRA_DIRS = \
	--bindir="/bin" \
	--libdir="/lib" \
	--libsubdir="/ahc-core/lib" \
	--dynlibdir="/lib" \
	--libexecdir="/lib/ahc-core/libexec" \
	--libexecsubdir="/ahc-core/lib/ahc-core/libexec" \
	--datadir="/share" \
	--datasubdir="/ahc-core/share" \
	--docdir="/share/doc" \
	--htmldir="/share/doc/html" \
	--haddockdir="/share/doc/haddock" \
	--sysconfdir="/etc" \
 \
	$(EMPTY)

DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS   = \
	$(CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF) \
 \
	$(EMPTY)

DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_EXTRA      = \
	$(EMPTY)

CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF)
DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF = \
	$(CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_$(BUILD_DEBUG)) \
	$(EMPTY)

CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_0 ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_0)
CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_  ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_ )
CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_1 ?= $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_1)

DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_  = $(DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_0)

DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_0 = \
	$(EMPTY)

DEFAULT_CABAL_CONFIGURE_BASE_FLAGS_SETTINGS_DEBUGPROF_1 = \
	--enable-debug-info \
	-v \
	--disable-optimization \
	--enable-profiling \
	--disable-split-sections --disable-executable-stripping --disable-library-stripping \
 \
	--enable-tests \
 \
	$(EMPTY)

CABAL_CONFIGURE_EXTRA_FLAGS ?= $(DEFAULT_CABAL_CONFIGURE_EXTRA_FLAGS)
DEFAULT_CABAL_CONFIGURE_EXTRA_FLAGS =

CABAL_CONFIGURE_STAGE_FLAGS ?= $(DEFAULT_CABAL_CONFIGURE_STAGE_FLAGS)
DEFAULT_CABAL_CONFIGURE_STAGE_FLAGS =

CABAL_CONFIGURE_FLAGS ?= $(DEFAULT_CABAL_CONFIGURE_FLAGS)
DEFAULT_CABAL_CONFIGURE_FLAGS = \
	$(CABAL_CONFIGURE_BASE_FLAGS) \
	$(CABAL_CONFIGURE_STAGE_FLAGS) \
	$(CABAL_CONFIGURE_EXTRA_FLAGS) \
	$(EMPTY)

CABAL_COMPILE_COMMAND ?= $(DEFAULT_CABAL_COMPILE_COMMAND)
DEFAULT_CABAL_COMPILE_COMMAND = v2-build

CABAL_LIBINSTALL_COMMAND ?= $(DEFAULT_CABAL_LIBINSTALL_COMMAND)
DEFAULT_CABAL_LIBINSTALL_COMMAND = v2-install

CABAL_INSTALL_COMMAND ?= $(DEFAULT_CABAL_INSTALL_COMMAND)
DEFAULT_CABAL_INSTALL_COMMAND = v2-install

CABAL_COMPILE_FLAGS ?= $(DEFAULT_CABAL_COMPILE_FLAGS)
DEFAULT_CABAL_COMPILE_FLAGS = \
	$(CABAL_COMPILE_BASE_FLAGS) \
	$(CABAL_COMPILE_STAGE_FLAGS) \
	$(CABAL_COMPILE_EXTRA_FLAGS) \
	$(EMPTY)

CABAL_LIBINSTALL_FLAGS ?= $(DEFAULT_CABAL_LIBINSTALL_FLAGS)
DEFAULT_CABAL_LIBINSTALL_FLAGS = \
	$(CABAL_LIBINSTALL_BASE_FLAGS) \
	$(CABAL_LIBINSTALL_STAGE_FLAGS) \
	$(CABAL_LIBINSTALL_EXTRA_FLAGS) \
	$(EMPTY)

CABAL_INSTALL_FLAGS ?= $(DEFAULT_CABAL_INSTALL_FLAGS)
DEFAULT_CABAL_INSTALL_FLAGS = \
	$(CABAL_INSTALL_BASE_FLAGS) \
	$(CABAL_INSTALL_STAGE_FLAGS) \
	$(CABAL_INSTALL_EXTRA_FLAGS) \
	$(EMPTY)

CABAL_COMPILE_EXTRA_FLAGS    ?= $(DEFAULT_CABAL_COMPILE_EXTRA_FLAGS)
CABAL_LIBINSTALL_EXTRA_FLAGS ?= $(DEFAULT_CABAL_LIBINSTALL_EXTRA_FLAGS)
CABAL_INSTALL_EXTRA_FLAGS    ?= $(DEFAULT_CABAL_INSTALL_EXTRA_FLAGS)
DEFAULT_CABAL_COMPILE_EXTRA_FLAGS    =
DEFAULT_CABAL_LIBINSTALL_EXTRA_FLAGS =
DEFAULT_CABAL_INSTALL_EXTRA_FLAGS    =

CABAL_COMPILE_STAGE_FLAGS    ?= $(DEFAULT_CABAL_COMPILE_STAGE_FLAGS)
CABAL_LIBINSTALL_STAGE_FLAGS ?= $(DEFAULT_CABAL_LIBINSTALL_STAGE_FLAGS)
CABAL_INSTALL_STAGE_FLAGS    ?= $(DEFAULT_CABAL_INSTALL_STAGE_FLAGS)
DEFAULT_CABAL_COMPILE_STAGE_FLAGS    =
DEFAULT_CABAL_LIBINSTALL_STAGE_FLAGS =
DEFAULT_CABAL_INSTALL_STAGE_FLAGS    =

CABAL_COMPILE_BASE_FLAGS    ?= $(DEFAULT_CABAL_COMPILE_BASE_FLAGS)
CABAL_LIBINSTALL_BASE_FLAGS ?= $(DEFAULT_CABAL_LIBINSTALL_BASE_FLAGS)
CABAL_INSTALL_BASE_FLAGS    ?= $(DEFAULT_CABAL_INSTALL_BASE_FLAGS)

DEFAULT_CABAL_COMPILE_BASE_FLAGS = \
	$(EMPTY)

DEFAULT_CABAL_LIBINSTALL_BASE_FLAGS = \
	$(CABAL_OVERWRITE_POLICY_FLAGS) \
 \
	--lib \
 \
	--package-env=$(CABAL_PACKAGE_ENV) \
 \
	$(EMPTY)

CABAL_PACKAGE_ENV ?= $(DEFAULT_CABAL_PACKAGE_ENV)

DEFAULT_CABAL_PACKAGE_ENV = $(BUILD_DIR)/cabal-builddir/package-env.env

DEFAULT_CABAL_INSTALL_BASE_FLAGS = \
	$(CABAL_OVERWRITE_POLICY_FLAGS) \
 \
	--installdir="$(BUILD_DIR)/cabal-builddir/exe-install" \
 \
	$(EMPTY)

CABAL_OVERWRITE_POLICY_FLAGS ?= $(DEFAULT_CABAL_OVERWRITE_POLICY_FLAGS)
DEFAULT_CABAL_OVERWRITE_POLICY_FLAGS = \
	--overwrite-policy=always \
	$(EMPTY)

CABAL_COMMON_FLAGS ?= $(DEFAULT_CABAL_COMMON_FLAGS)
DEFAULT_CABAL_COMMON_FLAGS = \
	$(CABAL_COMMON_FLAGS_BASE) \
	$(CABAL_COMMON_FLAGS_STAGE) \
	$(CABAL_COMMON_FLAGS_EXTRA) \
 \
	$(EMPTY)

CABAL_COMMON_FLAGS_BASE ?= $(DEFAULT_CABAL_COMMON_FLAGS_BASE)
CABAL_COMMON_FLAGS_STAGE ?= $(DEFAULT_CABAL_COMMON_FLAGS_STAGE)
CABAL_COMMON_FLAGS_EXTRA ?= $(DEFAULT_CABAL_COMMON_FLAGS_EXTRA)

DEFAULT_CABAL_COMMON_FLAGS_EXTRA =
DEFAULT_CABAL_COMMON_FLAGS_STAGE =

DEFAULT_CABAL_COMMON_FLAGS_BASE = \
	$(CABAL_COMMON_FLAGS_BASE_STORE_DIRS) \
	$(CABAL_COMMON_FLAGS_BASE_PROJECT_FLAGS) \
 \
	$(EMPTY)

CABAL_COMMON_FLAGS_BASE_STORE_DIRS ?= $(DEFAULT_CABAL_COMMON_FLAGS_BASE_STORE_DIRS)
CABAL_COMMON_FLAGS_BASE_PROJECT_FLAGS ?= $(DEFAULT_CABAL_COMMON_FLAGS_BASE_PROJECT_FLAGS)

DEFAULT_CABAL_COMMON_FLAGS_BASE_STORE_DIRS = \
	$(CABAL_COMMON_FLAGS_BASE_STORE_DIRS_$(CABAL_USE_LOCAL_STORE)) \
 \
	$(EMPTY)

CABAL_COMMON_FLAGS_BASE_STORE_DIRS_0 ?= $(DEFAULT_CABAL_COMMON_FLAGS_BASE_STORE_DIRS_0)
CABAL_COMMON_FLAGS_BASE_STORE_DIRS_  ?= $(DEFAULT_CABAL_COMMON_FLAGS_BASE_STORE_DIRS_)
CABAL_COMMON_FLAGS_BASE_STORE_DIRS_1 ?= $(DEFAULT_CABAL_COMMON_FLAGS_BASE_STORE_DIRS_1)

CABAL_COMMON_FLAGS_BASE_STORE_DIRS_ = $(DEFAULT_CABAL_COMMON_FLAGS_BASE_STORE_DIRS_0)

DEFAULT_CABAL_COMMON_FLAGS_BASE_STORE_DIRS_0 = \
	$(EMPTY)

DEFAULT_CABAL_COMMON_FLAGS_BASE_STORE_DIRS_1 = \
	--store-dir=$(CABAL_LOCAL_BUILD_STORE_DIR) \
 \
	$(EMPTY)

DEFAULT_CABAL_COMMON_FLAGS_BASE_PROJECT_FLAGS = \
	--project-file="$(CABAL_PROJECT_FILE)" \
 \
	$(EMPTY)

CABAL_PROJECT_DIR ?= $(DEFAULT_CABAL_PROJECT_DIR)

DEFAULT_CABAL_PROJECT_DIR = ./../../

# Keeping the ‘.local’ file inside $(BUILD_DIR) seems to work so long as it's
# relative.
CABAL_PROJECT_FILE ?= $(DEFAULT_CABAL_PROJECT_FILE)

DEFAULT_CABAL_PROJECT_FILE = $(_CABAL_PROJECT_FILE_REL)

_CABAL_PROJECT_FILE_DIRECT = $(BUILD_DIR)/cabal-builddir/cabal-contents.project
_CABAL_PROJECT_FILE_REL = $$(realpath --relative-to="$(CABAL_PROJECT_DIR)" -- "$$(dirname -- "$(_CABAL_PROJECT_FILE_DIRECT)")")/$$(basename -- "$(_CABAL_PROJECT_FILE_DIRECT)")

CABAL_COMMON_SUB_FLAGS ?= $(DEFAULT_CABAL_COMMON_SUB_FLAGS)
DEFAULT_CABAL_COMMON_SUB_FLAGS = \
	$(CABAL_COMMON_SUB_FLAGS_BASE) \
	$(CABAL_COMMON_SUB_FLAGS_STAGE) \
	$(CABAL_COMMON_SUB_FLAGS_EXTRA) \
 \
	$(EMPTY)

CABAL_COMMON_SUB_FLAGS_BASE ?= $(DEFAULT_CABAL_COMMON_SUB_FLAGS_BASE)
CABAL_COMMON_SUB_FLAGS_STAGE ?= $(DEFAULT_CABAL_COMMON_SUB_FLAGS_STAGE)
CABAL_COMMON_SUB_FLAGS_EXTRA ?= $(DEFAULT_CABAL_COMMON_SUB_FLAGS_EXTRA)

DEFAULT_CABAL_COMMON_SUB_FLAGS_EXTRA =
DEFAULT_CABAL_COMMON_SUB_FLAGS_STAGE =

# Just copy some ‘v2-configure’ flags to all normal sub-commands.
DEFAULT_CABAL_COMMON_SUB_FLAGS_BASE = \
	$(CABAL_CONFIGURE_BASE_FLAGS_COMPILER) \
	$(CABAL_CONFIGURE_BASE_FLAGS_BASE_DIRS) \
	$(CABAL_CONFIGURE_BASE_FLAGS_EXTRA_DIRS) \
	$(CABAL_CONFIGURE_BASE_FLAGS_SETTINGS) \
 \
	$(EMPTY)

CABAL_LOCAL_BUILD_STORE_DIR = $(BUILD_DIR)/cabal-builddir/local-store

# Select which stage compiler of ahc-minimal to choose.
$(BUILD_DIR)/ahc-minimal: $(BUILD_DIR)/ahc-minimal_$(BUILD_STAGE2)
	install -m 0775 -- "$<" "$@"
$(BUILD_DIR)/ahc-minimal_0 $(BUILD_DIR)/ahc-minimal_: $(BUILD_DIR)/ahc-minimal-stage1
	install -m 0775 -- "$<" "$@"
$(BUILD_DIR)/ahc-minimal_1: $(BUILD_DIR)/ahc-minimal-stage2
	install -m 0775 -- "$<" "$@"

# The ahc-minimal program.
# Select a means to build ‘ahc-minimal’ by ‘BUILD_SYS_TOOLS’.
$(BUILD_DIR)/ahc-minimal-stage1: $(BUILD_DIR)/ahc-minimal-stage1_$(BUILD_SYS_TOOLS)
	install -m 0775 -- "$<" "$@"

# Build directory setup: ensure they exist.
#$(BUILD_DIR):
#	install -d -m 0775 -- "$@"
$(BUILD_DIR)/cabal-builddir: | $(BUILD_DIR)
	install -d -m 0775 -- "$@"
$(BUILD_DIR)/cabal-builddir/install: | $(BUILD_DIR)/cabal-builddir
	install -d -m 0775 -- "$@"
$(BUILD_DIR)/direct-builddir: | $(BUILD_DIR)
	install -d -m 0775 -- "$@"
$(BUILD_DIR)/direct-builddir/ghc-outputdir: | $(BUILD_DIR)/direct-builddir
	install -d -m 0775 -- "$@"

# Configuration for direct builds.
#
# The ordering is such that dependent directories (parents) appear before their
# dependents.
#
# It's also organized into groups of parent directories.
_DIRECT_SRC_TREE_REL_DIRS = \
	Language \
 \
	Language/Haskell2010 \
 \
	Language/Haskell2010/Ahc \
 \
	Language/Haskell2010/Ahc/Benchmark \
	Language/Haskell2010/Ahc/Data \
	Language/Haskell2010/Ahc/Frontends \
	Language/Haskell2010/Ahc/Meta \
	Language/Haskell2010/Ahc/Syntax \
	Language/Haskell2010/Ahc/Tests \
 \
	Language/Haskell2010/Ahc/Syntax/Haskell2010 \
 \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple \
 \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST \
 \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base \
 \
	$(EMPTY)

_DIRECT_SRC_TREE_REL_LIB_SRCS = \
	Language/Haskell2010/Ahc/Benchmark/Benchmark.hs \
	Language/Haskell2010/Ahc/Data/Fixed.hs \
	Language/Haskell2010/Ahc/Data/Proxy.hs \
	Language/Haskell2010/Ahc/Frontends/CLI.hs \
	Language/Haskell2010/Ahc/Meta/Ahc.hs \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST.hs \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base.hs \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base/ExclusionStructures.hs \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base/LexicalFoundation.hs \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base/RegularStructures.hs \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/DefaultLinking.hs \
	Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/StandardLinking.hs \
	Language/Haskell2010/Ahc/Tests/Tests.hs \
 \
	$(EMPTY)

_DIRECT_SRC_TREE_REL_EXE_SRCS = \
	Language/Haskell2010/Ahc/Main.hs \
	Language/Haskell2010/Ahc/Benchmark/Main.hs \
	Language/Haskell2010/Ahc/Tests/Main.hs \
 \
	$(EMPTY)

_DIRECT_SRC_TREE_REL_SRCS = \
	$(_DIRECT_SRC_TREE_REL_LIB_SRCS) \
	$(_DIRECT_SRC_TREE_REL_EXE_SRCS) \
 \
	$(EMPTY)

_DIRECT_SRC_TREE_REL_RAWOBJS = $(patsubst %.hs,%.o,$(_DIRECT_SRC_TREE_REL_SRCS))

_DIRECT_SRC_TREE_DIRS = $(patsubst %,$(BUILD_DIR)/direct-builddir/%,$(_DIRECT_SRC_TREE_REL_DIRS))

_DIRECT_BUILD_ART_SUFF = $(_DIRECT_BUILD_ART_SUFF_BUILTIN)

_DIRECT_BUILD_ART_SUFF_BUILTIN = .o

_DIRECT_BUILD_LIB_OBJS = $(patsubst %.hs,$(BUILD_DIR)/direct-builddir/%$(_DIRECT_BUILD_ART_SUFF),$(_DIRECT_SRC_TREE_REL_LIB_SRCS))
_DIRECT_BUILD_EXE_OBJS = $(patsubst %.hs,$(BUILD_DIR)/direct-builddir/%$(_DIRECT_BUILD_ART_SUFF),$(_DIRECT_SRC_TREE_REL_EXE_SRCS))

_DIRECT_BUILD_MAIN_OBJ = $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Main.o

_DIRECT_BUILD_MAIN_SRC = Language/Haskell2010/Ahc/Main.hs
_DIRECT_BUILD_MAIN = Language/Haskell2010/Ahc/ahc-minimal

_DIRECT_BUILD_SRCS = $(patsubst %,$(SRC_DIR)/%,$(_DIRECT_SRC_TREE_REL_SRCS))

# The local store.  Available for optional use.
$(BUILD_DIR)/cabal-builddir/local-store: | $(BUILD_DIR)/cabal-builddir
	install -d -m 0775 -- "$@"

# Use ‘cabal’ to build ‘ahc-minimal’.
# (Getting ‘$(BUILD_DIR)/ahc-minimal’.)
# Depend on ‘_DIRECT_BUILD_SRCS’ to let make know when we should rebuild.
$(BUILD_DIR)/ahc-minimal-stage1_1: $(_DIRECT_BUILD_SRCS) $(CABAL_LOCAL_BUILD_STORE_DIR) | $(BUILD_DIR)/cabal-builddir
	install -d -m 0775 -- "$(BUILD_DIR)/cabal-builddir/install"
	install -d -m 0775 -- "$(BUILD_DIR)/cabal-builddir/exe-install"
	ln -nsf -- "$$(realpath -- "../../cabal.project")" "$(BUILD_DIR)/cabal-builddir/cabal-contents.project"
	$(SYS_TOOLS_CABAL) \
		$(CABAL_FLAGS) \
		$(CABAL_COMMON_FLAGS) \
		$(CABAL_CONFIGURE_COMMAND) \
		$(CABAL_COMMON_SUB_FLAGS) \
		$(CABAL_CONFIGURE_FLAGS) \
		$(EMPTY)
	$(SYS_TOOLS_CABAL) \
		$(CABAL_FLAGS) \
		$(CABAL_COMMON_FLAGS) \
		$(CABAL_COMPILE_COMMAND) \
		$(CABAL_COMMON_SUB_FLAGS) \
		$(CABAL_COMPILE_FLAGS) \
		$(EMPTY)
	$(SYS_TOOLS_CABAL) \
		$(CABAL_FLAGS) \
		$(CABAL_COMMON_FLAGS) \
		$(CABAL_LIBINSTALL_COMMAND) \
		$(CABAL_COMMON_SUB_FLAGS) \
		$(CABAL_LIBINSTALL_FLAGS) \
		$(EMPTY)
	$(SYS_TOOLS_CABAL) \
		$(CABAL_FLAGS) \
		$(CABAL_COMMON_FLAGS) \
		$(CABAL_INSTALL_COMMAND) \
		$(CABAL_COMMON_SUB_FLAGS) \
		$(CABAL_INSTALL_FLAGS) \
		$(EMPTY)
	@#install -m 0775 -- "$(BUILD_DIR)/cabal-builddir/install/bin/ahc-minimal" "$@"
	install -m 0775 -- "$(BUILD_DIR)/cabal-builddir/exe-install/ahc-minimal" "$@"

# Bypass ‘cabal’ and build it manually, using our toolchain variable
# configuration like ‘HC_STAGE0’.
# (Getting ‘$(BUILD_DIR)/ahc-minimal’.)
.PHONY: build-direct-main
build-direct-main: $(BUILD_DIR)/ahc-minimal-stage1_0
$(BUILD_DIR)/ahc-minimal-stage1_0 $(BUILD_DIR)/ahc-minimal-stage1_ : $(BUILD_DIR)/direct-builddir/$(_DIRECT_BUILD_MAIN) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree build-direct-units
	install -m 0775 -- "$<" "$@"

# Prefix $(BUILD_DIR)/direct-builddir/ to the tree, and make sure the hierarchy
# exists.
.PHONY: build-direct-builddir-tree
build-direct-builddir-tree: $(_DIRECT_SRC_TREE_DIRS) | $(BUILD_DIR)/direct-builddir
$(_DIRECT_SRC_TREE_DIRS): | $(BUILD_DIR)/direct-builddir
	install -d -m 0775 -- "$@"

# Build the ‘.o’ files - _that_ kind of unit.
.PHONY: build-direct-units
build-direct-units: $(_DIRECT_BUILD_OBJS) | build-direct-builddir-tree

# Used as an ‘if’ conditional, expanding appropriately according to the value
# of ‘HC_STAGE0_DEPINS_HIDE’, to determine whether to pass as extra arguments
# the ‘.o’ files for imports.  (Invert the condition.)
_CONDITION_STRING_HC_STAGE0_DEPINS_HIDE   = $(_CONDITION_STRING_HC_STAGE0_DEPINS_HIDE_$(HC_STAGE0_DEPINS_HIDE))
_CONDITION_STRING_HC_STAGE0_DEPINS_HIDE_  = $(_CONDITION_STRING_HC_STAGE0_DEPINS_HIDE_0)
_CONDITION_STRING_HC_STAGE0_DEPINS_HIDE_0 = 1
_CONDITION_STRING_HC_STAGE0_DEPINS_HIDE_1 =

$(BUILD_DIR)/direct-builddir/%$(_DIRECT_BUILD_ART_SUFF): $(SRC_DIR)/%.hs | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree
	$(HC_STAGE0) $(HC_STAGE0_FLAGS) $(HC_STAGE0_OUT_PREFIX_FLAGS) "$@" $(HC_STAGE0_OUT_SUFFIX_FLAGS) $(HC_STAGE0_IN_PREFIX_FLAGS) "$<" $(HC_STAGE0_IN_SUFFIX_FLAGS) $(HC_STAGE0_DEPIN_PREFIX_FLAGS) $(if $(_CONDITION_STRING_HC_STAGE0_DEPINS_HIDE),$(filter-out $<,$^)) $(HC_STAGE0_DEPIN_SUFFIX_FLAGS)

$(BUILD_DIR)/direct-builddir/$(_DIRECT_BUILD_MAIN): $(_DIRECT_BUILD_LIB_OBJS) $(_DIRECT_BUILD_MAIN_OBJ) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree build-direct-units
	$(HC_STAGE0_LINK) $(HC_STAGE0_LINK_FLAGS) $(HC_STAGE0_LINK_OUT_PREFIX_FLAGS) "$@" $(HC_STAGE0_LINK_OUT_SUFFIX_FLAGS) $(HC_STAGE0_LINK_IN_PREFIX_FLAGS) $^ $(HC_STAGE0_LINK_IN_SUFFIX_FLAGS) $(HC_STAGE0_LINK_DEPIN_PREFIX_FLAGS) $(HC_STAGE0_LINK_DEPIN_SUFFIX_FLAGS)

# ################################################################
# Manual dependency declarations (import statements).
# ################################################################

# Since we support building by hand, we must be aware of dependencies made
# though ‘import’ statements.  For each intra-package import, we must add a
# dependency rule in this section of the form: 
# 	$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/…Importer…$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/…Imported…$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Benchmark/Main$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Benchmark/Benchmark$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Main$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Frontends/CLI$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/DefaultLinking$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base/RegularStructures$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base/LexicalFoundation$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base/ExclusionStructures$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/DefaultLinking$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/StandardLinking$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/StandardLinking$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Data/Fixed$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/StandardLinking$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Data/Proxy$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/StandardLinking$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Syntax/Haskell2010/Simple/AST/Base$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

$(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Tests/Main$(_DIRECT_BUILD_ART_SUFF): $(BUILD_DIR)/direct-builddir/Language/Haskell2010/Ahc/Tests/Tests$(_DIRECT_BUILD_ART_SUFF) | $(BUILD_DIR)/direct-builddir build-direct-builddir-tree

# Configuration for build-prevalidate-import-consistency.
PREVALIDATOR_ENABLE_IMPORT_CONSISTENCY ?= $(DEFAULT_PREVALIDATOR_ENABLE_IMPORT_CONSISTENCY)

DEFAULT_PREVALIDATOR_ENABLE_IMPORT_CONSISTENCY = 1

_PREVALIDATOR_IMPORT_CONSISTENCY_TARGETS   = $(_PREVALIDATOR_IMPORT_CONSISTENCY_TARGETS_$(PREVALIDATOR_ENABLE_IMPORT_CONSISTENCY))
_PREVALIDATOR_IMPORT_CONSISTENCY_TARGETS_  = _PREVALIDATOR_IMPORT_CONSISTENCY_TARGETS_0
_PREVALIDATOR_IMPORT_CONSISTENCY_TARGETS_0 =
_PREVALIDATOR_IMPORT_CONSISTENCY_TARGETS_1 = $(BUILD_DIR)/prevalidate-import-consistency.stamp

# Fail if we detect an intra-package import that we don't list in our Makefile
# or visa versa.  This prevalidator assumes that there are multiple
# intra-package imports and source files.
.PHONY: build-prevalidate-import-consistency
build-prevalidate-import-consistency: $(_PREVALIDATOR_IMPORT_CONSISTENCY_TARGETS)
$(BUILD_DIR)/prevalidate-import-consistency.stamp: $(_PREVALIDATE_IMPORT_CONSISTENCY_DEPS) | build-dirs
	@# Print our script, then pipe it to run it.
	@n=$$'\n' && printf -- '%s\n' " $$n\
		#!/usr/bin/env bash $$n\
		set -ueE -o pipefail $$n\
		trap 'echo \"An error (code \$$?) occurred on line \$${LINENO:-} from ‘\$${BASH_SOURCE[0]:-}’; aborting…\" 1>&2' ERR $$n\
 $$n\
		main() { $$n\
			# First collect all source imports. $$n\
			# Make a variable holding lines like: $$n\
			# 	Language/Haskell/2010/Ahc/Syntax/Haskell2010/Simple/Ast/Base <- Language/Haskell/2010/Ahc/Syntax/Haskell2010/Simple/Ast/Base/RegularStructures $$n\
			# 	Language/Haskell/2010/Ahc/Syntax/Haskell2010/Simple/Ast/Base <- Language/Haskell/2010/Ahc/Syntax/Haskell2010/Simple/Ast/Base/ExclusionStructures $$n\
			# 	Language/Haskell/2010/Ahc/Syntax/Haskell2010/Tests/Main <- Language/Haskell/2010/Ahc/Syntax/Haskell2010/Tests/Tests $$n\
			# 	… $$n\
			local srcs=\"\" $$n\
			srcs=\"\$$(find './src/Language/Haskell2010/Ahc' -regextype egrep -iname '*.hs' -type f -regex '^(|(|\\.\\/)src\\/Language\\/Haskell2010\\/Ahc\\/)([[:alnum:]]+\\/)*([[:alnum:]]+)(\\.hs)\$$')\" $$n\
 $$n\
			# (Make sure we haven't found <= 1 files.) $$n\
			local numSrcsIfNonempty=\"\" $$n\
			numSrcsIfNonempty=\"\$$(printf -- '%s\\n' \"\$${srcs}\" | wc -l)\" $$n\
			if [[ \"\$${numSrcsIfNonempty}\" -le \"1\" ]]; then $$n\
				printf -- '%s\\n' \"Error: build-prevalidate-import-consistency: internal error: we failed to find source files, as we only detected a count <= 1.\" 1>&2 $$n\
				printf -- '%s\\n' \"	srcs              : \$$srcs\" 1>&2 $$n\
				printf -- '%s\\n' \"	numSrcsIfNonempty : \$$numSrcsIfNonempty\" 1>&2 $$n\
				# Skip the default error printer, since we already $$n\
				# printed one. $$n\
				exec false $$n\
			fi $$n\
 $$n\
			# Scan for intra-package imports. $$n\
			local srcsImports=\"\" $$n\
			srcsImports=\"\" $$n\
			for src in \$$srcs; do $$n\
				local srcMod=\"\" $$n\
				srcMod=\"\$$(printf -- '%s\\n' \"\$${src}\" | sed -nEe 's/\\.hs\$$//g; s@^(|\\./)(|src/)@@g; s@/@.@g; p; d')\" $$n\
				local srcModSedSafe=\"\" $$n\
				srcModSedSafe=\"\$$(printf -- '%s\\n' \"\$${srcMod}\" | sed -nEe 's/[^[:alnum:]]/\\\\&/g; p')\" $$n\
				local srcImports=\"\" $$n\
				srcImports=\"\$$(sed -nEe '/^(import\\s+(|qualified\\s+))((Language\\.Haskell2010\\.Ahc)([.[:alnum:]]*))(|\\s+(|as\\s+[.[:alnum:]]+)(|\\(.*))\$$/{s//\\3/g; p; b;}; d' < \"\$${src}\" | sed -nEe 's/^/'\"\$${srcModSedSafe}\"' -> &/g; p')\" $$n\
				srcsImports=\"\$$(printf -- '%s\\n' \"\$${srcsImports}\" \"\$${srcImports}\")\" $$n\
			done $$n\
			srcsImports=\"\$$(printf -- '%s\\n' \"\$${srcsImports}\" | sort -h | uniq | sed -nEe '/./p; d')\" $$n\
 $$n\
			# (Make sure we haven't found <= 1 imports.) $$n\
			local numSrcsImportsIfNonempty=\"\" $$n\
			numSrcsImportsIfNonempty=\"\$$(printf -- '%s\\n' \"\$${srcsImports}\" | wc -l)\" $$n\
			if [[ \"\$${numSrcsImportsIfNonempty}\" -le \"1\" ]]; then $$n\
				printf -- '%s\\n' \"Error: build-prevalidate-import-consistency: internal error: we failed to find source imports, as we only detected a count <= 1.\" 1>&2 $$n\
				printf -- '%s\\n' \"	srcsImports              : \$$srcsImports\" 1>&2 $$n\
				printf -- '%s\\n' \"	numSrcsImportsIfNonempty : \$$numSrcsImportsIfNonempty\" 1>&2 $$n\
				# Skip the default error printer, since we already $$n\
				# printed one. $$n\
				exec false $$n\
			fi $$n\
 $$n\
			# For debugging. $$n\
			if false; then $$n\
				printf -- '%s\\n' \"Found intra-package imports:\" \"\$${srcsImports}\" 1>&2 $$n\
			fi $$n\
 $$n\
			# Scan our own Makefile import declarations for dependencies. $$n\
			local makefileImports=\"\" $$n\
			makefileImports=\"\" $$n\
			makefileImports=\"\$$(sed -nEe '/^(\\\$$\\(BUILD_DIR\\)\\/direct-builddir\\/)(Language\\/Haskell2010\\/Ahc\\/)((([/[:alnum:]])+)([[:alnum:]]+))(\\\$$\\(_DIRECT_BUILD_ART_SUFF\\):\\s*\\\$$\\(BUILD_DIR\\)\\/direct-builddir\\/)(Language\\/Haskell2010\\/Ahc\\/)((([/[:alnum:]])+)([[:alnum:]]+))\\\$$\\(_DIRECT_BUILD_ART_SUFF\\)(|\\s+.*)\$$/{s//\\2\\3.hs -> \\8\\9.hs/g; s/\\.hs//g; s@/@.@g; p; b; }; d' < \"./Makefile\")\" $$n\
			makefileImports=\"\$$(printf -- '%s\\n' \"\$${makefileImports}\" | sort -h | uniq | sed -nEe '/./p; d')\" $$n\
 $$n\
			# For debugging. $$n\
			if false; then $$n\
				printf -- '%s\\n' \"Found Makefile imports:\" \"\$${makefileImports}\" 1>&2 $$n\
			fi $$n\
 $$n\
			# (Make sure we haven't found <= 1 makefile imports.) $$n\
			local numMakefileImportsIfNonempty=\"\" $$n\
			numMakefileImportsIfNonempty=\"\$$(printf -- '%s\\n' \"\$${makefileImports}\" | wc -l)\" $$n\
			if [[ \"\$${numMakefileImportsIfNonempty}\" -le \"1\" ]]; then $$n\
				printf -- '%s\\n' \"Error: build-prevalidate-import-consistency: internal error: we failed to find makefile imports, as we only detected a count <= 1.\" 1>&2 $$n\
				printf -- '%s\\n' \"	makefileImports              : \$$makefileImports\" 1>&2 $$n\
				printf -- '%s\\n' \"	numMakefileImportsIfNonempty : \$$numMakefileImportsIfNonempty\" 1>&2 $$n\
				# Skip the default error printer, since we already $$n\
				# printed one. $$n\
				exec false $$n\
			fi $$n\
 $$n\
			# (Make sure the graphs match.) $$n\
			if [[ \"\$${makefileImports}\" != \"\$${srcsImports}\" ]]; then $$n\
				printf -- '%s\\n' \"Error: build-prevalidate-import-consistency: error: we detected Makefile's known intra-package imports is not equivalent to the source file intra-package imports.\" 1>&2 $$n\
				printf -- '%s\\n' \"	Please make sure the Makefile's ‘Manual dependency declarations (import statements)’\" 1>&2 $$n\
				printf -- '%s\\n' \"	section is up-to-date with the source's intra-package import statements.’\" 1>&2 $$n\
				printf -- '%s\\n' \"\" 1>&2 $$n\
				if false; then $$n\
					printf -- '%s\\n' \"	makefileImports : \$$makefileImports\" 1>&2 $$n\
					printf -- '%s\\n' \"	srcsImports     : \$$srcsImports\" 1>&2 $$n\
					printf -- '%s\\n' \"\" 1>&2 $$n\
				fi $$n\
				printf -- '%s\\n' \"	Printing diff, actual to expected (makefile to srcs)…\" 1>&2 $$n\
				diff -u --color=always <(printf -- '%s\\n' \"\$${makefileImports}\") <(printf -- '%s\\n' \"\$${srcsImports}\") $$n\
				# Skip the default error printer, since we already $$n\
				# printed one. $$n\
				exec false $$n\
			fi $$n\
 $$n\
			: \"Passed version consistency check.  (No output.)\" $$n\
		} $$n$$n\
 $$n\
		main \"\$$@\" $$n\
	" | sed -nEe 's@^\t@@g; 1b; $$b; p' | xargs --null -- $(_CMD_BUILD_PREV_SHOW)

	touch -- "$@"

# ################################################################
# Multi-stage configuration.
# ################################################################

# The ahc-minimal program.
# Select a means to build ‘ahc-minimal’ by ‘BUILD_SYS_TOOLS’.
$(BUILD_DIR)/ahc-minimal-stage2: $(BUILD_DIR)/ahc-minimal-stage1
	$(warning WARNING: since ‘ahc’ is unimplemented, we'll pretend the stage 0 compiler is the stage 1 compiler.)
	install -d -m 0775 -- "$(BUILD_DIR)/stage2-builddir"
	$(MAKE) BUILD_STAGE2=0 BUILD_DIR="$(BUILD_DIR)/stage2-builddir" \
		HC_STAGE0="$(HC_STAGE0)" HC_STAGE0_FLAGS="$(HC_STAGE0_FLAGS)" HC_STAGE0_LINK="$(HC_STAGE0_LINK)" HC_STAGE0_LINK_FLAGS="$(HC_STAGE0_LINK_FLAGS)" \
 \
		HC_STAGE0_OUT_PREFIX_FLAGS="$(HC_STAGE0_OUT_PREFIX_FLAGS)" HC_STAGE0_OUT_SUFFIX_FLAGS="$(HC_STAGE0_OUT_SUFFIX_FLAGS)" \
		HC_STAGE0_IN_PREFIX_FLAGS="$(HC_STAGE0_IN_PREFIX_FLAGS)" HC_STAGE0_IN_SUFFIX_FLAGS="$(HC_STAGE0_IN_SUFFIX_FLAGS)" \
		HC_STAGE0_DEPIN_PREFIX_FLAGS="$(HC_STAGE0_DEPIN_PREFIX_FLAGS)" HC_STAGE0_DEPIN_SUFFIX_FLAGS="$(HC_STAGE0_DEPIN_SUFFIX_FLAGS)" \
		HC_STAGE0_LINK_OUT_PREFIX_FLAGS="$(HC_STAGE0_LINK_OUT_PREFIX_FLAGS)" HC_STAGE0_LINK_OUT_SUFFIX_FLAGS="$(HC_STAGE0_LINK_OUT_SUFFIX_FLAGS)" \
		HC_STAGE0_LINK_IN_PREFIX_FLAGS="$(HC_STAGE0_LINK_IN_PREFIX_FLAGS)" HC_STAGE0_LINK_IN_SUFFIX_FLAGS="$(HC_STAGE0_LINK_IN_SUFFIX_FLAGS)" \
		HC_STAGE0_LINK_DEPIN_PREFIX_FLAGS="$(HC_STAGE0_LINK_DEPIN_PREFIX_FLAGS)" HC_STAGE0_LINK_DEPIN_SUFFIX_FLAGS="$(HC_STAGE0_LINK_DEPIN_SUFFIX_FLAGS)" \
		HC_STAGE0_LINK_DEPINS_HIDE="$(HC_STAGE0_LINK_DEPINS_HIDE)" \
 \
		CABAL_COMMON_FLAGS_STAGE="$(CABAL_COMMON_FLAGS_STAGE_STAGE2)" \
		CABAL_COMMON_SUB_FLAGS_STAGE="$(CABAL_COMMON_SUB_FLAGS_STAGE_STAGE2)" \
		CABAL_CONFIGURE_STAGE_FLAGS="$(CABAL_CONFIGURE_STAGE_FLAGS_STAGE2)" \
		CABAL_COMPILE_STAGE_FLAGS="$(CABAL_COMPILE_STAGE_FLAGS_STAGE2)" \
		CABAL_LIBINSTALL_STAGE_FLAGS="$(CABAL_LIBINSTALL_STAGE_FLAGS_STAGE2)" \
		CABAL_INSTALL_STAGE_FLAGS="$(CABAL_INSTALL_STAGE_FLAGS_STAGE2)" \
 \
		$(EMPTY)
	install -m 0775 -- "$(BUILD_DIR)/stage2-builddir/ahc-minimal" "$@"
	@#$(warning WARNING: since ‘ahc’ is unimplemented, we'll pretend the stage 1 compiler is also the stage 2 compiler.)
	@#install -m 0775 -- "$<" "$@"

# TODO: let cabal build with ahc.
ifeq ($(strip $(BUILD_SYS_TOOLS)),1)
ifeq ($(strip $(BUILD_STAGE2)),1)
_CHECK_CABAL_STAGE2 = $(warning WARNING: we haven't yet added Makefile flags for stage 2 builds with cabal; this is yet to be implemented, and the build will probably fail.  Consider BUILD_SYS_TOOLS=0 until then.)
else
_CHECK_CABAL_STAGE2 =
endif
else
_CHECK_CABAL_STAGE2 =
endif

# Use this recipe once ‘ahc’ can compile.
$(BUILD_DIR)/ahc-minimal-stage2_wip: $(BUILD_DIR)/ahc-minimal-stage1
	$(_CHECK_CABAL_STAGE2)

	install -d -m 0775 -- "$(BUILD_DIR)/stage2-builddir"
	$(MAKE) BUILD_STAGE2=0 BUILD_DIR="$(BUILD_DIR)/stage2-builddir" \
		HC_STAGE0="$(HC_STAGE1)" HC_STAGE0_FLAGS="$(HC_STAGE1_FLAGS)" HC_STAGE0_LINK="$(HC_STAGE1_LINK)" HC_STAGE0_LINK_FLAGS="$(HC_STAGE1_LINK_FLAGS)" \
 \
		HC_STAGE0_OUT_PREFIX_FLAGS="$(HC_STAGE1_OUT_PREFIX_FLAGS)" HC_STAGE0_OUT_SUFFIX_FLAGS="$(HC_STAGE1_OUT_SUFFIX_FLAGS)" \
		HC_STAGE0_IN_PREFIX_FLAGS="$(HC_STAGE1_IN_PREFIX_FLAGS)" HC_STAGE0_IN_SUFFIX_FLAGS="$(HC_STAGE1_IN_SUFFIX_FLAGS)" \
		HC_STAGE0_DEPIN_PREFIX_FLAGS="$(HC_STAGE1_DEPIN_PREFIX_FLAGS)" HC_STAGE0_DEPIN_SUFFIX_FLAGS="$(HC_STAGE1_DEPIN_SUFFIX_FLAGS)" \
		HC_STAGE0_LINK_OUT_PREFIX_FLAGS="$(HC_STAGE1_LINK_OUT_PREFIX_FLAGS)" HC_STAGE0_LINK_OUT_SUFFIX_FLAGS="$(HC_STAGE1_LINK_OUT_SUFFIX_FLAGS)" \
		HC_STAGE0_LINK_IN_PREFIX_FLAGS="$(HC_STAGE1_LINK_IN_PREFIX_FLAGS)" HC_STAGE0_LINK_IN_SUFFIX_FLAGS="$(HC_STAGE1_LINK_IN_SUFFIX_FLAGS)" \
		HC_STAGE0_LINK_DEPIN_PREFIX_FLAGS="$(HC_STAGE1_LINK_DEPIN_PREFIX_FLAGS)" HC_STAGE0_LINK_DEPIN_SUFFIX_FLAGS="$(HC_STAGE1_LINK_DEPIN_SUFFIX_FLAGS)" \
		HC_STAGE0_LINK_DEPINS_HIDE="$(HC_STAGE1_LINK_DEPINS_HIDE)" \
 \
		CABAL_COMMON_FLAGS_STAGE="$(CABAL_COMMON_FLAGS_STAGE_STAGE2)" \
		CABAL_COMMON_SUB_FLAGS_STAGE="$(CABAL_COMMON_SUB_FLAGS_STAGE_STAGE2)" \
		CABAL_CONFIGURE_STAGE_FLAGS="$(CABAL_CONFIGURE_STAGE_FLAGS_STAGE2)" \
		CABAL_COMPILE_STAGE_FLAGS="$(CABAL_COMPILE_STAGE_FLAGS_STAGE2)" \
		CABAL_LIBINSTALL_STAGE_FLAGS="$(CABAL_LIBINSTALL_STAGE_FLAGS_STAGE2)" \
		CABAL_INSTALL_STAGE_FLAGS="$(CABAL_INSTALL_STAGE_FLAGS_STAGE2)" \
 \
		$(EMPTY)

# Stage 1 build configuration.

HC_STAGE1            ?= $(DEFAULT_HC_STAGE1)
HC_STAGE1_FLAGS      ?= $(DEFAULT_HC_STAGE1_FLAGS)
HC_STAGE1_LINK       ?= $(DEFAULT_HC_STAGE1_LINK)
HC_STAGE1_LINK_FLAGS ?= $(DEFAULT_HC_STAGE1_LINK_FLAGS)

DEFAULT_HC_STAGE1            = $(BUILD_DIR)/ahc-minimal-stage1
DEFAULT_HC_STAGE1_FLAGS      = -c
DEFAULT_HC_STAGE1_LINK       = $(BUILD_DIR)/ahc-minimal-stage1
DEFAULT_HC_STAGE1_LINK_FLAGS =

HC_STAGE1_OUT_PREFIX_FLAGS   ?= $(DEFAULT_HC_STAGE1_OUT_PREFIX_FLAGS)
HC_STAGE1_OUT_SUFFIX_FLAGS   ?= $(DEFAULT_HC_STAGE1_OUT_SUFFIX_FLAGS)
HC_STAGE1_IN_PREFIX_FLAGS    ?= $(DEFAULT_HC_STAGE1_IN_PREFIX_FLAGS)
HC_STAGE1_IN_SUFFIX_FLAGS    ?= $(DEFAULT_HC_STAGE1_IN_SUFFIX_FLAGS)
HC_STAGE1_DEPIN_PREFIX_FLAGS ?= $(DEFAULT_HC_STAGE1_DEPIN_PREFIX_FLAGS)
HC_STAGE1_DEPIN_SUFFIX_FLAGS ?= $(DEFAULT_HC_STAGE1_DEPIN_SUFFIX_FLAGS)
HC_STAGE1_DEPINS_HIDE        ?= $(DEFAULT_HC_STAGE1_DEPINS_HIDE)

HC_STAGE1_LINK_OUT_PREFIX_FLAGS   ?= $(DEFAULT_HC_STAGE1_LINK_OUT_PREFIX_FLAGS)
HC_STAGE1_LINK_OUT_SUFFIX_FLAGS   ?= $(DEFAULT_HC_STAGE1_LINK_OUT_SUFFIX_FLAGS)
HC_STAGE1_LINK_IN_PREFIX_FLAGS    ?= $(DEFAULT_HC_STAGE1_LINK_IN_PREFIX_FLAGS)
HC_STAGE1_LINK_IN_SUFFIX_FLAGS    ?= $(DEFAULT_HC_STAGE1_LINK_IN_SUFFIX_FLAGS)
HC_STAGE1_LINK_DEPIN_PREFIX_FLAGS ?= $(DEFAULT_HC_STAGE1_LINK_DEPIN_PREFIX_FLAGS)
HC_STAGE1_LINK_DEPIN_SUFFIX_FLAGS ?= $(DEFAULT_HC_STAGE1_LINK_DEPIN_SUFFIX_FLAGS)
HC_STAGE1_LINK_DEPINS_HIDE        ?= $(DEFAULT_HC_STAGE1_LINK_DEPINS_HIDE)

DEFAULT_HC_STAGE1_OUT_PREFIX_FLAGS   = -o
DEFAULT_HC_STAGE1_OUT_SUFFIX_FLAGS   =
DEFAULT_HC_STAGE1_IN_PREFIX_FLAGS    =
DEFAULT_HC_STAGE1_IN_SUFFIX_FLAGS    =
DEFAULT_HC_STAGE1_DEPIN_PREFIX_FLAGS =
DEFAULT_HC_STAGE1_DEPIN_SUFFIX_FLAGS =
DEFAULT_HC_STAGE1_DEPINS_HIDE        = 1

DEFAULT_HC_STAGE1_LINK_OUT_PREFIX_FLAGS   = -o
DEFAULT_HC_STAGE1_LINK_OUT_SUFFIX_FLAGS   =
DEFAULT_HC_STAGE1_LINK_IN_PREFIX_FLAGS    =
DEFAULT_HC_STAGE1_LINK_IN_SUFFIX_FLAGS    =
DEFAULT_HC_STAGE1_LINK_DEPIN_PREFIX_FLAGS =
DEFAULT_HC_STAGE1_LINK_DEPIN_SUFFIX_FLAGS =
DEFAULT_HC_STAGE1_LINK_DEPINS_HIDE        = 1

CABAL_COMMON_FLAGS_STAGE_STAGE2     ?= $(DEFAULT_CABAL_COMMON_FLAGS_STAGE_STAGE2)
CABAL_COMMON_SUB_FLAGS_STAGE_STAGE2 ?= $(DEFAULT_CABAL_COMMON_SUB_FLAGS_STAGE_STAGE2)
CABAL_CONFIGURE_STAGE_FLAGS_STAGE2  ?= $(DEFAULT_CABAL_CONFIGURE_STAGE_FLAGS_STAGE2)
CABAL_COMPILE_STAGE_FLAGS_STAGE2    ?= $(DEFAULT_CABAL_COMPILE_STAGE_FLAGS_STAGE2)
CABAL_LIBINSTALL_STAGE_FLAGS_STAGE2 ?= $(DEFAULT_CABAL_LIBINSTALL_STAGE_FLAGS_STAGE2)
CABAL_INSTALL_STAGE_FLAGS_STAGE2    ?= $(DEFAULT_CABAL_INSTALL_STAGE_FLAGS_STAGE2)

DEFAULT_CABAL_COMMON_FLAGS_STAGE_STAGE2     = \
	$(EMPTY)

# TODO: need additional flags (or adaptation) to tell cabal-install about our new compiler.
DEFAULT_CABAL_COMMON_SUB_FLAGS_STAGE_STAGE2 = \
	$(EMPTY)

DEFAULT_CABAL_COMMON_SUB_FLAGS_STAGE_STAGE2_OPTIONAL = \
	-w $(BUILD_DIR)/ahc-minimal-stage1 \
	--with-hc-pkg=$(BUILD_DIR)/ahc-minimal-stage1 \
 \
	$(EMPTY)

DEFAULT_CABAL_CONFIGURE_STAGE_FLAGS_STAGE2  = \
	$(EMPTY)

DEFAULT_CABAL_COMPILE_STAGE_FLAGS_STAGE2    = \
	$(EMPTY)

DEFAULT_CABAL_LIBINSTALL_STAGE_FLAGS_STAGE2 = \
	$(EMPTY)

DEFAULT_CABAL_INSTALL_STAGE_FLAGS_STAGE2    = \
	$(EMPTY)
