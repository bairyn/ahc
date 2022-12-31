# vim: ft=make noet

# An alternative way to build ‘ahc’'s programs.
#
# To run only a minimal build, e.g. one that requires only a Haskell2010 stage
# 0 implementation, run ‘make BUILD_MINIMAL=1’.
#
# By default, this Makefile bypasses ‘cabal’ and invokes only a stage 0
# Haskell2010 implementation directly (this can be ‘ghc’ or an ‘ahc’ compiler),
# but this Makefile can also build with ‘cabal-install’.

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
	@printf '%s\n' "Makefile for ‘ahc’."
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

# Build the ‘ahc’ programs.
# (Currently, just ‘ahc-core’'s ‘ahc-minimal’.  Probably more will be added
# later on.)
.PHONY: build-programs
build-programs: build-programs_$(BUILD_MINIMAL) build-ahc-minimal | build-prevalidate

# Additional ‘build-programs’ targets if this is not a minimal build.
# (Currently, this is empty, but probably more will be added later on.)
.PHONY: build-programs_0 build_programs_
build-programs_0 build-programs_:

# Additional ‘build-programs’ targets if this is a minimal build (none).
.PHONY: build-programs_1
build-programs_1:

# The ahc-minimal program.
$(BUILD_DIR)/ahc-minimal: $(BUILD_DIR)/ahc-core/ahc-minimal
	install -m 0775 -- "$<" "$@"

$(BUILD_DIR)/ahc-core/ahc-minimal: build-ahc-minimal | build-dirs

# Build ‘ahc-core‛s ‘ahc-minimal’.
.PHONY: build-ahc-minimal
build-ahc-minimal: build-ahc-core | build-dirs

# Configuration for building ahc-core.
_BUILD_DIR_ABS = $(shell realpath -- "$(BUILD_DIR)")

# Build ahc-core.
.PHONY: build-ahc-core
#build-ahc-core: | build-dirs $(_BUILD_DIR_ABS)/ahc-core
build-ahc-core: | build-dirs
	$(MAKE) -C ./packages/ahc-core/ "BUILD_DIR=$(_BUILD_DIR_ABS)/ahc-core"

# Let sub-make alone manage this directory.
#$(_BUILD_DIR_ABS)/ahc-core: | build-dirs
#	install -d -m 0775 -- "$@"

# Prevalidation.
.PHONY: build-prevalidate
build-prevalidate: build-prevalidate-version-consistency

# Configuration for ‘build-prevalidate’
DEBUG_SHOW_PREVALIDATOR_SCRIPTS ?= 0

# Configuration for ‘build-prevalidate-version-consistency’.
BASH ?= bash

# Use ‘bash -c’ or ‘echo -nE’?
_CMD_BUILD_PREV_SHOW   = $(_CMD_BUILD_PREV_SHOW_$(DEBUG_SHOW_PREVALIDATOR_SCRIPTS))
_CMD_BUILD_PREV_SHOW_0 = bash -c
_CMD_BUILD_PREV_SHOW_  = $(_CMD_BUILD_PREV_SHOW_0)
_CMD_BUILD_PREV_SHOW_1 = echo -nE

_PREVALIDATE_VERSION_CONSISTENCY_DEPS = \
	./CHANGELOG.md \
	./packages/ahc-core/CHANGELOG.md \
	./packages/ahc-core/ahc-core.cabal \
	./packages/ahc-core/src/Language/Haskell2010/Ahc/Meta/Ahc.hs \
 \
	$(EMPTY)

# Fail if the versions are inconsistent in the 4 locations in which they are
# found:
# 	- `CHANGELOG.md`
# 	- `packages/ahc-core/CHANGELOG.md`
# 	- `packages/ahc-core/ahc-core.cabal`
# 	- `packages/ahc-core/src/Language/Haskell2010/Ahc/Meta/Ahc.hs`
# TODO: timestamp, dependencies.
.PHONY: build-prevalidate-version-consistency
build-prevalidate-version-consistency: $(BUILD_DIR)/prevalidate-version-consistency.stamp
$(BUILD_DIR)/prevalidate-version-consistency.stamp: $(_PREVALIDATE_VERSION_CONSISTENCY_DEPS) | build-dirs
	@# Print our script, then pipe it to run it.
	@n=$$'\n' && printf -- '%s\n' " $$n\
		#!/usr/bin/env bash $$n\
		set -ueE -o pipefail $$n\
		trap 'echo \"An error (code \$$?) occurred on line \$${LINENO:-} from ‘\$${BASH_SOURCE[0]:-}’; aborting…\" 1>&2' ERR $$n\
 $$n\
		main() { $$n\
			# Pre-initialize with ‘local’ with empty values separately so the $$n\
			# exit code isn't disregarded. $$n\
			local version_changelog=\"\" $$n\
			local version_subchangelog=\"\" $$n\
			local version_package=\"\" $$n\
 $$n\
			local version_meta=\"\" $$n\
 $$n\
			local version_keys=\"\" $$n\
			local last=\"\" $$n\
 $$n\
			local version_path_version_changelog=\"\" $$n\
			local version_path_version_subchangelog=\"\" $$n\
			local version_path_version_package=\"\" $$n\
			local version_path_version_meta=\"\" $$n\
 $$n\
			local version_extractor_version_changelog=\"\" $$n\
			local version_extractor_version_subchangelog=\"\" $$n\
			local version_extractor_version_package=\"\" $$n\
			local version_extractor_version_meta=\"\" $$n\
 $$n\
			local util_sed_n_independent=\"\" $$n\
 $$n\
			util_sed_n_independent=\"; d; b\" $$n\
 $$n\
			# Word-array of version variable names. $$n\
			version_keys=\" \\$$n\
				version_changelog \\$$n\
				version_subchangelog \\$$n\
				version_package \\$$n\
				version_meta \\$$n\
			\" $$n\
			# Setup paths. $$n\
			version_path_version_changelog=\"./CHANGELOG.md\" $$n\
			version_path_version_subchangelog=\"./packages/ahc-core/CHANGELOG.md\" $$n\
			version_path_version_package=\"./packages/ahc-core/ahc-core.cabal\" $$n\
			version_path_version_meta=\"./packages/ahc-core/src/Language/Haskell2010/Ahc/Meta/Ahc.hs\" $$n\
			# Setup version tag extractors. $$n\
			version_extractor_version_changelog='/^( ? ? ?\#\#\s*)([^[:space:]]+)(\s*.*)\$$/ { s//\2/g; p; q; }'\"\$$util_sed_n_independent\" $$n\
			version_extractor_version_subchangelog='/^( ? ? ?\#\#\s*)([^[:space:]]+)(\s*.*)\$$/ { s//\2/g; p; q; }'\"\$$util_sed_n_independent\" $$n\
			version_extractor_version_package='/^(\s*version\s*:\s*)([^[:space:]]+)(\s*)\$$/ { s//\2/g; p; q; }'\"\$$util_sed_n_independent\" $$n\
			version_extractor_version_meta='/^(\s*appVersionStr\s*=\s*\")([^\"]+)(\"\s*)\$$/ { s//\2/g; p; q; }'\"\$$util_sed_n_independent\" $$n\
			# Get first ‘## ’-level word from ‘CHANGELOG.md’. $$n\
			version_changelog=\"\$$(sed -nEe \"\$$version_extractor_version_changelog\" < \"\$$version_path_version_changelog\")\" $$n\
			# Get first ‘## ’-level word from ‘packages/ahc-core/CHANGELOG.md’. $$n\
			version_subchangelog=\"\$$(sed -nEe \"\$$version_extractor_version_subchangelog\" < \"\$$version_path_version_subchangelog\")\" $$n\
			# Get ‘version: ’ value from ‘packages/ahc-core/ahc-core.cabal’. $$n\
			version_package=\"\$$(sed -nEe \"\$$version_extractor_version_package\" < \"\$$version_path_version_package\")\" $$n\
			# Get ‘appVersionStr = \"…\"’ string literal contents from ‘packages/ahc-core/src/Language/Haskell2010/Ahc/Meta/Ahc.hs’. $$n\
			version_meta=\"\$$(sed -nEe \"\$$version_extractor_version_meta\" < \"\$$version_path_version_meta\")\" $$n\
			# Make sure we could find the versions. $$n\
			for version_key in \$$version_keys; do $$n\
				local version_value=\"\" $$n\
				local version_path=\"\" $$n\
				local version_extractor=\"\" $$n\
				eval 'version_value=\$$'\"\$$version_key\"'' $$n\
				eval 'version_path=\$$version_path_'\"\$$version_key\"'' $$n\
$$n\
				eval 'version_extractor=\$$version_extractor_'\"\$$version_key\"'' $$n\
				if [[ -z \"\$$version_value\" ]]; then $$n\
					printf -- '%s\\n' \"Error: build-prevalidate-version-consistency: internal error: we failed to extract the version tag from ‘\$$version_key’; we got an empty string.\" 1>&2 $$n\
					printf -- '%s\\n' \"	Try ensuring that the extraction script is up-to-date with this file, and that the file is formatted correctly.\" 1>&2 $$n\
					printf -- '%s\\n' \"\" 1>&2 $$n\
					printf -- '%s\\n' \"	version_key       : \$$version_key\" 1>&2 $$n\
					printf -- '%s\\n' \"	version_value     : \$$version_value\" 1>&2 $$n\
					printf -- '%s\\n' \"	version_path      : \$$version_path\" 1>&2 $$n\
					printf -- '%s\\n' \"	version_extractor : \$$version_extractor\" 1>&2 $$n\
					# Skip the default error printer, since we already $$n\
					# printed one. $$n\
					exec false $$n\
				fi $$n\
			done $$n\
			# Require consistency. $$n\
			for version_key in \$$version_keys; do $$n\
				# First version variable?  Special case for this. $$n\
				if [[ -z \"\$$last\" ]]; then $$n\
					last=\"\$$version_key\" $$n\
				else $$n\
					local previous_version_key=\"\" $$n\
					local previous_version_value=\"\" $$n\
					local previous_version_path=\"\" $$n\
					local previous_version_extractor=\"\" $$n\
					local this_version_key=\"\" $$n\
					local this_version_value=\"\" $$n\
					local this_version_path=\"\" $$n\
					local this_version_extractor=\"\" $$n\
 $$n\
					previous_version_key=\"\$$last\" $$n\
					this_version_key=\"\$$version_key\" $$n\
 $$n\
					eval 'previous_version_value=\$$'\"\$$previous_version_key\"'' $$n\
					eval 'this_version_value=\$$'\"\$$this_version_key\"'' $$n\
 $$n\
					eval 'previous_version_path=\$$version_path_'\"\$$previous_version_key\"'' $$n\
					eval 'this_version_path=\$$version_path_'\"\$$this_version_key\"'' $$n\
 $$n\
					eval 'previous_version_extractor=\$$version_extractor_'\"\$$previous_version_key\"'' $$n\
					eval 'this_version_extractor=\$$version_extractor_'\"\$$this_version_key\"'' $$n\
 $$n\
					if [[ \"x\$$previous_version_value\" != \"x\$$this_version_value\" ]]; then $$n\
						# We found an inconsistency.  Fail. $$n\
						printf -- '%s\\n' \"Error: build-prevalidate-versions-consistency: found inconsistent versions in the files.\" 1>&2 $$n\
						printf -- '%s\\n' \"	We found different version tags in the files.  Please ensure they are consistent,\" 1>&2 $$n\
						printf -- '%s\\n' \"	if this validator is correctly detecting a discrepancy.\" 1>&2 $$n\
						printf -- '%s\\n' \"\" 1>&2 $$n\
						printf -- '%s\\n' \"	previous_version_key       : \$$previous_version_key\" 1>&2 $$n\
						printf -- '%s\\n' \"	this_version_key           : \$$this_version_key\" 1>&2 $$n\
						printf -- '%s\\n' \"	previous_version_value     : \$$previous_version_value\" 1>&2 $$n\
						printf -- '%s\\n' \"	this_version_value         : \$$this_version_value\" 1>&2 $$n\
						printf -- '%s\\n' \"	previous_version_path      : \$$previous_version_path\" 1>&2 $$n\
						printf -- '%s\\n' \"	this_version_path          : \$$this_version_path\" 1>&2 $$n\
						printf -- '%s\\n' \"	previous_version_extractor : \$$previous_version_extractor\" 1>&2 $$n\
						printf -- '%s\\n' \"	this_version_extractor     : \$$this_version_extractor\" 1>&2 $$n\
						# Skip the default error printer, since we already $$n\
						# printed one. $$n\
						exec false $$n\
					fi $$n\
				fi $$n\
			done $$n\
 $$n\
			: \"Passed version consistency check.  (No output.)\" $$n\
		} $$n$$n\
 $$n\
		main \"\$$@\" $$n\
	" | sed -nEe 's@^\t@@g; 1b; $$b; p' | xargs --null -- $(_CMD_BUILD_PREV_SHOW)

	touch -- "$@"
