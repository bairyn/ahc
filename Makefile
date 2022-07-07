.PHONY: default
default: all

.PHONY: all
all: _build

.PHONY: all-more
all-more: _build dist all

.PHONY: clean
clean:
	rm -rf -- "./build"

.PHONY: _build
_build: executable

AS ?= as
AS_FLAGS ?= $(BASE_AS_FLAGS) $(PROJECT_AS_FLAGS) $(EXTRA_AS_FLAGS)

PROJECT_AS_FLAGS = \
	-nostdlib \
	#

BASE_AS_FLAGS ?=
EXTRA_AS_FLAGS ?=

LD ?= ld

LD_FLAGS ?= $(BASE_LD_FLAGS) $(PROJECT_LD_FLAGS) $(EXTRA_LD_FLAGS)

# --omagic: support self-modifying code via writeable text sections.
PROJECT_LD_FLAGS = \
	-e _ns_main_entry \
	--omagic \
	#

BASE_LD_FLAGS ?=
EXTRA_LD_FLAGS ?=

OBJ_DIR_BASE = obj
OBJ_DIR = $(BUILD_DIR)/$(OBJ_DIR_BASE)

BUILD_DIR = ./build
DIST_DIR_BASE = dist
DIST_DIR = $(BUILD_DIR)/$(DIST_DIR_BASE)
BUILD_DIRS = \
	$(BUILD_DIR) \
	$(DIST_DIR) \
	$(OBJ_DIR) \
	#

EXECUTABLE = ahc

SRC_DIR = ./src
SYSTEM = x86_64-linux
SRC_SYSTEM_DIR = $(SRC_DIR)/$(SYSTEM)

SRC_FILES = \
	cli.s \
	main.s \
	system.s \
	util.s \
	fun_ahc.s \
	#

OBJS ?= \
	$(OBJ_DIR)/cli.o \
	$(OBJ_DIR)/main.o \
	$(OBJ_DIR)/system.o \
	$(OBJ_DIR)/util.o \
	$(OBJ_DIR)/fun_ahc.o \
	#

PREFIX ?= /usr
BASE_BINDIR ?= /bin
BINDIR ?= $(PREFIX)$(BASE_BINDIR)

.PHONY: dist
dist: build | $(DIST_DIR)
	install -d -m 0775 -- "$(DIST_DIR)"

	install -d -m 0775 -- "$(DIST_DIR)$(BINDIR)"
	install -m 0775 -- "$(BUILD_DIR)/$(EXECUTABLE)" "$(DIST_DIR)$(BINDIR)$(EXECUTABLE)"

.PHONY: build-dirs
build-dirs: $(BUILD_DIRS)

$(filter-out $(DIST_DIR) $(OBJ_DIR),$(BUILD_DIRS)):
	install -d -m 0775 -- "$@"
$(DIST_DIR) $(OBJ_DIR): | $(BUILD_DIR)
	install -d -m 0775 -- "$@"

.PHONY: executable
executable: $(BUILD_DIR)/$(EXECUTABLE) | $(BUILD_DIR)

# TODO (external): fix binutil's as and ld's handling of ‘--’.
$(OBJS): $(OBJ_DIR)/%.o: $(SRC_SYSTEM_DIR)/%.s | $(BUILD_DIRS)
	printf '%s\n' "WARNING: gas seems broken with ‘--’.  Bug?"
	#$(AS) $(AS_FLAGS) -o "$@" -- $^
	$(AS) $(AS_FLAGS) -o "$@" $^

$(BUILD_DIR)/$(EXECUTABLE): $(OBJS)
	#$(LD) $(LD_FLAGS) -o "$@" -- $^
	$(LD) $(LD_FLAGS) -o "$@" $^
