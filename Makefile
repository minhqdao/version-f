.POSIX:
.SUFFIXES:
.PHONY: all static shared test clean

FC ?= gfortran
FFLAGS := -O2
AR := ar
ARFLAGS := rcs

ifeq ($(OS),Windows_NT)
	PLATFORM := Windows
	SHARED := libversion-f.dll
	LDFLAGS := -Wl,--export-all-symbols
else
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		PLATFORM := Linux
		SHARED := libversion-f.so
		LDFLAGS := -Wl,-rpath=.
	else ifeq ($(UNAME_S),Darwin)
		PLATFORM := MacOS
		SHARED := libversion-f.dylib
		LDFLAGS :=
	endif
endif

IS_GFORT  := $(findstring gfortran,$(FC))
IS_LFORTR := $(findstring lfortran,$(FC))
IS_FLANG  := $(findstring flang,$(FC))

ifneq (,$(IS_GFORT)$(IS_LFORTR))
	MODIN  := -I$(MODDIR)
	MODOUT := -J$(MODDIR)
else ifneq (,$(IS_FLANG))
	MODIN  := -I$(MODDIR)
	MODOUT := -module-dir $(MODDIR)
else
	MODIN  := -I$(MODDIR)
	MODOUT := -module $(MODDIR)
endif

BUILD_TARGETS := static
TEST_TARGETS  := 

ifneq (,$(IS_LFORTR))
	TEST_TARGETS += $(EXESSTATIC)
else
	BUILD_TARGETS += shared
	TEST_TARGETS  += $(EXESSTATIC) $(EXESSHARED)
endif

NAME := version-f
STATIC := lib$(NAME).a

SRCDIR := src
TESTDIR := test
EXMPLDIR := example
BUILDDIR := build/Makefile
MODDIR := $(BUILDDIR)/mod
OBJDIR := $(BUILDDIR)/obj
EXEDIR := $(BUILDDIR)/exe

SRCS := $(wildcard $(SRCDIR)/*.f90)
OBJS := $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SRCS))

EXESRCS := $(foreach dir,$(TESTDIR) $(EXMPLDIR),$(wildcard $(dir)/*.f90))

EXESSTATIC := $(patsubst %.f90,$(EXEDIR)/%_static.out,$(notdir $(EXESRCS)))
EXESSHARED := $(patsubst %.f90,$(EXEDIR)/%_shared.out,$(notdir $(EXESRCS)))

all: $(BUILD_TARGETS)
static: $(STATIC)
shared: $(SHARED)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	@mkdir -p $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) $(MODOUT) -c $< -o $@

$(STATIC): $(OBJS)
	$(AR) $(ARFLAGS) $@ $^

$(SHARED): $(OBJS)
	@mkdir -p $(MODDIR)
	$(FC) $(FFLAGS) -fpic -shared $(MODOUT) -o $@ $^ $(LDFLAGS)

$(EXEDIR):
	@mkdir -p $(EXEDIR)

$(EXEDIR)/%_static.out: $(TESTDIR)/%.f90 $(STATIC) | $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^

$(EXEDIR)/%_static.out: $(EXMPLDIR)/%.f90 $(STATIC) | $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^

$(EXEDIR)/%_shared.out: $(TESTDIR)/%.f90 $(SHARED) | $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^ $(LDFLAGS)

$(EXEDIR)/%_shared.out: $(EXMPLDIR)/%.f90 $(SHARED) | $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^ $(LDFLAGS)

test: $(TEST_TARGETS)
	@for f in $(TEST_TARGETS); do \
		echo "Running $$f..."; \
		./$$f || exit 1; \
	done
	@echo "All tests passed!"

clean:
	rm -rf $(BUILDDIR)
	rm -f $(STATIC) $(SHARED)
