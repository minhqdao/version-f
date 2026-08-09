.POSIX:
.SUFFIXES:
.PHONY: all static shared examples test clean

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
	MODIN  = -I$(MODDIR)
	MODOUT = -J$(MODDIR)
else ifneq (,$(IS_FLANG))
	MODIN  = -I$(MODDIR)
	MODOUT = -module-dir $(MODDIR)
else
	MODIN  = -I$(MODDIR)
	MODOUT = -module $(MODDIR)
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

TESTSRCS := $(wildcard $(TESTDIR)/*.f90)
EXMPLSRCS := $(wildcard $(EXMPLDIR)/*.f90)

TESTEXESSTATIC := $(patsubst %.f90,$(EXEDIR)/%_static.out,$(notdir $(TESTSRCS)))
TESTEXESSHARED := $(patsubst %.f90,$(EXEDIR)/%_shared.out,$(notdir $(TESTSRCS)))
EXMPLEXESSTATIC := $(patsubst %.f90,$(EXEDIR)/%_static.out,$(notdir $(EXMPLSRCS)))
EXMPLEXESSHARED := $(patsubst %.f90,$(EXEDIR)/%_shared.out,$(notdir $(EXMPLSRCS)))

BUILD_TARGETS := static
TEST_TARGETS := $(TESTEXESSTATIC)
EXAMPLE_TARGETS := $(EXMPLEXESSTATIC)

ifeq (,$(IS_LFORTR))
	BUILD_TARGETS += shared
	TEST_TARGETS += $(TESTEXESSHARED)
	EXAMPLE_TARGETS += $(EXMPLEXESSHARED)
endif

all: $(BUILD_TARGETS)
static: $(STATIC)
shared: $(SHARED)
examples: $(EXAMPLE_TARGETS)

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
	@test_count=0; \
	for f in $(TEST_TARGETS); do \
		echo "Running $$f..."; \
		./$$f || exit 1; \
		test_count=$$((test_count + 1)); \
	done; \
	if [ "$$test_count" -eq 0 ]; then \
		echo "Error: no tests were run."; \
		exit 1; \
	fi; \
	echo "All $$test_count tests passed!"

clean:
	rm -rf $(BUILDDIR)
	rm -f $(STATIC) $(SHARED)
