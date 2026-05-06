.POSIX:
.SUFFIXES:
.PHONY: all static shared test clean

FC := gfortran
FFLAGS := -O2
AR := ar
ARFLAGS := rcs

NAME := version-f
STATIC := lib$(NAME).a

ifeq ($(shell uname), Linux)
	SHARED := lib$(NAME).so
	LDFLAGS := -Wl,-rpath=.
else ifeq ($(shell uname), Darwin)
	SHARED := lib$(NAME).dylib
endif

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

ifeq ($(FC),gfortran)
	MODIN := -I$(MODDIR)
	MODOUT := -J$(MODDIR)
else
	MODIN := -module $(MODDIR)
	MODOUT := $(MODIN)
endif

all: $(STATIC) $(SHARED)
static: $(STATIC)
shared: $(SHARED)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	mkdir -p $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) $(MODOUT) -c $< -o $@

$(STATIC): $(OBJS)
	$(AR) $(ARFLAGS) $@ $<

$(SHARED): $(SRCS)
	mkdir -p $(MODDIR)
	$(FC) $(FFLAGS) -fpic -shared $(MODOUT) -o $@ $<

$(EXEDIR):
	mkdir -p $(EXEDIR)

$(EXEDIR)/%_static.out: $(TESTDIR)/%.f90 $(STATIC) | $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^

$(EXEDIR)/%_static.out: $(EXMPLDIR)/%.f90 $(STATIC) | $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^

$(EXEDIR)/%_shared.out: $(TESTDIR)/%.f90 $(SHARED) | $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^ $(LDFLAGS)

$(EXEDIR)/%_shared.out: $(EXMPLDIR)/%.f90 $(SHARED) | $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^ $(LDFLAGS)

test: $(EXESSTATIC) $(EXESSHARED)
	@for f in $^; do $$f; done

clean:
	rm -rf $(BUILDDIR)
	rm -f $(STATIC)
	rm -f $(SHARED)
