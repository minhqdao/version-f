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

ifeq ($(FC),gfortran)
	MODIN := -I$(MODDIR)
	MODOUT := -J$(MODDIR)
else
	MODIN := -module $(MODDIR)
	MODOUT := MODIN
endif

SRCS := $(wildcard $(SRCDIR)/*.f90)
TESTSRCS := $(wildcard $(TESTDIR)/*.f90)
EXMPLSRCS := $(wildcard $(EXMPLDIR)/*.f90)
OBJS := $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SRCS))
TESTEXESSTATIC := $(patsubst $(TESTDIR)/%.f90,$(EXEDIR)/%_static.out,$(TESTSRCS))
TESTEXESSHARED := $(patsubst $(TESTDIR)/%.f90,$(EXEDIR)/%_shared.out,$(TESTSRCS))
EXMPLEXESSTATIC := $(patsubst $(EXMPLDIR)/%.f90,$(EXEDIR)/%_static.out,$(EXMPLSRCS))
EXMPLEXESSHARED := $(patsubst $(EXMPLDIR)/%.f90,$(EXEDIR)/%_shared.out,$(EXMPLSRCS))

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

$(EXEDIR)/%_static.out: $(TESTDIR)/%.f90 $(STATIC)
	mkdir -p $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^

$(EXEDIR)/%_static.out: $(EXMPLDIR)/%.f90 $(STATIC)
	mkdir -p $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^

$(EXEDIR)/%_shared.out: $(TESTDIR)/%.f90 $(SHARED)
	mkdir -p $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^ $(LDFLAGS)

$(EXEDIR)/%_shared.out: $(EXMPLDIR)/%.f90 $(SHARED)
	mkdir -p $(EXEDIR)
	$(FC) $(FFLAGS) $(MODIN) -o $@ $^ $(LDFLAGS)

test: $(TESTEXESSTATIC) $(TESTEXESSHARED) $(EXMPLEXESSTATIC) $(EXMPLEXESSHARED)
	@for f in $^; do $$f; done

clean:
	rm -rf $(BUILDDIR)
	rm -f $(STATIC)
	rm -f $(SHARED)
