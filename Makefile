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
else ifeq ($(shell uname), Darwin)
    SHARED := lib$(NAME).dylib
endif

SRCDIR := src
TESTDIR := test
EXMPLDIR := example
BUILDDIR := build/Makefile
MODDIR := $(BUILDDIR)/mod
OBJDIR := $(BUILDDIR)/obj
EXEDIRSTATIC := $(BUILDDIR)/exe/static
EXEDIRSHARED := $(BUILDDIR)/exe/shared

ifeq ($(FC),gfortran)
	MODOUT := -J$(MODDIR)
else
	MODOUT := -module $(MODDIR)
endif

ifeq ($(FC),gfortran)
	MODIN := -I$(MODDIR)
else
	MODIN := -module $(MODDIR)
endif

SRCS := $(wildcard $(SRCDIR)/*.f90)
TESTSRCS := $(wildcard $(TESTDIR)/*.f90)
EXMPLSRCS := $(wildcard $(EXMPLDIR)/*.f90)
OBJS := $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SRCS))
TESTEXESSTATIC := $(patsubst $(TESTDIR)/%.f90,$(EXEDIRSTATIC)/%.out,$(TESTSRCS))
TESTEXESSHARED := $(patsubst $(TESTDIR)/%.f90,$(EXEDIRSHARED)/%.out,$(TESTSRCS))
EXMPLEXESSTATIC := $(patsubst $(EXMPLDIR)/%.f90,$(EXEDIRSTATIC)/%.out,$(EXMPLSRCS))
EXMPLEXESSHARED := $(patsubst $(EXMPLDIR)/%.f90,$(EXEDIRSHARED)/%.out,$(EXMPLSRCS))

all: $(STATIC) $(SHARED)
static: $(STATIC)
shared: $(SHARED)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
	mkdir -p $(MODDIR) $(OBJDIR)
	$(FC) $(FFLAGS) $(MODOUT) -c $< -o $@

$(STATIC): $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)

$(SHARED): $(SRCS)
	mkdir -p $(MODDIR)
	$(FC) $(FFLAGS) -fpic -shared $(MODOUT) -o $@ $<

$(EXEDIRSTATIC)/%.out: $(TESTDIR)/%.f90 $(STATIC)
	mkdir -p $(EXEDIRSTATIC)
	$(FC) $(FFLAGS) $< $(MODIN) -o $@ $(STATIC)

$(EXEDIRSTATIC)/%.out: $(EXMPLDIR)/%.f90 $(STATIC)
	mkdir -p $(EXEDIRSTATIC)
	$(FC) $(FFLAGS) $< $(MODIN) -o $@ $(STATIC)

$(EXEDIRSHARED)/%.out: $(TESTDIR)/%.f90 $(SHARED)
	mkdir -p $(EXEDIRSHARED)
	$(FC) $(FFLAGS) $< $(MODIN) -o $@ $(SHARED)

$(EXEDIRSHARED)/%.out: $(EXMPLDIR)/%.f90 $(SHARED)
	mkdir -p $(EXEDIRSHARED)
	$(FC) $(FFLAGS) $< $(MODIN) -o $@ $(SHARED)

test: $(TESTEXESSTATIC) $(TESTEXESSHARED) $(EXMPLEXESSTATIC) $(EXMPLEXESSHARED)
	@for f in $^; do $$f; done

clean:
	rm -rf $(BUILDDIR)
	rm -f $(STATIC)
	rm -f $(SHARED)
