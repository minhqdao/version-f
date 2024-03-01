.POSIX:
.SUFFIXES:
.PHONY: all test clean

FC = gfortran
FFLAGS = -O2
AR = ar
ARFLAGS = rcs

NAME = version-f
STATIC = lib$(NAME).a

SRCDIR = src
TESTDIR = test
EXMPLDIR = example
BUILDDIR = build/Makefile
MODDIR = $(BUILDDIR)/mod
OBJDIR = $(BUILDDIR)/obj
EXEDIR = $(BUILDDIR)/exe

SRCS = $(wildcard $(SRCDIR)/*.f90)
TESTSRCS = $(wildcard $(TESTDIR)/*.f90)
EXMPLSRCS = $(wildcard $(EXMPLDIR)/*.f90)
OBJS = $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SRCS))
TESTEXES = $(patsubst $(TESTDIR)/%.f90,$(EXEDIR)/%.out,$(TESTSRCS))
EXMPLEXES = $(patsubst $(EXMPLDIR)/%.f90,$(EXEDIR)/%.out,$(EXMPLSRCS))

ifeq ($(FC),nvfortran)
	MODOUT = -module $(MODDIR)
else
	MODOUT = -J$(MODDIR)
endif

ifeq ($(FC),nvfortran)
	MODIN = -module $(MODDIR)
else
	MODIN = -I$(MODDIR)
endif

all: $(STATIC)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
		mkdir -p $(MODDIR) $(OBJDIR)
		$(FC) $(FFLAGS) $(MODOUT) -c $< -o $@

$(STATIC): $(OBJS)
		$(AR) $(ARFLAGS) $@ $(OBJS)

$(EXEDIR)/%.out: $(TESTDIR)/%.f90 $(STATIC)
		mkdir -p $(EXEDIR)
		$(FC) $(FFLAGS) $< $(MODIN) -o $@ $(STATIC)

$(EXEDIR)/%.out: $(EXMPLDIR)/%.f90 $(STATIC)
		mkdir -p $(EXEDIR)
		$(FC) $(FFLAGS) $< $(MODIN) -o $@ $(STATIC)

test: $(TESTEXES) $(EXMPLEXES)
		@for f in $(TESTEXES) $(EXMPLEXES); do $$f; done

clean:
		rm -rf $(BUILDDIR)
		rm -f $(STATIC)
