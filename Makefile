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
BUILDDIR = build/Makefile
MODDIR = $(BUILDDIR)/mod
OBJDIR = $(BUILDDIR)/obj
EXEDIR = $(BUILDDIR)/exe

SRCS := $(wildcard $(SRCDIR)/*.f90)
TESTSRCS := $(wildcard $(TESTDIR)/*.f90)
OBJS := $(patsubst $(SRCDIR)/%.f90,$(OBJDIR)/%.o,$(SRCS))
TESTOBJS := $(patsubst $(TESTDIR)/%.f90,$(EXEDIR)/%.out,$(TESTSRCS))

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

test: $(TESTOBJS)
		$(TESTOBJS)

clean:
		rm -rf $(BUILDDIR)
		rm -f $(STATIC)
