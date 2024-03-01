.POSIX:
.SUFFIXES:

FC = gfortran
FFLAGS = -O2
AR = ar
ARFLAGS = rcs

NAME = version-f
FILENAME = version_f
SRC = src/$(FILENAME).f90
TESTSRC = test/version_f_test.f90
STATIC = lib$(NAME).a

BUILDDIR = build/Makefile
MODDIR = $(BUILDDIR)/mod
OBJDIR = $(BUILDDIR)/obj
TESTDIR = $(BUILDDIR)/test

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

.PHONY: all test clean

all: $(STATIC)

$(STATIC): $(SRC)
		mkdir -p $(MODDIR) $(OBJDIR)
		$(FC) $(FFLAGS) -c $(SRC) $(MODOUT) -o $(OBJDIR)/$(FILENAME).o
		$(AR) $(ARFLAGS) $(STATIC) $(OBJDIR)/$(FILENAME).o

test: $(TESTSRC) $(STATIC)
		mkdir -p $(TESTDIR)
		$(FC) $(FFLAGS) $(TESTSRC) $(MODIN) -o $(TESTDIR)/test.out $(STATIC)
		$(TESTDIR)/test.out

clean:
		rm -rf $(BUILDDIR)
		rm -f $(STATIC)
