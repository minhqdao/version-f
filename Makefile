.POSIX:
.SUFFIXES:

FC = gfortran
FFLAGS = -O2
AR = ar
ARFLAGS = rcs

NAME = version-f
FILENAME = version_f
SRC = src/$(FILENAME).f90
TEST-SRC = test/version_f_test.f90
STATIC = lib$(NAME).a

BUILD-DIR = build/Makefile
MOD-DIR = $(BUILD-DIR)/mod
OBJ-DIR = $(BUILD-DIR)/obj
TEST-DIR = $(BUILD-DIR)/test

ifeq ($(FC),nvfortran)
	MOD-OUTPUT = -module $(MOD-DIR)
else
	MOD-OUTPUT = -J$(MOD-DIR)
endif

ifeq ($(FC),nvfortran)
	MOD-INPUT = -module $(MOD-DIR)
else
	MOD-INPUT = -I$(MOD-DIR)
endif

.PHONY: all test clean

all: $(STATIC)

$(STATIC): $(SRC)
		mkdir -p $(MOD-DIR) $(OBJ-DIR)
		$(FC) $(FFLAGS) -c $(SRC) $(MOD-OUTPUT) -o $(OBJ-DIR)/$(FILENAME).o
		$(AR) $(ARFLAGS) $(STATIC) $(OBJ-DIR)/$(FILENAME).o

test: $(TEST-SRC) $(STATIC)
		mkdir -p $(TEST-DIR)
		$(FC) $(FFLAGS) $(TEST-SRC) $(MOD-INPUT) -o $(TEST-DIR)/test.out $(STATIC)
		$(TEST-DIR)/test.out

clean:
		rm -rf $(BUILD-DIR)
		rm -f $(STATIC)
