FC = gfortran
FFLAGS = -O2
AR = ar
ARFLAGS = rcs

NAME = version-f
FILENAME = version_f
SRC = src/$(FILENAME).f90
TEST-SRC = test/test.f90
STATIC = lib$(NAME).a

BUILD-DIR = build/Makefile
MOD-DIR = $(BUILD-DIR)/mod
OBJ-DIR = $(BUILD-DIR)/obj
TEST-DIR = $(BUILD-DIR)/test

ifeq ($(FC),nvfortran)
	MOD-OUT-OPTION = -module $(MOD-DIR)
else
	MOD-OUT-OPTION = -J$(MOD-DIR)
endif

ifeq ($(FC),nvfortran)
	MOD-IN-OPTION = -module $(MOD-DIR)
else
	MOD-IN-OPTION = -I$(MOD-DIR)
endif

.PHONY: all test clean

all: $(STATIC)

$(STATIC): $(SRC)
		mkdir -p $(MOD-DIR) $(OBJ-DIR)
		$(FC) $(FFLAGS) -c $(SRC) $(MOD-OUT-OPTION) -o $(OBJ-DIR)/$(FILENAME).o
		$(AR) $(ARFLAGS) $(STATIC) $(OBJ-DIR)/$(FILENAME).o

test: $(TEST-SRC) $(STATIC)
		mkdir -p $(TEST-DIR)
		$(FC) $(FFLAGS) $(TEST-SRC) $(MOD-IN-OPTION) -o $(TEST-DIR)/test.out $(STATIC)
		$(TEST-DIR)/test.out

clean:
		rm -rf $(BUILD-DIR)
		rm -f $(STATIC)
