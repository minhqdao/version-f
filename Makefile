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

.PHONY: all test clean

all: $(STATIC)

$(STATIC): $(SRC)
		mkdir -p $(MOD-DIR) $(OBJ-DIR)
		$(FC) $(FFLAGS) -c $(SRC) -J$(MOD-DIR) -o $(OBJ-DIR)/$(FILENAME).o
		$(AR) $(ARFLAGS) $(STATIC) $(OBJ-DIR)/$(FILENAME).o

test: $(TEST-SRC) $(STATIC)
		mkdir -p $(TEST-DIR)
		$(FC) $(FFLAGS) $(TEST-SRC) -I$(MOD-DIR) -o $(TEST-DIR)/test.out $(STATIC)
		$(TEST-DIR)/test.out

clean:
		rm -rf $(BUILD-DIR)
		rm -f $(STATIC)
