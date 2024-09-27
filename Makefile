# Variables
GHC = ghc
SRC_DIR = src
MAIN_MODULE = Main
OUT_DIR = bin
EXECUTABLE = $(OUT_DIR)/sat-solver

# Compiler options
GHC_FLAGS = -Wall -O2

# Find all Haskell source files in the src/ directory
SOURCES = $(wildcard $(SRC_DIR)/*.hs)

# Default target
all: $(EXECUTABLE)

# Build the executable
$(EXECUTABLE): $(SOURCES)
	@mkdir -p $(OUT_DIR)
	$(GHC) $(GHC_FLAGS) -o $(EXECUTABLE) -i$(SRC_DIR) $(SRC_DIR)/$(MAIN_MODULE).hs

# Clean up the generated files
clean:
	rm -rf $(OUT_DIR) */*.hi */*.o

clean-outputs:
	rm -rf output/ 2> /dev/null

.PHONY: all clean clean-outputs
