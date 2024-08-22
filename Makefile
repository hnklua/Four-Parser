# Makefile for Haskell project

# Compiler options
GHC = ghc
GHCFLAGS = -Wall

# Executable name
EXEC = forth_parser

# Source files
SRCS = forthparser.hs

# Default target
all: $(EXEC)

# Compile Haskell source files into executable
$(EXEC): $(SRCS)
	$(GHC) $(GHCFLAGS) -o $(EXEC) $(SRCS)

# Clean up object files and executable
clean:
	rm -f $(EXEC) *.o
