#!/bin/bash
set -e

# Color definitions
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${GREEN}Building with ReleaseFast optimization...${NC}"
# We use v3 since v4 makes use of avx512. Valgrind does not yet have support for
# avx512.
zig build -Doptimize=ReleaseSafe -Dcpu=x86_64_v3

echo ""
echo -e "${BLUE}Running benchmark: examples/fib.scm${NC}"
echo -e "${BLUE}======================================${NC}"
valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes --cache-sim=yes --callgrind-out-file=/tmp/fib.callgrind zig-out/bin/szl <examples/fib.scm
echo ""
echo -e "${CYAN}Profile data saved to /tmp/fib.callgrind${NC}"
echo -e "${CYAN}View with: kcachegrind /tmp/fib.callgrind${NC}"
