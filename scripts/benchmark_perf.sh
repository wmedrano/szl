#!/bin/bash
set -e

# Color definitions
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${GREEN}Building with ReleaseFast optimization...${NC}"
zig build -Doptimize=ReleaseSafe

echo ""
echo -e "${BLUE}Running benchmark: examples/fib.scm${NC}"
echo -e "${BLUE}======================================${NC}"
time perf record -g -F 999 -o /tmp/fib.perf.data zig-out/bin/szl <examples/fib.scm
perf script -F +pid -i /tmp/fib.perf.data > /tmp/fib.perf
echo ""
echo -e "${CYAN}Profile data saved to /tmp/fib.perf${NC}"
echo -e "${CYAN}View profile at: https://profiler.firefox.com/${NC}"
