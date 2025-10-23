#!/bin/bash
set -e

# Color definitions
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

echo -e "${GREEN}Running unit tests with perf profiling...${NC}"
echo -e "${BLUE}======================================${NC}"
time perf record -g -F 999 -o /tmp/zig-test.perf.data zig build test

echo ""
echo -e "${YELLOW}Processing perf data...${NC}"
perf script -F +pid -i /tmp/zig-test.perf.data > /tmp/zig-test.perf

echo ""
echo -e "${CYAN}Profile data saved to /tmp/zig-test.perf${NC}"
echo -e "${CYAN}View profile at: https://profiler.firefox.com/${NC}"
