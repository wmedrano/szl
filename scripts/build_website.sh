#!/bin/bash
# Build the szl website from README.md

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DOCS_DIR="$PROJECT_ROOT/zig-out/site"

# Create docs directory if it doesn't exist
mkdir -p "$DOCS_DIR"

# Convert README.md to HTML with pandoc
pandoc "$PROJECT_ROOT/README.md" \
    --from markdown \
    --to html5 \
    --standalone \
    --css style.css \
    --metadata title="Sizzle (szl) - A Scheme Interpreter in Zig" \
    --output "$DOCS_DIR/index.html"

# Copy CSS file to docs
cp "$SCRIPT_DIR/style.css" "$DOCS_DIR/style.css"

echo "✓ Website built successfully at $DOCS_DIR/index.html"
