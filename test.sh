#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$ROOT/build/tests"
MOD_DIR="$BUILD_DIR/mod"
BIN="$BUILD_DIR/test_math_pipeline"

mkdir -p "$BUILD_DIR" "$MOD_DIR"

FFLAGS="${FFLAGS:-"-O2 -g -Wall -Wextra -std=f2018 -ffree-line-length-none"}"

gfortran $FFLAGS -J "$MOD_DIR" -I "$MOD_DIR" -c "$ROOT/src/vector_math.f90" -o "$BUILD_DIR/vector_math.o"
gfortran $FFLAGS -J "$MOD_DIR" -I "$MOD_DIR" -c "$ROOT/src/model_library.f90" -o "$BUILD_DIR/model_library.o"
gfortran $FFLAGS -J "$MOD_DIR" -I "$MOD_DIR" -c "$ROOT/test/test_math_pipeline.f90" -o "$BUILD_DIR/test_math_pipeline.o"
gfortran "$BUILD_DIR/vector_math.o" "$BUILD_DIR/model_library.o" "$BUILD_DIR/test_math_pipeline.o" -o "$BIN"

"$BIN"
