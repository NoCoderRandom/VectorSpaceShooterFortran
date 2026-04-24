#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="$ROOT/build"
OBJ_DIR="$BUILD_DIR/obj"
MOD_DIR="$BUILD_DIR/mod"
BIN_DIR="$BUILD_DIR/bin"
BIN="$BIN_DIR/vector_space_shooter"

if ! pkg-config --exists sdl2; then
    echo "SDL2 development files are missing."
    echo "Install: sudo apt-get update && sudo apt-get install -y build-essential gfortran pkg-config libsdl2-dev"
    exit 1
fi

mkdir -p "$OBJ_DIR" "$MOD_DIR" "$BIN_DIR"

SDL_CFLAGS="$(pkg-config --cflags sdl2)"
SDL_LIBS="$(pkg-config --libs sdl2)"
FFLAGS="${FFLAGS:-"-O2 -g -Wall -Wextra -std=f2018 -ffree-line-length-none"}"
CFLAGS="${CFLAGS:-"-O2 -g -Wall -Wextra -std=c11"}"

gcc $CFLAGS $SDL_CFLAGS -c "$ROOT/src/sdl_platform.c" -o "$OBJ_DIR/sdl_platform.o"

FORTRAN_SOURCES=(
    "$ROOT/src/vector_math.f90"
    "$ROOT/src/platform.f90"
    "$ROOT/src/model_library.f90"
    "$ROOT/src/vector_renderer.f90"
    "$ROOT/src/vector_font.f90"
    "$ROOT/src/game.f90"
    "$ROOT/app/main.f90"
)

OBJECTS=("$OBJ_DIR/sdl_platform.o")
for src in "${FORTRAN_SOURCES[@]}"; do
    obj="$OBJ_DIR/$(basename "${src%.f90}").o"
    gfortran $FFLAGS -J "$MOD_DIR" -I "$MOD_DIR" -c "$src" -o "$obj"
    OBJECTS+=("$obj")
done

gfortran "${OBJECTS[@]}" $SDL_LIBS -lm -o "$BIN"

echo "Built $BIN"
