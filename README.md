# Vector Strike 77

Modern Fortran + SDL2 vector-arcade space shooter vertical slice. The game uses a CPU-side 3D wireframe pipeline in Fortran, then hands final 2D colored line segments to a small SDL2 C boundary for windowing, input, audio beeps, screenshots, and presentation.

## Dependencies

Ubuntu/WSL2 packages:

```bash
sudo apt-get update
sudo apt-get install -y build-essential gfortran gcc pkg-config libsdl2-dev
```

`fpm.toml` is included for project metadata, but the reliable build path is `./build.sh` because it uses `pkg-config` for SDL2.

## Build And Run

```bash
./build.sh
./run.sh
```

Demo/capture modes:

```bash
./run.sh --demo
./run.sh --screenshot
```

`--screenshot` saves `capture.bmp` after a scripted showcase warmup. In-game, press `F12` to save `capture.bmp`.

## Controls

Mouse or `WASD`/arrows aim the reticle — use either, swap any time. Left mouse, `Space`, or `F` fires. Hold `Left Shift` or right mouse for precision aim. `P` pauses. `R` restarts after game over. `F12` captures a screenshot. `Esc` quits. See `docs/CONTROLS.md` for the full list.

## Rendering Pipeline

The render path is explicit and CPU-side:

1. Wire models are defined as vertices plus colored edges.
2. Local transforms scale and rotate object vertices.
3. World transforms place enemies, gates, debris, and cockpit elements.
4. Camera/view transform moves world points into camera space.
5. Perspective projection maps visible 3D points to 2D screen coordinates.
6. Near-plane and coarse visibility checks reject unsafe lines.
7. Screen-space colored line records are generated.
8. SDL2 draws black-background glowing vector lines, HUD text, reticle, and feedback.

## Current Slice

The game includes a title screen with persistent high score, playable crosshair-aim combat, 5 enemy motion archetypes (straight, weave, lateral sweep, dive-bomber, orbiter) unlocked across the first waves, wave pacing, hit detection, score/lives/shield/high HUD, vector explosions, screen shake, simple SDL audio tones, a naturalized demo autopilot, and screenshot capture. Frame pacing is driven by SDL's VSYNC when available and falls back to a 60 Hz sleep otherwise.

## Tests

```bash
./test.sh
```

Tests cover projection safety, transforms, and the model-to-screen-line pipeline.

## Layout

`app/` contains the executable entry point. `src/` contains Fortran game/math/rendering modules plus the small SDL2 wrapper. `test/` contains focused Fortran tests. `docs/` has architecture and control notes. `build.sh`, `run.sh`, and `test.sh` at the repo root are the build/run/test helpers.
