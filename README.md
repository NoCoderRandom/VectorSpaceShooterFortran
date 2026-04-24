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

`WASD` steer the ship through drifting hazards. Mouse or arrow keys aim the reticle — independent of ship movement. Left mouse, `Space`, or `F` fires. Hold `Left Shift` or right mouse for precision aim. `P` pauses. `R` restarts after game over. `F12` captures a screenshot. `Esc` quits. See `docs/CONTROLS.md` for the full list.

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

Six-sector campaign with a branching finale. The Mk-77 steers independently from the reticle, so you dodge hazards with `WASD` while aiming with mouse or arrow keys.

Mechanics:
- Six sectors, each with a distinct palette and signature hazard (Picket Buoys, Asteroid Shards, Gate Spines, Wrecks, Void Arcs, Hive Lattice).
- Six bosses (Harrower, Seer, The Maw, Boneforge, Stormveil, Maw Core), each multi-phase with attack-cadence escalation and a "PHASE" banner on transitions.
- Rocket enemies (Lancers) fire tracking Lances you shoot down or evade.
- Elite variants: Juggernaut (shielded Hunter, breaks with one hit), Phantom (flickering Skimmer, only hittable during the visible pulse).
- Lattice shards drop from kills: cyan restores shield, green restores hull, amber is pure score.
- Combo streak scoring with multiplier up to 4.0x, perfect-wave bonus, and HUD readout.
- Wingmate comms ([K] Kestrel, [V] Vane, [E] Ember) react to Lances, Wrecks, low shield, and sector entry.
- Two endings: close the gate (sacrifice) or hold it (return home).

Vector-style rendering on a black background with bright colored lines. Frame pacing is driven by SDL's VSYNC when available and falls back to a 60 Hz sleep otherwise.

## Tests

```bash
./test.sh
```

Tests cover projection safety, transforms, and the model-to-screen-line pipeline.

## Layout

`app/` contains the executable entry point. `src/` contains Fortran game/math/rendering modules plus the small SDL2 wrapper. `test/` contains focused Fortran tests. `docs/` has architecture and control notes. `build.sh`, `run.sh`, and `test.sh` at the repo root are the build/run/test helpers.
