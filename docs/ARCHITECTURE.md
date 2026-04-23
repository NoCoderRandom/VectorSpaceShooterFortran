# Architecture

Vector Strike 77 keeps the game architecture Fortran-first. SDL2 is isolated in `src/sdl_platform.c` and `src/platform.f90`; every gameplay, math, model, HUD, and line-pipeline decision lives in Fortran modules.

## Module Boundaries

`vector_math.f90` provides `vec2`, `vec3`, transforms, camera view conversion, perspective projection, and near-plane checks.

`model_library.f90` defines wire models as vertices plus colored edges and emits screen-space line records from model/world/camera transforms.

`vector_renderer.f90` draws screen-space lines through the platform API with a small multi-pass glow style.

`vector_font.f90` is a 5x7 vector/line HUD font, avoiding sprite or bitmap asset dependence.

`game.f90` owns deterministic update passes: input, enemies, spawning, collision, scoring, particles, HUD, title screen, demo mode, and render ordering.

`sdl_platform.c` owns SDL init/shutdown, window/renderer lifetime, event polling, keyboard state, line drawing, BMP screenshots, and a tiny generated-tone audio callback.

## Control Model

The selected gameplay model is crosshair aim with forward motion. Enemies approach through depth toward the player, making perspective scale and looming motion part of the threat read. This keeps handling responsive and readable on keyboard while preserving the 3D vector presentation.

## Render Ordering

Each frame clears to black, then draws stars, depth grid, environment gates, debris, enemies, cockpit/reticle, and HUD/screen overlays. The visual identity comes from colored edge models, sparse negative space, simple glow overdraw, and vector text.

## Strongest Features

- CPU-side Fortran 3D wireframe pipeline: `vector_math` -> `model_library` -> `vector_renderer` with per-edge color and near-plane safety.
- Five enemy motion archetypes whose difficulty pool opens up as the wave count rises.
- Multi-voice SDL audio mixer with 24 concurrent voices and linear attack/release envelope.
- Screen shake driven from a normalized `scene_camera` that keeps jitter consistent across resolutions.
- Demo mode with targeting autopilot plus aim/fire jitter so captured footage does not look mechanical.
- Persistent high score with HUD readout in play, title, and game-over screens.
- VSYNC-aware frame pacing that falls back to a 60 Hz sleep when VSYNC is unavailable.

## Known Limitations

Audio is intentionally synthetic and minimal. The SDL wrapper uses line drawing rather than platform-specific GPU geometry. The vector HUD font is a 5x7 cell font rendered as short horizontal strokes rather than true vector glyph outlines. `fpm.toml` is present as metadata, but `scripts/build.sh` is the supported SDL2 build path because it handles `pkg-config` reliably. High scores persist to `highscore.dat` in the current working directory; deleting that file resets the record.
