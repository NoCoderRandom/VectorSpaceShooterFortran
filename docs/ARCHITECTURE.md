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

The selected gameplay model is crosshair aim with independent ship translation. Enemies and hazards approach through depth toward the player; `WASD` translates the Mk-77 laterally and vertically within a cockpit-space envelope while the reticle stays on its own axis via mouse or arrow keys. The scene camera carries the ship offset so hazards, enemies, and starfield all slide correctly.

## Gameplay Systems

- **Hazards (`hazard_t`)**: collidable obstacles, per-sector kind (Picket Buoys, Shards, Gate Spines, Wrecks, Arcs, Lattice). Contact triggers `player_hit` with a 0.85s i-frame.
- **Rockets (`rocket_t`)**: Lancer enemies fire tracking Lances with a limited turn rate. Player shots intercept them via `find_rocket_target` before the regular enemy lock-on path.
- **Shards (`shard_t`)**: cyan/green/amber lattice pickups drift toward the ship with a soft magnet and apply on fly-through.
- **Elite variants**: `variant_juggernaut` (green cage, one hit to break) and `variant_phantom` (visibility pulse).
- **Streak scoring**: `add_streak` / `break_streak` track chain progress; `streak_multiplier` scales kill and intercept score.
- **Multi-phase bosses**: `maybe_trigger_boss_phase` flips phases at HP thresholds with an invulnerable pause and cadence boost.
- **Wingmate comms**: `trigger_comm` + `check_comm_events` route short situational lines to the HUD comm banner.
- **Finale**: `state_finale_choice` resolves into `tx_ending_close` or `tx_ending_hold`.

## Render Ordering

Each frame clears to black, then draws stars, depth grid, environment gates, debris, enemies, cockpit/reticle, and HUD/screen overlays. The visual identity comes from colored edge models, sparse negative space, simple glow overdraw, and vector text.

## Strongest Features

- CPU-side Fortran 3D wireframe pipeline: `vector_math` -> `model_library` -> `vector_renderer` with per-edge color and near-plane safety.
- Six-sector campaign with distinct palettes, hazards, and multi-phase bosses.
- Independent ship-steer + reticle-aim input split, enabling dodge and aim on the same keyboard.
- Tracking-rocket threat with shoot-down intercept prioritized ahead of enemy lock-on.
- Combo streak scoring with PERFECT-wave bonus and visible HUD multiplier.
- Wingmate voice-over that reacts to in-game events, giving the war a tone without cutscenes.
- Multi-voice SDL audio mixer with 24 concurrent voices and linear attack/release envelope.
- Screen shake driven from a normalized `scene_camera` that keeps jitter consistent across resolutions.
- Demo mode with targeting autopilot, ship steering, and sector cycling so captured footage does not look mechanical.
- Persistent high score with HUD readout in play, title, game-over, and victory screens.
- VSYNC-aware frame pacing that falls back to a 60 Hz sleep when VSYNC is unavailable.

## Known Limitations

Audio is intentionally synthetic and minimal. The SDL wrapper uses line drawing rather than platform-specific GPU geometry. The vector HUD font is a 5x7 cell font rendered as short horizontal strokes rather than true vector glyph outlines. `fpm.toml` is present as metadata, but `./build.sh` is the supported SDL2 build path because it handles `pkg-config` reliably. High scores persist to `highscore.dat` in the current working directory; deleting that file resets the record.
