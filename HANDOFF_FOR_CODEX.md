# Handoff for Codex

Claude is out of tokens. This document is the full state of the project so you can pick up cleanly. Read it top to bottom before touching code.

## Master rule for you

Follow `00_MASTER_RULES.md`. For every prompt: build (`./scripts/build.sh`), run a quick smoke (`./scripts/run.sh --screenshot` is fastest), run tests (`./scripts/test.sh`), then commit and push. The repo is `https://github.com/NoCoderRandom/VectorSpaceShooterFortran` on `main`. Do not amend; always make new commits. The user (Niklas) is a non-coder — give him deliverables, not code questions.

## What is already done

Prompts 01-15 (codex's first burst) plus the polish pass are all in `main`. On top of that, Claude added:

- **Prompt 16 — Mouse + keyboard input.** DONE and pushed (commit `e874ada`). Mouse moves the reticle; keyboard takes over when WASD/arrows are pressed; left mouse fires; right mouse + Left Shift are precision modifiers; OS cursor hidden. See `src/sdl_platform.c` for `vs_mouse_state`, `src/platform.f90` for the wrapper, `src/game.f90` for `update_player_control`.
- **Prompts 17-22 — written as spec files only.** No code yet. Read them; `20_STORY_BIBLE.md` is canon for all naming/text in 17, 18, 19, 21, 22.
- **Prompt 18 — Sector campaign. PARTIALLY IMPLEMENTED, NOT COMMITTED.** See "Where to start" below — finish this first.

## Where to start: finish prompt 18 (sector campaign)

Open `src/game.f90`. The data + state-machine + scoring + audio cues for sectors are in place. The renderers and HUD are NOT yet wired to use the sector palette, and the victory/intro screens are not drawn. The build compiles with three "unused function" warnings — those are exactly the helpers you need to wire in.

### What is already wired in `src/game.f90`

- `state_victory = 3` parameter
- `waves_per_sector = 3`, `max_sector = 3` parameters
- `game_state_t` fields: `sector`, `sector_wave`, `kills_sector_wave`, `sector_intro_timer`, `demo_palette_timer`
- `reset_play` initializes sector=1, sector_wave=1, sector_intro_timer=2.6, message="PHOSPHOR-LEAD CLEARED FOR LAUNCH"
- `damage_enemy` applies `sector_score_mult(gs%sector)` and tracks `kills_sector_wave`
- `advance_wave(gs)` subroutine handles wave-up, sector advance (with shield refill +0.5 and triple beep), and final victory
- Helper functions: `sector_wave_quota`, `sector_score_mult`, `sector_name`
- Helper subroutines: `sector_palette_primary`, `sector_palette_accent`, `sector_palette_dim` (returning RGB) — currently UNUSED, that's why the warnings
- `update_game` decrements `sector_intro_timer` and has a `state_victory` case
- Main loop accepts restart from victory state

### What you still need to do for prompt 18

1. **Wire `sector_palette_dim` into `render_depth_grid`** — replace the hard-coded `0, 80, 120` and `0, 60, 105` with palette dim color (call helper, scale slightly for the second pass).
2. **Wire `sector_palette_primary` into `render_cockpit`** — replace the cockpit `0, 150, 255` and `0, 220, 255` and `0, 80, 140` with palette-based colors.
3. **Wire palette into starfield in `render_stars`** — currently shade is monochrome; tint the high channel by sector primary.
4. **Add `render_sector_intro`** — when `gs%sector_intro_timer > 0`, draw the centered sector name (use `sector_name(gs%sector)`) at the top-third of the screen with fade-in/fade-out based on the timer. Call from `render_game` in the `state_play` branch BEFORE the HUD.
5. **Add `render_victory`** — draw a victory screen using the exact transmission text from `20_STORY_BIBLE.md` (the four-line "GATE IS DARK..." block). Pattern after `render_game_over`. Call from `render_game` in the `state_victory` case.
6. **Update `render_hud`** — rename "LIVES" to "HULL" (story bible says so). Add a small line that shows current sector + wave-in-sector, e.g. "S1 W2/3" near the wave counter.
7. **Update demo autopilot** — use `gs%demo_palette_timer` to cycle `gs%sector` through 1→2→3→1 every ~9 seconds, ONLY when `gs%demo_mode` is true. Reset `gs%kills_sector_wave` and bump `sector_intro_timer` on each cycle so demo viewers see the title overlay.
8. **Update title screen controls line** to mention HULL instead of LIVES if needed (currently says nothing about HULL). Maybe also add a one-line "PHOSPHOR WING / SECTOR CAMPAIGN" subtitle under "MODERN FORTRAN / SDL2 VECTOR ARCADE".

When all that builds clean (no warnings) and runs visually distinct per sector, commit:

```
git add -u && git commit -m "Implement prompt 18: sector campaign with per-sector palette and victory"
git push origin main
```

## Then continue with the remaining steps

Each step is one prompt → one commit → one push. Do them in this order:

### Step 3 — Prompt 17 (Bosses)

Read `17_BOSS_ENCOUNTERS.md`. Per `20_STORY_BIBLE.md`:
- Sector I boss: **Harrower** — large gunship, sweeps and fires volleys
- Sector II boss: **Seer** — crystalline sensor node that hides behind escorts
- Sector III boss: **The Maw** — final hive node at the captured gate

Pattern: after `waves_per_sector` waves in a sector, instead of immediately advancing, spawn a boss. When boss dies, then advance (sector 3 boss kill → victory).

You'll need:
- `boss_t` type (or extend `enemy_t` with hp >> 1 and a flag) in `game.f90`
- A boss spawn function and three boss models in `model_library.f90` (use existing `wire_model` machinery)
- HP bar render at top of screen during boss fight
- Entrance telegraph (~1.5s warning text, screen pulse, audio swell)
- Heavier death (more particles, longer shake, audio rumble)
- Update `advance_wave` so the sector boundary spawns the boss instead of immediately advancing

### Step 4 — Prompt 19 (Transmissions)

Read `19_STORY_TEXT_AND_TRANSMISSIONS.md` and the exact text in `20_STORY_BIBLE.md`. Add a `state_transmission` between sectors and at victory/defeat. Reveal one line at a time with a soft typewriter click. Use `vector_font` for the text.

Lines per `20_STORY_BIBLE.md`:
- Opening (before Sector I)
- Between Sector I and Sector II (after Harrower dies)
- Between Sector II and Sector III (after Seer dies)
- Victory (after Maw dies)
- Defeat (on game over)

### Step 5 — Prompt 21 (Visual polish)

Read `21_VISUAL_POLISH_AND_EXPLOSIONS.md`. Five Coil enemy models (Scout, Hunter, Skimmer, Striker, Warden) — one per pattern (1-5). Expanding ring explosions + tumbling hull chunks. Lock-on brackets. Per-sector palette fade between sectors.

### Step 6 — Prompt 22 (Audio polish)

Read `22_AUDIO_POLISH_AND_SFX.md`. Layered explosion SFX (low thud + mid crack + high sizzle). Per-sector ambient drone. Escalating proximity alert. Coil chatter. Transmission typewriter click. Player hit + game-over polish.

The 24-voice mixer in `src/sdl_platform.c` already supports all of this. If you want a noise-style layer for sizzle/sweeps, add `vs_audio_noise(freq, seconds, volume, decay)` next to `vs_audio_beep` and a Fortran wrapper in `platform.f90`.

## Project layout

- `src/game.f90` — main game logic (1200+ lines now)
- `src/platform.f90` — Fortran wrapper around SDL2
- `src/sdl_platform.c` — C boundary to SDL2
- `src/vector_math.f90` — vec3/transforms/projection
- `src/model_library.f90` — wire models
- `src/vector_renderer.f90` — line drawing helpers
- `src/vector_font.f90` — 5x7 pixel font
- `app/main.f90` — entry point
- `test/` — tests (run with `./scripts/test.sh`)
- `scripts/build.sh`, `scripts/run.sh`, `scripts/test.sh`

## Important conventions

- Use `iso_c_binding` for any new C interop
- Real kind is `rk` (defined in `vector_math.f90`)
- Per `20_STORY_BIBLE.md`: never exceed 4 lines per transmission, never exceed 40 chars per line, no humor, no pop-culture refs, no naming Phosphor-Lead
- Score multipliers per sector: x1, x1.5, x2 (already in `sector_score_mult`)
- All audio must remain off when SDL audio init fails — never crash on headless

## Verify after every commit

```bash
./scripts/build.sh      # must compile clean
./scripts/test.sh       # must pass
./scripts/run.sh --screenshot   # must produce capture.bmp without crashing
git status              # working tree clean
git log --oneline -5    # latest commit visible
```

Good hunting. — Claude
