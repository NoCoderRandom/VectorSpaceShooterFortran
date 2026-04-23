# MASTER RULES FOR THE AI CODER

You are building a showcase-quality Modern Fortran game.
The goal is that people should say: "Wow, is this really a Modern Fortran game?"

## Creative target
This is NOT:
- a terminal ASCII toy
- a generic retro shooter
- a sprite-first game

This IS:
- a windowed vector-style space shooter
- 3D-looking through perspective projection
- black background with bright colored lines
- first-person or near-first-person arcade framing
- dramatic, readable, investor/demo quality

## Technology
- Core game and math in modern Fortran
- SDL2 for window/input/timing/audio
- Optional tiny wrapper layer if needed
- Prefer fpm layout if practical
- If packages are needed, tell the user the exact apt packages first

## Fortran-first architecture
Lean into:
- modules
- allocatable arrays
- numeric clarity
- deterministic update passes
- array-oriented transforms where useful
- clear boundaries between math, rendering, gameplay, and platform

## Required render pipeline
1. object model data (vertices + edges)
2. local transform
3. world transform
4. camera/view transform
5. perspective projection
6. clipping / visibility checks
7. 2D line generation
8. line draw through SDL2

No sprites as the primary art style.
No filled-polygon look as the main identity.

## Quality bar
The project is not done when it merely works.
It must:
- handle well
- look impressive in screenshots/video
- have strong game feel
- have visible feedback
- have good pacing
- look intentionally designed

## Mandatory loop after every prompt
1. Build
2. Run
3. Verify honestly
4. If broken, fix before stopping
5. Do not continue until the step works

## Always output after each prompt
- Files created or changed
- Full contents of changed files
- Exact build command
- Exact run command
- Exact expected behavior
- What was actually verified
- Any apt packages the user must install now

## Scope control
- Do not rewrite working modules casually
- Keep the architecture stable once established
- Prefer robust solutions over clever tricks
- Keep the repo GitHub-ready
- No AI logs, no personal junk files

## Visual rules
Emphasize:
- black negative space
- bright colored lines
- readable silhouettes
- motion that sells depth
- line intensity variation
- restrained glow/persistence tricks
- stable frame pacing

## Gameplay rules
The game must quickly become fun:
- responsive input
- readable threats
- satisfying shooting
- clear score/reward loop
- escalating intensity

## Final target
This should become a vertical slice worthy of showing.
If it is functional but bland, it is not enough.
