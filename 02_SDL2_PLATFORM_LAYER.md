# Prompt 02 — SDL2 platform layer

Implement a dedicated platform layer.

Responsibilities:
- SDL init/shutdown
- window creation/destruction
- renderer creation/destruction
- event polling
- keyboard input state
- frame timing
- optional audio init hooks
- basic line drawing API exposed cleanly to Fortran modules

Requirements:
- isolate SDL-specific code behind one boundary
- avoid spreading low-level SDL calls everywhere
- if wrappers are needed, keep them tiny and documented

Deliver a demo:
- open a black window
- draw a few colored test lines
- show event loop and ESC/quit handling

Verify:
- no crash on open/close
- responsive input
- visible colored lines on black
