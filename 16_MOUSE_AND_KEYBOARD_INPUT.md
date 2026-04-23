# Prompt 16 — Mouse and keyboard together

Let the player aim and fire with the mouse alongside the existing keyboard controls, without breaking keyboard-only play.

Requirements:
- mouse movement drives the reticle in screen space
- left mouse button fires, same cadence as Space/F
- right mouse button or Left Shift acts as precision aim
- whichever input moved last wins, so a player can swap mid-run
- hide or dim the OS cursor inside the window
- mouse input must not interfere with demo mode or screenshot mode
- keyboard-only play must still feel identical to before

Implementation notes:
- add a small mouse state surface to the SDL2 platform layer
- expose mouse position, button state, and a "mouse was moved this frame" flag to Fortran
- map mouse position to the same normalized reticle coordinates the keyboard uses
- keep the aim speed and clamp limits consistent with keyboard so the cockpit frame stays readable

Update docs:
- CONTROLS.md must describe mouse+keyboard usage
- README controls section must mention the new inputs

Verify:
- mouse aim feels 1:1 with the reticle
- left click fires and feels as responsive as Space
- keyboard still aims and fires cleanly on its own
- switching hand on the fly does not jitter the reticle
- demo and screenshot modes still start cleanly
