# Prompt 17 — Boss encounters

Introduce large boss enemies that appear at the end of each sector and give the player a clear, dramatic target.

Requirements:
- one unique boss wireframe per sector
- boss is visibly larger and more detailed than a normal raider
- boss has multi-stage hit points, not a one-shot kill
- a labeled boss health bar is drawn at the top or bottom of the screen during the fight
- boss approach telegraphs before the fight begins (siren tone, slow-in from depth, warning text)
- defeating a boss triggers a louder explosion, bonus score, and a short victory pause
- the player cannot be struck by normal enemies during the boss entrance animation

Design notes:
- boss models live alongside player/enemy models in the model library
- reuse the existing hit-detection math (screen-space radius vs reticle) but expose a larger radius
- boss motion should feel heavier than a raider: slow weave, occasional sweep, not erratic
- boss death should spawn a dense vector debris burst using the existing particle system

Verify:
- the boss reads as a boss at first sight, not as a bigger raider
- the health bar communicates damage taken
- combat with the boss is clearly a different beat than a normal wave
- destroying the boss feels celebratory
- the player can still die to the boss, and game over flows correctly
