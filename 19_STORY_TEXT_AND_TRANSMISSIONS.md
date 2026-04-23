# Prompt 19 — Story text and transmissions

Add a light story layer so the game has voice and stakes, without turning into a visual novel.

Requirements:
- a short briefing transmission before Sector I begins
- a between-sector transmission before Sector II and Sector III
- a closing transmission after the final boss
- transmissions are 2-4 lines of text in the existing vector HUD font
- text appears line by line with a small reveal cadence, not all at once
- a "PRESS SPACE / FIRE TO CONTINUE" prompt ends every transmission
- transmissions never appear during the title or gameplay; only between states
- demo mode skips the transmissions so the auto-play showcase stays fast

Suggested voice:
- in-universe pilot chatter from command, not an omniscient narrator
- short, punchy, arcade-flavored lines
- sample tone: "PATROL WING, HOSTILES ON YOUR AXIS. HOLD THE PICKET."

Design notes:
- reuse the existing font and glow renderer for the text
- keep layout consistent between transmissions so players learn where to look
- add a faint scanline or underline accent so the panel reads as a comms screen, not the HUD
- play a short beep cue when a new line appears

Verify:
- the transmissions make the game feel like it has a world
- they never block or slow down gameplay
- a new player understands what sector they are heading into
- demo mode still auto-plays without pausing on text
