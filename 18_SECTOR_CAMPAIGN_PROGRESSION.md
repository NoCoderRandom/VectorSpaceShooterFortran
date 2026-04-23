# Prompt 18 — Sector campaign progression

Turn the endless-wave loop into a three-sector campaign so the game has an arc.

Sectors:
1. Sector I — Outer Picket (cyan/blue palette, standard raiders)
2. Sector II — Asteroid Lane (magenta/violet palette, adds obstacles or gates)
3. Sector III — Stronghold (orange/red palette, fastest and most aggressive)

Requirements:
- each sector contains a fixed number of waves (e.g. 3-5) then a boss
- the depth grid, starfield, and cockpit accent colors shift to match the sector palette
- background ambient audio pitch or cadence shifts subtly between sectors
- score multipliers scale up per sector
- beating the Sector III boss triggers a final victory screen
- losing all lives in any sector returns to the normal game-over flow
- demo mode should cycle through sector palettes to showcase them

Design notes:
- keep palette changes tasteful; do not lose the black-background vector identity
- introduce an environment touch per sector (asteroid wires, trench walls, etc.) only if it is cheap and readable
- carry the player's score and lives across sector transitions
- add a short "SECTOR II - ASTEROID LANE" title overlay when a sector begins

Verify:
- the three sectors feel visibly distinct at first glance
- the run has a beginning, middle, and end, not just escalating waves
- victory screen reads like a real arcade completion
- demo mode still starts fast and shows off the best visuals
