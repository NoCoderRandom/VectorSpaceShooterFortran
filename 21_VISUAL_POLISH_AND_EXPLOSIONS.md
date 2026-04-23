# Prompt 21 — Visual polish, explosions, and enemy variety

Lift the visual quality from "works" to "investor reel".

## Explosions

Replace the current point-particle spray with layered vector explosions.

Requirements:
- expanding ring of short line segments at impact, fading over 0.4s
- 6-12 tumbling hull chunks (short line pairs) that rotate as they fly
- a single-frame radial white flash at kill moment
- boss death adds 2-3 secondary internal explosions before the main break-up
- different palette per hit outcome: yellow for damage, orange/red for kill, cyan for player hit

Implementation notes:
- hull chunks are short segments with independent rotation velocity
- the expanding ring is N segments arranged on a circle whose radius grows with time
- keep particle budget in mind; the existing max_particles cap may need to rise

## Enemy ship variety

Give each Coil archetype (see prompt 20) its own wireframe silhouette.

Requirements:
- Scout: small, dart-shaped, minimal detail (current enemy_raider can stay for Scouts)
- Hunter: broader, aggressive forward sweep, twin prongs
- Skimmer: flat delta shape, wide and thin
- Striker: pointed dive-form with elongated body
- Warden: disc/ring body with orbiting filaments

Each model must:
- use edge colors consistent with the Coil (violet, red, sickly green accents)
- be readable at distance (silhouette reads before color)
- fit in the existing append_model_lines pipeline without special-casing

## Player lock-on indicator

- when the reticle is over an enemy in range, draw 4 corner brackets around that enemy's projected screen position
- brackets tint red if the target is inside damage range, yellow if only tracking
- a thin line from the cockpit to the locked target when firing

## Cockpit frame polish

- add faint interior tick marks along the bottom trapezoid to sell the "viewport"
- subtle side-rail HUD accents that react to sector palette
- keep negative space dominant; do not clutter

## Sector palette shifts (from prompt 18)

- depth grid, starfield shade, and cockpit accents all shift to the active sector palette
- transitions between sectors fade over ~1s, not snap

## Verify

- a still frame from any sector looks intentional and distinct
- enemies read as different types on sight, not by movement pattern alone
- explosions feel heavier and more satisfying than a puff of dots
- lock-on makes aiming feel purposeful
