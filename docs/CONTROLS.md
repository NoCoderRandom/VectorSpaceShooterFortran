# Controls

## Steer the ship

`W` `A` `S` `D` move the Mk-77 through cockpit space. Use them to dodge the hazard drifting toward you. Each sector has a different hazard: Picket Buoys, Asteroid Shards, Gate Spines, Wrecks, Void Arcs, or Hive Lattice.

## Aim the reticle

Mouse motion or arrow keys drive the reticle. Ship movement and reticle aim are independent — steer with one hand, aim with the other.

## Fire and intercept

Left mouse button, `Space`, or `F` fires. Destroying a Lancer's tracking rocket is a valid target; the nearest Lance to the reticle is prioritized.

## Precision

Hold `Left Shift` or the right mouse button to slow aim and sharpen shots. Precision aim tightens the intercept window on Lances.

## Hazards and pickups

Hazards are not enemies — they cannot be shot. Dodge them with `WASD`. Contact drains shield first, then one hull. A brief invulnerability window prevents grind damage.

Destroyed Coil sometimes drop a shard. Fly through it to collect: cyan restores shield, green restores a hull, amber is score.

## Combo streak

Chain kills and intercepts without getting hit. The streak is shown in the HUD (`STREAK xN`) and raises the score multiplier up to 4.0x. Taking any damage breaks the streak. Clearing a wave without breaking the streak awards a PERFECT WAVE bonus.

## Finale

After defeating the Maw Core at the end of Sector VI, you choose:
- `A` or `Left`: **CLOSE GATE** — sacrifice ending
- `B` or `Right`: **HOLD GATE** — return-home ending
- `Enter` / `Space` / `Fire` commits the choice.

## Other

`P` pauses during play.

`R` restarts after game over or victory.

`F12` saves `capture.bmp`.

`Esc` quits.

The OS cursor is hidden while the game is running — the in-game reticle is your cursor.

## Demo Mode

Run `./run.sh --demo` for a fast-starting curated demo. It immediately enters play, steers past hazards, tracks targets, intercepts Lances, fires with a quick cadence, and cycles sector palettes without waiting on title-screen input or the finale choice.
