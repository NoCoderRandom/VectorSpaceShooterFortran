# Prompt 20 — Story bible

This document is canon. All transmissions, HUD labels, sector names, boss names, enemy names, and victory/defeat text must be consistent with it. Implementation prompts pull their strings from here.

## Setting

The year is 2177. Humanity's long-range jump gates are the only reason Sol still matters — they are how colonies, fleets, and news travel between stars. A year ago, a machine hive surfaced from the dark between galaxies and began consuming jump gates one system at a time. It speaks on no frequency. Sol named it **The Silent Coil**.

## The war

The Coil eats jump gates to harvest their quantum lattice, then uses that lattice to open its own rift back to the hive. Any system whose gate falls becomes a staging world for the next strike. Three gates remain between the Coil and Sol.

The player's squadron, **Phosphor Wing**, is the last auxiliary unit still flying the prototype **Mk-77 Vector Interceptor** — the only fighter small enough to slip past Coil sensors and still carry a shield generator. Mass production failed. Only Phosphor flies.

## The player

- Callsign: **Pilot 77** (spoken as "Phosphor-Lead")
- Ship: Mk-77 Vector Interceptor (the wireframe craft on the title screen)
- Orders: hold the three remaining gates long enough for Sol to finish the evacuation

## Command voice

- Speaker: **Phosphor Command** (voice of mission control)
- Tone: calm, clipped, arcade-military; never panicked, never cheerful
- Never more than 4 lines per transmission

## Factions of the Coil

Each enemy archetype in the code maps to a Coil unit name. HUD and transmissions use these names.

| Code pattern | Coil name    | Role                                  |
|--------------|--------------|---------------------------------------|
| 1 (straight) | **Scout**    | disposable recon drones               |
| 2 (weave)    | **Hunter**   | attack wing, weaving approach         |
| 3 (lateral)  | **Skimmer**  | sideways strafe, cut across the picket|
| 4 (dive)     | **Striker**  | dives from above toward the reticle   |
| 5 (orbit)    | **Warden**   | circles and binds the pilot's aim     |

## Bosses

- Sector I boss: **Harrower** — large Coil gunship, sweeps and fires volleys
- Sector II boss: **Seer** — crystalline sensor node that hides behind escorts
- Sector III boss: **The Maw** — the hive node occupying the captured jump gate; final enemy

## Sectors (for prompt 18)

1. **Sector I — Outer Picket.** Cyan/blue palette. Standard raider mix. Boss: Harrower.
2. **Sector II — Asteroid Lane.** Magenta/violet palette. Adds fractured rock wires drifting through the lane. Boss: Seer.
3. **Sector III — Stronghold.** Orange/red palette. Fastest enemies, heaviest fire. Boss: The Maw.

## Transmissions (exact text)

Opening (before Sector I):
```
>> PHOSPHOR COMMAND TO PILOT 77
>> COIL SCOUTS CROSSED THE PICKET LINE
>> HOLD THE GATE. SOL CANNOT KNOW THEY FOUND US
>> GOOD HUNTING, PHOSPHOR-LEAD
```

Between Sector I and Sector II:
```
>> PICKET HELD. HARROWER DOWN
>> ASTEROID LANE IS COIL TERRITORY NOW
>> CUT THROUGH. DO NOT LINGER
>> THE ROCKS ARE ALIVE
```

Between Sector II and Sector III:
```
>> LAST TRANSMISSION, PHOSPHOR-LEAD
>> THE MAW IS AWAKE AT THE GATE
>> CLOSE IT OR THE COIL SEES HOME
>> SOL IS WITH YOU
```

Victory (after The Maw):
```
>> GATE IS DARK. COIL IS SILENT
>> SOL CONFIRMS EVACUATION COMPLETE
>> PHOSPHOR-LEAD, COME HOME
>> WE LOG THIS ONE IN LEGEND
```

Defeat (on final game over):
```
>> PHOSPHOR SIGNAL LOST
>> COIL ADVANCING ON SOL
>> STAND BY FOR NEXT PILOT
```

## HUD wording

- "SCORE" stays "SCORE"
- "WAVE" becomes "WAVE" within a sector; the sector title overlays the screen at sector start ("SECTOR I — OUTER PICKET")
- "LIVES" becomes "HULL" so it fits the Mk-77 theme (3 hulls = 3 lives)
- "SHIELD" stays "SHIELD"
- "HIGH" stays "HIGH"

## Do nots

- Do not name the Coil's home, its motive beyond gate-harvesting, or any individual Coil character
- Do not give Phosphor-Lead a personality, gender, face, or voice line — the player is the pilot
- Do not add humor or pop-culture references in transmissions
- Do not exceed 4 lines per transmission or 40 characters per line
