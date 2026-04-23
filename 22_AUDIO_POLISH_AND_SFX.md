# Prompt 22 — Audio polish and SFX

Take the audio from "placeholder beeps" to "arcade cabinet".

## Layered explosions

A kill should sound like three things at once, not one beep:
- low-frequency thud (60-90 Hz, 0.18s, fast decay)
- mid-frequency crack (240-360 Hz, 0.08s)
- high-frequency sizzle (1100-1600 Hz, 0.12s, lower volume)

Boss kill adds a sustained 40 Hz rumble and a triumphant rising chord.

## Sector ambience

Each sector plays a faint sustained drone at a distinct pitch so players feel the location change:
- Sector I: A2 (110 Hz) calm drone
- Sector II: low E (82 Hz) with a slow 4 Hz tremolo
- Sector III: F#2 (92 Hz) dissonant against the shot pitch

Drone fades in when the sector begins and fades out during transmissions.

## Proximity warning

Replace the flat "danger tone" with an escalating alert:
- pitch rises as the nearest enemy's depth drops below 10.0
- pulse rate doubles when below 5.0
- stops the instant that enemy dies or passes

## Coil chatter

Very faint tonal bursts at irregular intervals during gameplay, to suggest the hive is present:
- random pitches in the 600-900 Hz band
- short (0.04s), quiet (volume 0.03-0.05)
- never during transmissions, title, or game over

## Transmission cues

Each line of a transmission plays a soft typewriter-style click on reveal.

## Shot variety

- normal shot keeps the current 780 Hz chirp
- rapid consecutive shots detune slightly so a burst doesn't sound like a loop
- precision aim shots get a higher, cleaner tone

## Player hit and game over

- player hit: low 70 Hz hit + brief white-noise-like descending sweep
- game over: the existing two-tone descending cadence, plus a final sub-bass hit on the last line of text

## Implementation notes

- the existing 24-voice mixer in sdl_platform.c can handle all of this; no new backend needed
- add a platform_audio_noise(freq, seconds, volume, decay) helper if a sizzle/noise layer is wanted
- volumes must be balanced so the drone sits under SFX, not over them
- all audio must remain off when SDL audio init fails (do not crash on headless systems)

## Verify

- a kill feels impactful, not thin
- the three sectors sound different even with eyes closed
- the proximity alert builds tension instead of nagging
- boss fights feel bigger in the mix
- nothing clips or buzzes
