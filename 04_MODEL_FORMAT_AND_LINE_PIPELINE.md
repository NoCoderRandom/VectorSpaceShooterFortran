# Prompt 04 — Model format and line pipeline

Design model representation.

Requirements:
- objects defined by vertices + edges
- color/style information per object or line group
- support local transform and world placement
- easy to extend for enemies, player craft, obstacles, HUD markers

Implement:
- model storage module
- transformed line extraction
- screen-space line generation

Add at least:
- player craft wireframe
- enemy craft wireframe
- one environment test object

Verify:
- multiple object types render correctly
- silhouettes are readable
- pipeline is modular
