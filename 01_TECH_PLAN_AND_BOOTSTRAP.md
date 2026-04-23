# Prompt 01 — Technical plan and bootstrap

Create the initial project skeleton for a Modern Fortran vector-arcade shooter using SDL2.

Before writing code:
- state the exact rendering pipeline
- state the exact apt packages required on Ubuntu/WSL2
- state whether you will use direct Fortran SDL2 bindings or a very small wrapper layer
- prefer keeping the majority of code in Fortran

Create a clean layout with:
- app/
- src/
- test/
- docs/
- scripts/
- assets/
- .github/workflows/

Create:
- README.md
- fpm.toml
- app/main.f90
- scripts/build.sh
- scripts/run.sh
- a placeholder executable that opens a window and displays a simple success state

Expected milestone:
- a window opens
- it remains responsive
- closing works correctly
- a basic title or line appears

After implementation:
- build
- run
- verify window creation, clean shutdown, and basic drawing
