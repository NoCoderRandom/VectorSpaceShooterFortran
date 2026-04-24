module model_library
    use vector_math, only: rk, vec2, vec3, transform3, camera3, apply_transform, project_point, pi
    implicit none
    private

    type, public :: edge3
        integer :: a = 1
        integer :: b = 1
        integer :: r = 255
        integer :: g = 255
        integer :: bcol = 255
    end type edge3

    type, public :: wire_model
        character(len=32) :: name = ""
        integer :: vertex_count = 0
        integer :: edge_count = 0
        type(vec3), allocatable :: vertices(:)
        type(edge3), allocatable :: edges(:)
        real(rk) :: radius = 1.0_rk
    end type wire_model

    type, public :: screen_line
        integer :: x1 = 0
        integer :: y1 = 0
        integer :: x2 = 0
        integer :: y2 = 0
        integer :: r = 255
        integer :: g = 255
        integer :: b = 255
        integer :: a = 255
        real(rk) :: depth = 1.0_rk
    end type screen_line

    public :: build_player_model
    public :: build_enemy_model
    public :: build_hunter_model
    public :: build_skimmer_model
    public :: build_striker_model
    public :: build_warden_model
    public :: build_harrower_model
    public :: build_seer_model
    public :: build_maw_model
    public :: build_boneforge_model
    public :: build_stormveil_model
    public :: build_maw_core_model
    public :: build_wreck_model
    public :: build_arc_model
    public :: build_lattice_model
    public :: build_gate_model
    public :: build_shield_gate_model
    public :: build_cube_model
    public :: build_buoy_model
    public :: build_shard_model
    public :: build_spine_model
    public :: build_rocket_model
    public :: build_lancer_model
    public :: build_pickup_model
    public :: build_cage_model
    public :: append_model_lines

contains

    subroutine build_player_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "player_craft", 9, 14, 1.7_rk)

        model%vertices = [ &
            vec3( 0.0_rk,  0.00_rk,  1.35_rk), &
            vec3(-0.85_rk, -0.20_rk, -0.55_rk), &
            vec3( 0.85_rk, -0.20_rk, -0.55_rk), &
            vec3(-1.15_rk, -0.42_rk, -0.90_rk), &
            vec3( 1.15_rk, -0.42_rk, -0.90_rk), &
            vec3( 0.0_rk,  0.46_rk, -0.35_rk), &
            vec3(-0.38_rk, -0.28_rk, -1.15_rk), &
            vec3( 0.38_rk, -0.28_rk, -1.15_rk), &
            vec3( 0.0_rk, -0.05_rk, -1.35_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 0, 255, 255)
        call set_edge(model, 2, 1, 3, 0, 255, 255)
        call set_edge(model, 3, 1, 6, 255, 255, 255)
        call set_edge(model, 4, 2, 6, 0, 150, 255)
        call set_edge(model, 5, 3, 6, 0, 150, 255)
        call set_edge(model, 6, 2, 3, 0, 180, 255)
        call set_edge(model, 7, 2, 4, 255, 80, 0)
        call set_edge(model, 8, 3, 5, 255, 80, 0)
        call set_edge(model, 9, 4, 7, 255, 180, 0)
        call set_edge(model, 10, 5, 8, 255, 180, 0)
        call set_edge(model, 11, 7, 9, 255, 80, 0)
        call set_edge(model, 12, 8, 9, 255, 80, 0)
        call set_edge(model, 13, 6, 9, 0, 220, 255)
        call set_edge(model, 14, 7, 8, 255, 255, 255)
    end subroutine build_player_model

    subroutine build_enemy_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "enemy_raider", 11, 18, 1.45_rk)

        model%vertices = [ &
            vec3( 0.0_rk,  0.0_rk,  1.10_rk), &
            vec3(-0.70_rk,  0.0_rk,  0.15_rk), &
            vec3( 0.70_rk,  0.0_rk,  0.15_rk), &
            vec3( 0.0_rk,  0.52_rk, -0.18_rk), &
            vec3( 0.0_rk, -0.52_rk, -0.18_rk), &
            vec3(-1.18_rk,  0.22_rk, -0.70_rk), &
            vec3( 1.18_rk,  0.22_rk, -0.70_rk), &
            vec3(-0.68_rk, -0.42_rk, -0.82_rk), &
            vec3( 0.68_rk, -0.42_rk, -0.82_rk), &
            vec3( 0.0_rk,  0.0_rk, -1.02_rk), &
            vec3( 0.0_rk,  0.0_rk,  0.10_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 255, 50, 80)
        call set_edge(model, 2, 1, 3, 255, 50, 80)
        call set_edge(model, 3, 1, 4, 255, 140, 0)
        call set_edge(model, 4, 1, 5, 255, 140, 0)
        call set_edge(model, 5, 2, 4, 255, 40, 180)
        call set_edge(model, 6, 3, 4, 255, 40, 180)
        call set_edge(model, 7, 2, 5, 255, 40, 180)
        call set_edge(model, 8, 3, 5, 255, 40, 180)
        call set_edge(model, 9, 2, 6, 255, 210, 0)
        call set_edge(model, 10, 3, 7, 255, 210, 0)
        call set_edge(model, 11, 6, 8, 255, 80, 0)
        call set_edge(model, 12, 7, 9, 255, 80, 0)
        call set_edge(model, 13, 8, 10, 255, 50, 80)
        call set_edge(model, 14, 9, 10, 255, 50, 80)
        call set_edge(model, 15, 4, 10, 255, 255, 80)
        call set_edge(model, 16, 5, 10, 255, 255, 80)
        call set_edge(model, 17, 2, 11, 255, 255, 255)
        call set_edge(model, 18, 3, 11, 255, 255, 255)
    end subroutine build_enemy_model

    subroutine build_hunter_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "enemy_hunter", 13, 22, 1.62_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  0.00_rk,  1.25_rk), &
            vec3(-0.55_rk,  0.15_rk,  0.75_rk), &
            vec3( 0.55_rk,  0.15_rk,  0.75_rk), &
            vec3( 0.00_rk,  0.00_rk,  0.20_rk), &
            vec3(-1.35_rk,  0.25_rk, -0.25_rk), &
            vec3( 1.35_rk,  0.25_rk, -0.25_rk), &
            vec3(-0.85_rk, -0.35_rk, -0.95_rk), &
            vec3( 0.85_rk, -0.35_rk, -0.95_rk), &
            vec3( 0.00_rk,  0.00_rk, -1.20_rk), &
            vec3( 0.00_rk,  0.55_rk, -0.15_rk), &
            vec3( 0.00_rk, -0.55_rk, -0.25_rk), &
            vec3(-0.35_rk, -0.15_rk,  1.05_rk), &
            vec3( 0.35_rk, -0.15_rk,  1.05_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 255, 60, 120)
        call set_edge(model, 2, 1, 3, 255, 60, 120)
        call set_edge(model, 3, 2, 4, 255, 40, 190)
        call set_edge(model, 4, 3, 4, 255, 40, 190)
        call set_edge(model, 5, 2, 5, 255, 90, 60)
        call set_edge(model, 6, 3, 6, 255, 90, 60)
        call set_edge(model, 7, 5, 7, 255, 180, 50)
        call set_edge(model, 8, 6, 8, 255, 180, 50)
        call set_edge(model, 9, 7, 9, 255, 60, 120)
        call set_edge(model, 10, 8, 9, 255, 60, 120)
        call set_edge(model, 11, 5, 10, 180, 255, 90)
        call set_edge(model, 12, 6, 10, 180, 255, 90)
        call set_edge(model, 13, 7, 11, 180, 255, 90)
        call set_edge(model, 14, 8, 11, 180, 255, 90)
        call set_edge(model, 15, 10, 4, 255, 255, 120)
        call set_edge(model, 16, 11, 4, 255, 255, 120)
        call set_edge(model, 17, 12, 2, 255, 255, 255)
        call set_edge(model, 18, 13, 3, 255, 255, 255)
        call set_edge(model, 19, 12, 1, 255, 90, 60)
        call set_edge(model, 20, 13, 1, 255, 90, 60)
        call set_edge(model, 21, 5, 9, 255, 40, 190)
        call set_edge(model, 22, 6, 9, 255, 40, 190)
    end subroutine build_hunter_model

    subroutine build_skimmer_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "enemy_skimmer", 10, 16, 1.75_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  0.00_rk,  1.00_rk), &
            vec3(-1.65_rk,  0.08_rk,  0.10_rk), &
            vec3( 1.65_rk,  0.08_rk,  0.10_rk), &
            vec3(-1.25_rk, -0.10_rk, -0.80_rk), &
            vec3( 1.25_rk, -0.10_rk, -0.80_rk), &
            vec3( 0.00_rk, -0.18_rk, -1.05_rk), &
            vec3(-0.55_rk,  0.18_rk,  0.25_rk), &
            vec3( 0.55_rk,  0.18_rk,  0.25_rk), &
            vec3(-0.35_rk, -0.18_rk, -0.30_rk), &
            vec3( 0.35_rk, -0.18_rk, -0.30_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 255, 40, 170)
        call set_edge(model, 2, 1, 3, 255, 40, 170)
        call set_edge(model, 3, 2, 4, 210, 255, 90)
        call set_edge(model, 4, 3, 5, 210, 255, 90)
        call set_edge(model, 5, 4, 6, 255, 90, 60)
        call set_edge(model, 6, 5, 6, 255, 90, 60)
        call set_edge(model, 7, 2, 7, 255, 255, 255)
        call set_edge(model, 8, 3, 8, 255, 255, 255)
        call set_edge(model, 9, 7, 8, 180, 255, 90)
        call set_edge(model, 10, 7, 9, 255, 40, 170)
        call set_edge(model, 11, 8, 10, 255, 40, 170)
        call set_edge(model, 12, 9, 10, 255, 90, 60)
        call set_edge(model, 13, 9, 6, 210, 255, 90)
        call set_edge(model, 14, 10, 6, 210, 255, 90)
        call set_edge(model, 15, 4, 9, 255, 255, 120)
        call set_edge(model, 16, 5, 10, 255, 255, 120)
    end subroutine build_skimmer_model

    subroutine build_striker_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "enemy_striker", 10, 17, 1.68_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  0.45_rk,  1.45_rk), &
            vec3( 0.00_rk, -0.45_rk,  0.95_rk), &
            vec3(-0.45_rk,  0.00_rk,  0.25_rk), &
            vec3( 0.45_rk,  0.00_rk,  0.25_rk), &
            vec3( 0.00_rk,  0.00_rk, -1.25_rk), &
            vec3(-0.82_rk, -0.22_rk, -0.55_rk), &
            vec3( 0.82_rk, -0.22_rk, -0.55_rk), &
            vec3( 0.00_rk,  0.78_rk, -0.20_rk), &
            vec3(-0.22_rk, -0.78_rk, -0.15_rk), &
            vec3( 0.22_rk, -0.78_rk, -0.15_rk)  &
        ]

        call set_edge(model, 1, 1, 3, 255, 70, 70)
        call set_edge(model, 2, 1, 4, 255, 70, 70)
        call set_edge(model, 3, 2, 3, 255, 210, 60)
        call set_edge(model, 4, 2, 4, 255, 210, 60)
        call set_edge(model, 5, 3, 5, 255, 40, 180)
        call set_edge(model, 6, 4, 5, 255, 40, 180)
        call set_edge(model, 7, 3, 6, 180, 255, 90)
        call set_edge(model, 8, 4, 7, 180, 255, 90)
        call set_edge(model, 9, 6, 5, 255, 70, 70)
        call set_edge(model, 10, 7, 5, 255, 70, 70)
        call set_edge(model, 11, 1, 8, 255, 255, 255)
        call set_edge(model, 12, 8, 5, 255, 210, 60)
        call set_edge(model, 13, 2, 9, 255, 255, 255)
        call set_edge(model, 14, 2, 10, 255, 255, 255)
        call set_edge(model, 15, 9, 6, 255, 40, 180)
        call set_edge(model, 16, 10, 7, 255, 40, 180)
        call set_edge(model, 17, 9, 10, 255, 210, 60)
    end subroutine build_striker_model

    subroutine build_warden_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "enemy_warden", 14, 24, 1.72_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  1.00_rk,  0.20_rk), &
            vec3( 0.72_rk,  0.72_rk,  0.20_rk), &
            vec3( 1.00_rk,  0.00_rk,  0.20_rk), &
            vec3( 0.72_rk, -0.72_rk,  0.20_rk), &
            vec3( 0.00_rk, -1.00_rk,  0.20_rk), &
            vec3(-0.72_rk, -0.72_rk,  0.20_rk), &
            vec3(-1.00_rk,  0.00_rk,  0.20_rk), &
            vec3(-0.72_rk,  0.72_rk,  0.20_rk), &
            vec3( 0.00_rk,  0.00_rk,  0.90_rk), &
            vec3( 0.00_rk,  0.00_rk, -0.70_rk), &
            vec3( 1.45_rk,  0.00_rk, -0.20_rk), &
            vec3(-1.45_rk,  0.00_rk, -0.20_rk), &
            vec3( 0.00_rk,  1.45_rk, -0.20_rk), &
            vec3( 0.00_rk, -1.45_rk, -0.20_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 190, 80, 255)
        call set_edge(model, 2, 2, 3, 190, 80, 255)
        call set_edge(model, 3, 3, 4, 190, 80, 255)
        call set_edge(model, 4, 4, 5, 190, 80, 255)
        call set_edge(model, 5, 5, 6, 190, 80, 255)
        call set_edge(model, 6, 6, 7, 190, 80, 255)
        call set_edge(model, 7, 7, 8, 190, 80, 255)
        call set_edge(model, 8, 8, 1, 190, 80, 255)
        call set_edge(model, 9, 1, 9, 255, 255, 255)
        call set_edge(model, 10, 3, 9, 255, 255, 255)
        call set_edge(model, 11, 5, 9, 255, 255, 255)
        call set_edge(model, 12, 7, 9, 255, 255, 255)
        call set_edge(model, 13, 1, 10, 180, 255, 90)
        call set_edge(model, 14, 3, 10, 180, 255, 90)
        call set_edge(model, 15, 5, 10, 180, 255, 90)
        call set_edge(model, 16, 7, 10, 180, 255, 90)
        call set_edge(model, 17, 10, 11, 255, 80, 80)
        call set_edge(model, 18, 10, 12, 255, 80, 80)
        call set_edge(model, 19, 10, 13, 255, 80, 80)
        call set_edge(model, 20, 10, 14, 255, 80, 80)
        call set_edge(model, 21, 2, 11, 255, 210, 60)
        call set_edge(model, 22, 6, 12, 255, 210, 60)
        call set_edge(model, 23, 8, 13, 255, 210, 60)
        call set_edge(model, 24, 4, 14, 255, 210, 60)
    end subroutine build_warden_model

    subroutine build_harrower_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "boss_harrower", 18, 32, 2.9_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  0.00_rk,  1.80_rk), &
            vec3(-2.40_rk,  0.00_rk,  0.40_rk), &
            vec3( 2.40_rk,  0.00_rk,  0.40_rk), &
            vec3( 0.00_rk,  0.90_rk,  0.10_rk), &
            vec3( 0.00_rk, -0.80_rk,  0.10_rk), &
            vec3( 0.00_rk,  0.00_rk, -1.40_rk), &
            vec3(-1.70_rk,  0.35_rk, -1.10_rk), &
            vec3( 1.70_rk,  0.35_rk, -1.10_rk), &
            vec3(-1.30_rk, -0.55_rk, -1.00_rk), &
            vec3( 1.30_rk, -0.55_rk, -1.00_rk), &
            vec3( 0.00_rk,  0.35_rk,  0.75_rk), &
            vec3(-0.85_rk, -0.25_rk,  1.15_rk), &
            vec3( 0.85_rk, -0.25_rk,  1.15_rk), &
            vec3(-1.70_rk, -0.10_rk,  0.60_rk), &
            vec3( 1.70_rk, -0.10_rk,  0.60_rk), &
            vec3(-2.10_rk, -0.10_rk, -0.90_rk), &
            vec3( 2.10_rk, -0.10_rk, -0.90_rk), &
            vec3( 0.00_rk,  0.00_rk,  0.10_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 255, 70, 80)
        call set_edge(model, 2, 1, 3, 255, 70, 80)
        call set_edge(model, 3, 1, 4, 255, 180, 40)
        call set_edge(model, 4, 1, 5, 255, 180, 40)
        call set_edge(model, 5, 2, 7, 255, 40, 160)
        call set_edge(model, 6, 3, 8, 255, 40, 160)
        call set_edge(model, 7, 7, 6, 255, 70, 80)
        call set_edge(model, 8, 8, 6, 255, 70, 80)
        call set_edge(model, 9, 9, 6, 255, 120, 40)
        call set_edge(model, 10, 10, 6, 255, 120, 40)
        call set_edge(model, 11, 2, 9, 255, 40, 160)
        call set_edge(model, 12, 3, 10, 255, 40, 160)
        call set_edge(model, 13, 4, 7, 255, 210, 80)
        call set_edge(model, 14, 4, 8, 255, 210, 80)
        call set_edge(model, 15, 5, 9, 255, 210, 80)
        call set_edge(model, 16, 5, 10, 255, 210, 80)
        call set_edge(model, 17, 7, 8, 255, 100, 180)
        call set_edge(model, 18, 9, 10, 255, 100, 180)
        call set_edge(model, 19, 11, 1, 255, 255, 120)
        call set_edge(model, 20, 11, 4, 255, 255, 120)
        call set_edge(model, 21, 12, 1, 255, 255, 255)
        call set_edge(model, 22, 13, 1, 255, 255, 255)
        call set_edge(model, 23, 12, 14, 255, 180, 40)
        call set_edge(model, 24, 13, 15, 255, 180, 40)
        call set_edge(model, 25, 14, 16, 255, 70, 80)
        call set_edge(model, 26, 15, 17, 255, 70, 80)
        call set_edge(model, 27, 16, 7, 255, 40, 160)
        call set_edge(model, 28, 17, 8, 255, 40, 160)
        call set_edge(model, 29, 14, 2, 255, 255, 120)
        call set_edge(model, 30, 15, 3, 255, 255, 120)
        call set_edge(model, 31, 18, 4, 255, 120, 40)
        call set_edge(model, 32, 18, 5, 255, 120, 40)
    end subroutine build_harrower_model

    subroutine build_seer_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "boss_seer", 16, 32, 2.25_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  1.50_rk,  0.00_rk), &
            vec3( 0.00_rk, -1.50_rk,  0.00_rk), &
            vec3(-1.35_rk,  0.00_rk,  0.00_rk), &
            vec3( 1.35_rk,  0.00_rk,  0.00_rk), &
            vec3( 0.00_rk,  0.00_rk,  1.15_rk), &
            vec3( 0.00_rk,  0.00_rk, -1.15_rk), &
            vec3( 0.00_rk,  0.00_rk,  0.00_rk), &
            vec3(-1.25_rk,  0.00_rk,  0.65_rk), &
            vec3( 0.00_rk,  1.05_rk,  0.65_rk), &
            vec3( 1.25_rk,  0.00_rk,  0.65_rk), &
            vec3( 0.00_rk, -1.05_rk,  0.65_rk), &
            vec3(-1.05_rk,  0.00_rk, -0.75_rk), &
            vec3( 0.00_rk,  0.90_rk, -0.75_rk), &
            vec3( 1.05_rk,  0.00_rk, -0.75_rk), &
            vec3( 0.00_rk, -0.90_rk, -0.75_rk), &
            vec3( 0.00_rk,  0.00_rk,  1.55_rk)  &
        ]

        call set_edge(model, 1, 1, 3, 190, 90, 255)
        call set_edge(model, 2, 1, 4, 190, 90, 255)
        call set_edge(model, 3, 1, 5, 255, 255, 255)
        call set_edge(model, 4, 1, 6, 140, 255, 220)
        call set_edge(model, 5, 2, 3, 190, 90, 255)
        call set_edge(model, 6, 2, 4, 190, 90, 255)
        call set_edge(model, 7, 2, 5, 255, 255, 255)
        call set_edge(model, 8, 2, 6, 140, 255, 220)
        call set_edge(model, 9, 3, 5, 210, 110, 255)
        call set_edge(model, 10, 5, 4, 210, 110, 255)
        call set_edge(model, 11, 4, 6, 210, 110, 255)
        call set_edge(model, 12, 6, 3, 210, 110, 255)
        call set_edge(model, 13, 7, 1, 255, 255, 255)
        call set_edge(model, 14, 7, 2, 255, 255, 255)
        call set_edge(model, 15, 7, 3, 120, 255, 200)
        call set_edge(model, 16, 7, 4, 120, 255, 200)
        call set_edge(model, 17, 7, 5, 255, 255, 255)
        call set_edge(model, 18, 7, 6, 120, 255, 200)
        call set_edge(model, 19, 8, 9, 120, 255, 200)
        call set_edge(model, 20, 9, 10, 120, 255, 200)
        call set_edge(model, 21, 10, 11, 120, 255, 200)
        call set_edge(model, 22, 11, 8, 120, 255, 200)
        call set_edge(model, 23, 12, 13, 190, 90, 255)
        call set_edge(model, 24, 13, 14, 190, 90, 255)
        call set_edge(model, 25, 14, 15, 190, 90, 255)
        call set_edge(model, 26, 15, 12, 190, 90, 255)
        call set_edge(model, 27, 8, 12, 255, 255, 255)
        call set_edge(model, 28, 9, 13, 255, 255, 255)
        call set_edge(model, 29, 10, 14, 255, 255, 255)
        call set_edge(model, 30, 11, 15, 255, 255, 255)
        call set_edge(model, 31, 16, 8, 255, 255, 255)
        call set_edge(model, 32, 16, 10, 255, 255, 255)
    end subroutine build_seer_model

    subroutine build_maw_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "boss_maw", 21, 40, 2.45_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  1.80_rk,  0.20_rk), &
            vec3( 1.25_rk,  1.25_rk,  0.20_rk), &
            vec3( 1.80_rk,  0.00_rk,  0.20_rk), &
            vec3( 1.25_rk, -1.25_rk,  0.20_rk), &
            vec3( 0.00_rk, -1.80_rk,  0.20_rk), &
            vec3(-1.25_rk, -1.25_rk,  0.20_rk), &
            vec3(-1.80_rk,  0.00_rk,  0.20_rk), &
            vec3(-1.25_rk,  1.25_rk,  0.20_rk), &
            vec3( 0.00_rk,  0.85_rk,  1.00_rk), &
            vec3( 0.85_rk,  0.00_rk,  1.00_rk), &
            vec3( 0.00_rk, -0.85_rk,  1.00_rk), &
            vec3(-0.85_rk,  0.00_rk,  1.00_rk), &
            vec3( 0.00_rk,  1.20_rk, -1.00_rk), &
            vec3( 1.20_rk,  0.00_rk, -1.00_rk), &
            vec3( 0.00_rk, -1.20_rk, -1.00_rk), &
            vec3(-1.20_rk,  0.00_rk, -1.00_rk), &
            vec3( 0.00_rk,  0.00_rk,  1.60_rk), &
            vec3( 0.00_rk,  0.45_rk,  1.35_rk), &
            vec3( 0.45_rk,  0.00_rk,  1.35_rk), &
            vec3( 0.00_rk, -0.45_rk,  1.35_rk), &
            vec3(-0.45_rk,  0.00_rk,  1.35_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 255, 80, 40)
        call set_edge(model, 2, 2, 3, 255, 80, 40)
        call set_edge(model, 3, 3, 4, 255, 80, 40)
        call set_edge(model, 4, 4, 5, 255, 80, 40)
        call set_edge(model, 5, 5, 6, 255, 80, 40)
        call set_edge(model, 6, 6, 7, 255, 80, 40)
        call set_edge(model, 7, 7, 8, 255, 80, 40)
        call set_edge(model, 8, 8, 1, 255, 80, 40)
        call set_edge(model, 9, 9, 10, 255, 220, 80)
        call set_edge(model, 10, 10, 11, 255, 220, 80)
        call set_edge(model, 11, 11, 12, 255, 220, 80)
        call set_edge(model, 12, 12, 9, 255, 220, 80)
        call set_edge(model, 13, 13, 14, 255, 110, 130)
        call set_edge(model, 14, 14, 15, 255, 110, 130)
        call set_edge(model, 15, 15, 16, 255, 110, 130)
        call set_edge(model, 16, 16, 13, 255, 110, 130)
        call set_edge(model, 17, 1, 9, 255, 180, 40)
        call set_edge(model, 18, 2, 10, 255, 180, 40)
        call set_edge(model, 19, 3, 10, 255, 180, 40)
        call set_edge(model, 20, 4, 11, 255, 180, 40)
        call set_edge(model, 21, 5, 11, 255, 180, 40)
        call set_edge(model, 22, 6, 12, 255, 180, 40)
        call set_edge(model, 23, 7, 12, 255, 180, 40)
        call set_edge(model, 24, 8, 9, 255, 180, 40)
        call set_edge(model, 25, 1, 13, 255, 80, 40)
        call set_edge(model, 26, 3, 14, 255, 80, 40)
        call set_edge(model, 27, 5, 15, 255, 80, 40)
        call set_edge(model, 28, 7, 16, 255, 80, 40)
        call set_edge(model, 29, 9, 13, 255, 220, 80)
        call set_edge(model, 30, 10, 14, 255, 220, 80)
        call set_edge(model, 31, 11, 15, 255, 220, 80)
        call set_edge(model, 32, 12, 16, 255, 220, 80)
        call set_edge(model, 33, 17, 18, 255, 255, 255)
        call set_edge(model, 34, 17, 19, 255, 255, 255)
        call set_edge(model, 35, 17, 20, 255, 255, 255)
        call set_edge(model, 36, 17, 21, 255, 255, 255)
        call set_edge(model, 37, 18, 9, 255, 255, 120)
        call set_edge(model, 38, 19, 10, 255, 255, 120)
        call set_edge(model, 39, 20, 11, 255, 255, 120)
        call set_edge(model, 40, 21, 12, 255, 255, 120)
    end subroutine build_maw_model

    subroutine build_gate_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "depth_gate", 8, 12, 3.1_rk)

        model%vertices = [ &
            vec3(-2.20_rk, -1.20_rk,  0.0_rk), &
            vec3( 2.20_rk, -1.20_rk,  0.0_rk), &
            vec3( 2.20_rk,  1.20_rk,  0.0_rk), &
            vec3(-2.20_rk,  1.20_rk,  0.0_rk), &
            vec3(-1.50_rk, -0.72_rk,  0.55_rk), &
            vec3( 1.50_rk, -0.72_rk,  0.55_rk), &
            vec3( 1.50_rk,  0.72_rk,  0.55_rk), &
            vec3(-1.50_rk,  0.72_rk,  0.55_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 0, 140, 255)
        call set_edge(model, 2, 2, 3, 0, 140, 255)
        call set_edge(model, 3, 3, 4, 0, 140, 255)
        call set_edge(model, 4, 4, 1, 0, 140, 255)
        call set_edge(model, 5, 5, 6, 0, 255, 190)
        call set_edge(model, 6, 6, 7, 0, 255, 190)
        call set_edge(model, 7, 7, 8, 0, 255, 190)
        call set_edge(model, 8, 8, 5, 0, 255, 190)
        call set_edge(model, 9, 1, 5, 0, 110, 255)
        call set_edge(model, 10, 2, 6, 0, 110, 255)
        call set_edge(model, 11, 3, 7, 0, 110, 255)
        call set_edge(model, 12, 4, 8, 0, 110, 255)
    end subroutine build_gate_model

    subroutine build_shield_gate_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "shield_gate", 12, 18, 2.8_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  1.60_rk,  0.0_rk), &
            vec3( 0.95_rk,  1.15_rk,  0.0_rk), &
            vec3( 1.55_rk,  0.38_rk,  0.0_rk), &
            vec3( 1.35_rk, -0.70_rk,  0.0_rk), &
            vec3( 0.48_rk, -1.42_rk,  0.0_rk), &
            vec3(-0.48_rk, -1.42_rk,  0.0_rk), &
            vec3(-1.35_rk, -0.70_rk,  0.0_rk), &
            vec3(-1.55_rk,  0.38_rk,  0.0_rk), &
            vec3(-0.95_rk,  1.15_rk,  0.0_rk), &
            vec3( 0.00_rk,  0.82_rk,  0.42_rk), &
            vec3( 0.70_rk, -0.45_rk,  0.42_rk), &
            vec3(-0.70_rk, -0.45_rk,  0.42_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 40, 255, 130)
        call set_edge(model, 2, 2, 3, 40, 255, 130)
        call set_edge(model, 3, 3, 4, 40, 255, 130)
        call set_edge(model, 4, 4, 5, 40, 255, 130)
        call set_edge(model, 5, 5, 6, 40, 255, 130)
        call set_edge(model, 6, 6, 7, 40, 255, 130)
        call set_edge(model, 7, 7, 8, 40, 255, 130)
        call set_edge(model, 8, 8, 9, 40, 255, 130)
        call set_edge(model, 9, 9, 1, 40, 255, 130)
        call set_edge(model, 10, 10, 11, 255, 255, 120)
        call set_edge(model, 11, 11, 12, 255, 255, 120)
        call set_edge(model, 12, 12, 10, 255, 255, 120)
        call set_edge(model, 13, 1, 10, 40, 180, 255)
        call set_edge(model, 14, 3, 11, 40, 180, 255)
        call set_edge(model, 15, 5, 11, 40, 180, 255)
        call set_edge(model, 16, 6, 12, 40, 180, 255)
        call set_edge(model, 17, 8, 12, 40, 180, 255)
        call set_edge(model, 18, 9, 10, 40, 180, 255)
    end subroutine build_shield_gate_model

    subroutine build_cube_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "test_cube", 8, 12, 1.8_rk)

        model%vertices = [ &
            vec3(-1.0_rk, -1.0_rk, -1.0_rk), &
            vec3( 1.0_rk, -1.0_rk, -1.0_rk), &
            vec3( 1.0_rk,  1.0_rk, -1.0_rk), &
            vec3(-1.0_rk,  1.0_rk, -1.0_rk), &
            vec3(-1.0_rk, -1.0_rk,  1.0_rk), &
            vec3( 1.0_rk, -1.0_rk,  1.0_rk), &
            vec3( 1.0_rk,  1.0_rk,  1.0_rk), &
            vec3(-1.0_rk,  1.0_rk,  1.0_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 0, 255, 255)
        call set_edge(model, 2, 2, 3, 0, 255, 255)
        call set_edge(model, 3, 3, 4, 0, 255, 255)
        call set_edge(model, 4, 4, 1, 0, 255, 255)
        call set_edge(model, 5, 5, 6, 255, 255, 0)
        call set_edge(model, 6, 6, 7, 255, 255, 0)
        call set_edge(model, 7, 7, 8, 255, 255, 0)
        call set_edge(model, 8, 8, 5, 255, 255, 0)
        call set_edge(model, 9, 1, 5, 255, 80, 255)
        call set_edge(model, 10, 2, 6, 255, 80, 255)
        call set_edge(model, 11, 3, 7, 255, 80, 255)
        call set_edge(model, 12, 4, 8, 255, 80, 255)
    end subroutine build_cube_model

    subroutine build_buoy_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "picket_buoy", 6, 12, 1.1_rk)

        model%vertices = [ &
            vec3( 1.00_rk,  0.00_rk,  0.00_rk), &
            vec3(-1.00_rk,  0.00_rk,  0.00_rk), &
            vec3( 0.00_rk,  1.00_rk,  0.00_rk), &
            vec3( 0.00_rk, -1.00_rk,  0.00_rk), &
            vec3( 0.00_rk,  0.00_rk,  1.00_rk), &
            vec3( 0.00_rk,  0.00_rk, -1.00_rk)  &
        ]

        call set_edge(model, 1, 1, 3, 140, 220, 255)
        call set_edge(model, 2, 3, 2, 140, 220, 255)
        call set_edge(model, 3, 2, 4, 140, 220, 255)
        call set_edge(model, 4, 4, 1, 140, 220, 255)
        call set_edge(model, 5, 1, 5, 200, 240, 255)
        call set_edge(model, 6, 3, 5, 200, 240, 255)
        call set_edge(model, 7, 2, 5, 200, 240, 255)
        call set_edge(model, 8, 4, 5, 200, 240, 255)
        call set_edge(model, 9, 1, 6, 80, 160, 220)
        call set_edge(model, 10, 3, 6, 80, 160, 220)
        call set_edge(model, 11, 2, 6, 80, 160, 220)
        call set_edge(model, 12, 4, 6, 80, 160, 220)
    end subroutine build_buoy_model

    subroutine build_shard_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "asteroid_shard", 6, 12, 1.3_rk)

        model%vertices = [ &
            vec3( 1.15_rk,  0.22_rk,  0.10_rk), &
            vec3(-0.90_rk, -0.18_rk,  0.30_rk), &
            vec3( 0.28_rk,  1.10_rk, -0.22_rk), &
            vec3(-0.22_rk, -1.18_rk,  0.12_rk), &
            vec3( 0.18_rk,  0.14_rk,  1.08_rk), &
            vec3(-0.12_rk,  0.22_rk, -1.22_rk)  &
        ]

        call set_edge(model, 1, 1, 3, 220, 80, 200)
        call set_edge(model, 2, 3, 2, 200, 100, 220)
        call set_edge(model, 3, 2, 4, 220, 80, 200)
        call set_edge(model, 4, 4, 1, 180, 60, 180)
        call set_edge(model, 5, 1, 5, 255, 140, 220)
        call set_edge(model, 6, 3, 5, 255, 140, 220)
        call set_edge(model, 7, 2, 5, 220, 120, 220)
        call set_edge(model, 8, 4, 5, 220, 120, 220)
        call set_edge(model, 9, 1, 6, 160, 40, 160)
        call set_edge(model, 10, 3, 6, 160, 40, 160)
        call set_edge(model, 11, 2, 6, 140, 30, 140)
        call set_edge(model, 12, 4, 6, 140, 30, 140)
    end subroutine build_shard_model

    subroutine build_spine_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "gate_spine", 6, 10, 1.9_rk)

        model%vertices = [ &
            vec3( 0.00_rk, -1.20_rk,  0.00_rk), &
            vec3( 0.00_rk,  2.80_rk,  0.00_rk), &
            vec3(-0.55_rk, -0.80_rk, -0.25_rk), &
            vec3( 0.55_rk, -0.80_rk, -0.25_rk), &
            vec3(-0.28_rk,  1.50_rk, -0.12_rk), &
            vec3( 0.28_rk,  1.50_rk, -0.12_rk)  &
        ]

        call set_edge(model, 1, 1, 3, 255, 140, 60)
        call set_edge(model, 2, 1, 4, 255, 140, 60)
        call set_edge(model, 3, 3, 4, 255, 100, 40)
        call set_edge(model, 4, 3, 5, 255, 180, 60)
        call set_edge(model, 5, 4, 6, 255, 180, 60)
        call set_edge(model, 6, 5, 6, 255, 200, 100)
        call set_edge(model, 7, 5, 2, 255, 220, 140)
        call set_edge(model, 8, 6, 2, 255, 220, 140)
        call set_edge(model, 9, 3, 2, 255, 160, 80)
        call set_edge(model, 10, 4, 2, 255, 160, 80)
    end subroutine build_spine_model

    subroutine build_rocket_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "coil_lance", 6, 10, 0.45_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  0.00_rk,  1.20_rk), &
            vec3(-0.18_rk,  0.00_rk,  0.30_rk), &
            vec3( 0.18_rk,  0.00_rk,  0.30_rk), &
            vec3( 0.00_rk,  0.18_rk,  0.30_rk), &
            vec3( 0.00_rk, -0.18_rk,  0.30_rk), &
            vec3( 0.00_rk,  0.00_rk, -0.60_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 255, 120, 60)
        call set_edge(model, 2, 1, 3, 255, 120, 60)
        call set_edge(model, 3, 1, 4, 255, 160, 80)
        call set_edge(model, 4, 1, 5, 255, 160, 80)
        call set_edge(model, 5, 2, 3, 220, 90, 40)
        call set_edge(model, 6, 4, 5, 220, 90, 40)
        call set_edge(model, 7, 2, 6, 255, 200, 120)
        call set_edge(model, 8, 3, 6, 255, 200, 120)
        call set_edge(model, 9, 4, 6, 255, 220, 140)
        call set_edge(model, 10, 5, 6, 255, 220, 140)
    end subroutine build_rocket_model

    subroutine build_lancer_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "coil_lancer", 9, 14, 1.35_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  0.00_rk,  1.30_rk), &
            vec3(-0.55_rk,  0.00_rk,  0.10_rk), &
            vec3( 0.55_rk,  0.00_rk,  0.10_rk), &
            vec3( 0.00_rk,  0.38_rk,  0.00_rk), &
            vec3( 0.00_rk, -0.38_rk,  0.00_rk), &
            vec3(-0.80_rk,  0.00_rk, -0.85_rk), &
            vec3( 0.80_rk,  0.00_rk, -0.85_rk), &
            vec3(-0.80_rk,  0.00_rk, -1.25_rk), &
            vec3( 0.80_rk,  0.00_rk, -1.25_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 180, 60, 200)
        call set_edge(model, 2, 1, 3, 180, 60, 200)
        call set_edge(model, 3, 1, 4, 220, 100, 220)
        call set_edge(model, 4, 1, 5, 220, 100, 220)
        call set_edge(model, 5, 2, 4, 160, 50, 180)
        call set_edge(model, 6, 3, 4, 160, 50, 180)
        call set_edge(model, 7, 2, 5, 160, 50, 180)
        call set_edge(model, 8, 3, 5, 160, 50, 180)
        call set_edge(model, 9, 2, 6, 150, 50, 160)
        call set_edge(model, 10, 3, 7, 150, 50, 160)
        call set_edge(model, 11, 6, 8, 255, 60, 40)
        call set_edge(model, 12, 7, 9, 255, 60, 40)
        call set_edge(model, 13, 6, 7, 120, 40, 140)
        call set_edge(model, 14, 8, 9, 200, 60, 60)
    end subroutine build_lancer_model

    subroutine build_wreck_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "hull_wreck", 10, 16, 2.2_rk)

        model%vertices = [ &
            vec3(-1.30_rk, -0.95_rk,  1.40_rk), &
            vec3( 1.40_rk, -0.85_rk,  1.20_rk), &
            vec3( 1.70_rk,  0.90_rk, -0.10_rk), &
            vec3(-0.80_rk,  1.10_rk,  0.30_rk), &
            vec3(-1.50_rk,  0.10_rk, -1.20_rk), &
            vec3( 1.10_rk,  0.20_rk, -1.60_rk), &
            vec3(-0.20_rk, -1.10_rk, -0.60_rk), &
            vec3( 0.30_rk,  0.40_rk,  0.50_rk), &
            vec3(-0.60_rk, -0.40_rk,  0.80_rk), &
            vec3( 0.80_rk, -0.20_rk, -0.70_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 200, 110, 60)
        call set_edge(model, 2, 2, 3, 200, 110, 60)
        call set_edge(model, 3, 3, 4, 200, 110, 60)
        call set_edge(model, 4, 4, 1, 200, 110, 60)
        call set_edge(model, 5, 1, 5, 160, 90, 50)
        call set_edge(model, 6, 2, 6, 160, 90, 50)
        call set_edge(model, 7, 3, 6, 160, 90, 50)
        call set_edge(model, 8, 4, 5, 160, 90, 50)
        call set_edge(model, 9, 5, 7, 120, 70, 40)
        call set_edge(model, 10, 6, 7, 120, 70, 40)
        call set_edge(model, 11, 1, 9, 180, 100, 60)
        call set_edge(model, 12, 2, 10, 180, 100, 60)
        call set_edge(model, 13, 8, 9, 220, 140, 90)
        call set_edge(model, 14, 8, 10, 220, 140, 90)
        call set_edge(model, 15, 5, 6, 90, 60, 40)
        call set_edge(model, 16, 7, 8, 100, 70, 50)
    end subroutine build_wreck_model

    subroutine build_arc_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "void_arc", 6, 8, 3.0_rk)

        model%vertices = [ &
            vec3(-2.50_rk,  0.00_rk,  0.00_rk), &
            vec3(-1.20_rk,  0.25_rk,  0.10_rk), &
            vec3( 0.10_rk, -0.20_rk, -0.12_rk), &
            vec3( 1.20_rk,  0.18_rk,  0.08_rk), &
            vec3( 2.50_rk, -0.05_rk,  0.00_rk), &
            vec3( 0.00_rk,  0.00_rk,  0.00_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 220, 240, 255)
        call set_edge(model, 2, 2, 3, 220, 240, 255)
        call set_edge(model, 3, 3, 4, 220, 240, 255)
        call set_edge(model, 4, 4, 5, 220, 240, 255)
        call set_edge(model, 5, 1, 6, 120, 200, 255)
        call set_edge(model, 6, 5, 6, 120, 200, 255)
        call set_edge(model, 7, 2, 6, 160, 220, 255)
        call set_edge(model, 8, 4, 6, 160, 220, 255)
    end subroutine build_arc_model

    subroutine build_lattice_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "coil_lattice", 8, 12, 1.3_rk)

        model%vertices = [ &
            vec3(-1.20_rk, -0.60_rk,  0.80_rk), &
            vec3( 1.20_rk, -0.60_rk,  0.80_rk), &
            vec3( 1.20_rk,  0.60_rk,  0.80_rk), &
            vec3(-1.20_rk,  0.60_rk,  0.80_rk), &
            vec3(-1.20_rk, -0.60_rk, -0.80_rk), &
            vec3( 1.20_rk, -0.60_rk, -0.80_rk), &
            vec3( 1.20_rk,  0.60_rk, -0.80_rk), &
            vec3(-1.20_rk,  0.60_rk, -0.80_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 200, 50, 50)
        call set_edge(model, 2, 3, 4, 200, 50, 50)
        call set_edge(model, 3, 5, 6, 180, 40, 40)
        call set_edge(model, 4, 7, 8, 180, 40, 40)
        call set_edge(model, 5, 1, 5, 220, 60, 60)
        call set_edge(model, 6, 2, 6, 220, 60, 60)
        call set_edge(model, 7, 3, 7, 220, 60, 60)
        call set_edge(model, 8, 4, 8, 220, 60, 60)
        call set_edge(model, 9, 1, 4, 140, 30, 30)
        call set_edge(model, 10, 2, 3, 140, 30, 30)
        call set_edge(model, 11, 5, 8, 120, 30, 30)
        call set_edge(model, 12, 6, 7, 120, 30, 30)
    end subroutine build_lattice_model

    subroutine build_boneforge_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "boneforge", 11, 18, 2.9_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  0.00_rk,  1.80_rk), &
            vec3(-1.80_rk,  0.90_rk,  0.20_rk), &
            vec3( 1.80_rk,  0.90_rk,  0.20_rk), &
            vec3(-1.80_rk, -0.90_rk,  0.20_rk), &
            vec3( 1.80_rk, -0.90_rk,  0.20_rk), &
            vec3( 0.00_rk,  1.80_rk, -0.20_rk), &
            vec3( 0.00_rk, -1.80_rk, -0.20_rk), &
            vec3(-1.20_rk,  0.00_rk, -1.60_rk), &
            vec3( 1.20_rk,  0.00_rk, -1.60_rk), &
            vec3( 0.00_rk,  0.00_rk, -2.20_rk), &
            vec3( 0.00_rk,  0.00_rk,  0.00_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 255, 180, 60)
        call set_edge(model, 2, 1, 3, 255, 180, 60)
        call set_edge(model, 3, 1, 4, 255, 180, 60)
        call set_edge(model, 4, 1, 5, 255, 180, 60)
        call set_edge(model, 5, 2, 6, 220, 140, 40)
        call set_edge(model, 6, 3, 6, 220, 140, 40)
        call set_edge(model, 7, 4, 7, 220, 140, 40)
        call set_edge(model, 8, 5, 7, 220, 140, 40)
        call set_edge(model, 9, 2, 4, 180, 100, 40)
        call set_edge(model, 10, 3, 5, 180, 100, 40)
        call set_edge(model, 11, 2, 8, 160, 80, 40)
        call set_edge(model, 12, 4, 8, 160, 80, 40)
        call set_edge(model, 13, 3, 9, 160, 80, 40)
        call set_edge(model, 14, 5, 9, 160, 80, 40)
        call set_edge(model, 15, 8, 10, 120, 60, 30)
        call set_edge(model, 16, 9, 10, 120, 60, 30)
        call set_edge(model, 17, 11, 6, 255, 220, 100)
        call set_edge(model, 18, 11, 7, 255, 220, 100)
    end subroutine build_boneforge_model

    subroutine build_stormveil_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "stormveil", 8, 14, 2.4_rk)

        model%vertices = [ &
            vec3( 0.00_rk,  2.20_rk,  0.00_rk), &
            vec3( 0.00_rk, -2.20_rk,  0.00_rk), &
            vec3( 0.00_rk,  0.00_rk,  1.20_rk), &
            vec3( 0.00_rk,  0.00_rk, -1.20_rk), &
            vec3(-0.85_rk,  1.10_rk, -0.10_rk), &
            vec3( 0.85_rk,  1.10_rk, -0.10_rk), &
            vec3(-0.85_rk, -1.10_rk, -0.10_rk), &
            vec3( 0.85_rk, -1.10_rk, -0.10_rk)  &
        ]

        call set_edge(model, 1, 1, 3, 220, 240, 255)
        call set_edge(model, 2, 1, 4, 220, 240, 255)
        call set_edge(model, 3, 2, 3, 220, 240, 255)
        call set_edge(model, 4, 2, 4, 220, 240, 255)
        call set_edge(model, 5, 1, 5, 180, 220, 255)
        call set_edge(model, 6, 1, 6, 180, 220, 255)
        call set_edge(model, 7, 2, 7, 180, 220, 255)
        call set_edge(model, 8, 2, 8, 180, 220, 255)
        call set_edge(model, 9, 3, 5, 255, 255, 255)
        call set_edge(model, 10, 3, 6, 255, 255, 255)
        call set_edge(model, 11, 4, 7, 140, 180, 220)
        call set_edge(model, 12, 4, 8, 140, 180, 220)
        call set_edge(model, 13, 5, 7, 120, 160, 200)
        call set_edge(model, 14, 6, 8, 120, 160, 200)
    end subroutine build_stormveil_model

    subroutine build_maw_core_model(model)
        type(wire_model), intent(out) :: model
        integer :: i
        integer :: edge_idx
        real(rk) :: angle
        real(rk) :: r_outer
        real(rk) :: r_mid

        call allocate_model(model, "maw_core", 13, 24, 3.0_rk)

        r_outer = 2.10_rk
        r_mid = 1.30_rk
        model%vertices(1) = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        do i = 0, 5
            angle = real(i, rk) * pi / 3.0_rk
            model%vertices(2 + i) = vec3(r_outer * cos(angle), r_outer * sin(angle), 0.0_rk)
        end do
        do i = 0, 5
            angle = real(i, rk) * pi / 3.0_rk + pi / 6.0_rk
            model%vertices(8 + i) = vec3(r_mid * cos(angle), r_mid * sin(angle), -0.35_rk)
        end do

        edge_idx = 0
        do i = 0, 5
            edge_idx = edge_idx + 1
            call set_edge(model, edge_idx, 2 + i, 2 + mod(i + 1, 6), 255, 60, 40)
        end do
        do i = 0, 5
            edge_idx = edge_idx + 1
            call set_edge(model, edge_idx, 8 + i, 8 + mod(i + 1, 6), 220, 80, 80)
        end do
        do i = 0, 5
            edge_idx = edge_idx + 1
            call set_edge(model, edge_idx, 2 + i, 8 + i, 180, 50, 50)
        end do
        do i = 0, 5
            edge_idx = edge_idx + 1
            call set_edge(model, edge_idx, 8 + i, 1, 255, 140, 80)
        end do
    end subroutine build_maw_core_model

    subroutine build_cage_model(model)
        type(wire_model), intent(out) :: model

        call allocate_model(model, "juggernaut_cage", 8, 12, 1.75_rk)

        model%vertices = [ &
            vec3(-1.20_rk, -0.95_rk, -1.05_rk), &
            vec3( 1.20_rk, -0.95_rk, -1.05_rk), &
            vec3( 1.20_rk,  0.95_rk, -1.05_rk), &
            vec3(-1.20_rk,  0.95_rk, -1.05_rk), &
            vec3(-1.20_rk, -0.95_rk,  1.25_rk), &
            vec3( 1.20_rk, -0.95_rk,  1.25_rk), &
            vec3( 1.20_rk,  0.95_rk,  1.25_rk), &
            vec3(-1.20_rk,  0.95_rk,  1.25_rk)  &
        ]

        call set_edge(model, 1, 1, 2, 120, 220, 80)
        call set_edge(model, 2, 2, 3, 120, 220, 80)
        call set_edge(model, 3, 3, 4, 120, 220, 80)
        call set_edge(model, 4, 4, 1, 120, 220, 80)
        call set_edge(model, 5, 5, 6, 160, 240, 120)
        call set_edge(model, 6, 6, 7, 160, 240, 120)
        call set_edge(model, 7, 7, 8, 160, 240, 120)
        call set_edge(model, 8, 8, 5, 160, 240, 120)
        call set_edge(model, 9, 1, 5, 80, 200, 60)
        call set_edge(model, 10, 2, 6, 80, 200, 60)
        call set_edge(model, 11, 3, 7, 80, 200, 60)
        call set_edge(model, 12, 4, 8, 80, 200, 60)
    end subroutine build_cage_model

    subroutine build_pickup_model(model, r, g, bcol)
        type(wire_model), intent(out) :: model
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: bcol
        integer :: r_lo
        integer :: g_lo
        integer :: b_lo

        call allocate_model(model, "lattice_shard", 6, 12, 0.45_rk)

        model%vertices = [ &
            vec3( 0.55_rk,  0.00_rk,  0.00_rk), &
            vec3(-0.55_rk,  0.00_rk,  0.00_rk), &
            vec3( 0.00_rk,  0.55_rk,  0.00_rk), &
            vec3( 0.00_rk, -0.55_rk,  0.00_rk), &
            vec3( 0.00_rk,  0.00_rk,  0.55_rk), &
            vec3( 0.00_rk,  0.00_rk, -0.55_rk)  &
        ]

        r_lo = max(0, r - 70)
        g_lo = max(0, g - 70)
        b_lo = max(0, bcol - 70)

        call set_edge(model, 1, 1, 3, r, g, bcol)
        call set_edge(model, 2, 3, 2, r, g, bcol)
        call set_edge(model, 3, 2, 4, r, g, bcol)
        call set_edge(model, 4, 4, 1, r, g, bcol)
        call set_edge(model, 5, 1, 5, r, g, bcol)
        call set_edge(model, 6, 3, 5, r, g, bcol)
        call set_edge(model, 7, 2, 5, r, g, bcol)
        call set_edge(model, 8, 4, 5, r, g, bcol)
        call set_edge(model, 9, 1, 6, r_lo, g_lo, b_lo)
        call set_edge(model, 10, 3, 6, r_lo, g_lo, b_lo)
        call set_edge(model, 11, 2, 6, r_lo, g_lo, b_lo)
        call set_edge(model, 12, 4, 6, r_lo, g_lo, b_lo)
    end subroutine build_pickup_model

    subroutine append_model_lines(model, transform, camera, width, height, lines, line_count, max_lines, alpha, color_boost)
        type(wire_model), intent(in) :: model
        type(transform3), intent(in) :: transform
        type(camera3), intent(in) :: camera
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(screen_line), intent(inout) :: lines(:)
        integer, intent(inout) :: line_count
        integer, intent(in) :: max_lines
        integer, intent(in) :: alpha
        real(rk), intent(in), optional :: color_boost
        integer :: i
        type(vec3) :: p1
        type(vec3) :: p2
        type(vec2) :: s1
        type(vec2) :: s2
        real(rk) :: d1
        real(rk) :: d2
        real(rk) :: boost
        logical :: ok1
        logical :: ok2

        boost = 1.0_rk
        if (present(color_boost)) boost = color_boost

        do i = 1, model%edge_count
            if (line_count >= max_lines) exit

            p1 = apply_transform(model%vertices(model%edges(i)%a), transform)
            p2 = apply_transform(model%vertices(model%edges(i)%b), transform)
            ok1 = project_point(p1, camera, width, height, s1, d1)
            ok2 = project_point(p2, camera, width, height, s2, d2)
            if (.not. ok1 .or. .not. ok2) cycle

            line_count = line_count + 1
            lines(line_count)%x1 = nint(s1%x)
            lines(line_count)%y1 = nint(s1%y)
            lines(line_count)%x2 = nint(s2%x)
            lines(line_count)%y2 = nint(s2%y)
            lines(line_count)%r = clamp_color(nint(real(model%edges(i)%r, rk) * boost))
            lines(line_count)%g = clamp_color(nint(real(model%edges(i)%g, rk) * boost))
            lines(line_count)%b = clamp_color(nint(real(model%edges(i)%bcol, rk) * boost))
            lines(line_count)%a = clamp_color(alpha)
            lines(line_count)%depth = 0.5_rk * (d1 + d2)
        end do
    end subroutine append_model_lines

    subroutine allocate_model(model, name, vertex_count, edge_count, radius)
        type(wire_model), intent(out) :: model
        character(len=*), intent(in) :: name
        integer, intent(in) :: vertex_count
        integer, intent(in) :: edge_count
        real(rk), intent(in) :: radius

        model%name = name
        model%vertex_count = vertex_count
        model%edge_count = edge_count
        model%radius = radius
        allocate(model%vertices(vertex_count))
        allocate(model%edges(edge_count))
    end subroutine allocate_model

    subroutine set_edge(model, index, a, b, r, g, bcol)
        type(wire_model), intent(inout) :: model
        integer, intent(in) :: index
        integer, intent(in) :: a
        integer, intent(in) :: b
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: bcol

        model%edges(index)%a = a
        model%edges(index)%b = b
        model%edges(index)%r = r
        model%edges(index)%g = g
        model%edges(index)%bcol = bcol
    end subroutine set_edge

    pure integer function clamp_color(value)
        integer, intent(in) :: value
        clamp_color = max(0, min(255, value))
    end function clamp_color

end module model_library
