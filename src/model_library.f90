module model_library
    use vector_math, only: rk, vec2, vec3, transform3, camera3, apply_transform, project_point
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
    public :: build_gate_model
    public :: build_shield_gate_model
    public :: build_cube_model
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
