module vector_math
    use iso_fortran_env, only: real64
    implicit none
    private

    integer, parameter, public :: rk = real64
    real(rk), parameter, public :: pi = 3.1415926535897932384626433832795_rk

    type, public :: vec2
        real(rk) :: x = 0.0_rk
        real(rk) :: y = 0.0_rk
    end type vec2

    type, public :: vec3
        real(rk) :: x = 0.0_rk
        real(rk) :: y = 0.0_rk
        real(rk) :: z = 0.0_rk
    end type vec3

    type, public :: transform3
        type(vec3) :: position = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        type(vec3) :: rotation = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        real(rk) :: scale = 1.0_rk
    end type transform3

    type, public :: camera3
        type(vec3) :: position = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        real(rk) :: yaw = 0.0_rk
        real(rk) :: pitch = 0.0_rk
        real(rk) :: roll = 0.0_rk
        real(rk) :: focal_length = 0.92_rk
        real(rk) :: near_z = 0.12_rk
    end type camera3

    public :: make_vec2
    public :: make_vec3
    public :: add3
    public :: sub3
    public :: scale3
    public :: dot3
    public :: cross3
    public :: length3
    public :: normalize3
    public :: rotate_x
    public :: rotate_y
    public :: rotate_z
    public :: rotate_euler
    public :: apply_transform
    public :: world_to_camera
    public :: project_point
    public :: clamp
    public :: lerp

contains

    pure type(vec2) function make_vec2(x, y)
        real(rk), intent(in) :: x
        real(rk), intent(in) :: y
        make_vec2 = vec2(x, y)
    end function make_vec2

    pure type(vec3) function make_vec3(x, y, z)
        real(rk), intent(in) :: x
        real(rk), intent(in) :: y
        real(rk), intent(in) :: z
        make_vec3 = vec3(x, y, z)
    end function make_vec3

    pure type(vec3) function add3(a, b)
        type(vec3), intent(in) :: a
        type(vec3), intent(in) :: b
        add3 = vec3(a%x + b%x, a%y + b%y, a%z + b%z)
    end function add3

    pure type(vec3) function sub3(a, b)
        type(vec3), intent(in) :: a
        type(vec3), intent(in) :: b
        sub3 = vec3(a%x - b%x, a%y - b%y, a%z - b%z)
    end function sub3

    pure type(vec3) function scale3(a, s)
        type(vec3), intent(in) :: a
        real(rk), intent(in) :: s
        scale3 = vec3(a%x * s, a%y * s, a%z * s)
    end function scale3

    pure real(rk) function dot3(a, b)
        type(vec3), intent(in) :: a
        type(vec3), intent(in) :: b
        dot3 = a%x * b%x + a%y * b%y + a%z * b%z
    end function dot3

    pure type(vec3) function cross3(a, b)
        type(vec3), intent(in) :: a
        type(vec3), intent(in) :: b
        cross3 = vec3(a%y * b%z - a%z * b%y, &
                      a%z * b%x - a%x * b%z, &
                      a%x * b%y - a%y * b%x)
    end function cross3

    pure real(rk) function length3(a)
        type(vec3), intent(in) :: a
        length3 = sqrt(dot3(a, a))
    end function length3

    pure type(vec3) function normalize3(a)
        type(vec3), intent(in) :: a
        real(rk) :: len

        len = length3(a)
        if (len <= 1.0e-12_rk) then
            normalize3 = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        else
            normalize3 = scale3(a, 1.0_rk / len)
        end if
    end function normalize3

    pure type(vec3) function rotate_x(v, angle)
        type(vec3), intent(in) :: v
        real(rk), intent(in) :: angle
        real(rk) :: s
        real(rk) :: c

        s = sin(angle)
        c = cos(angle)
        rotate_x = vec3(v%x, v%y * c - v%z * s, v%y * s + v%z * c)
    end function rotate_x

    pure type(vec3) function rotate_y(v, angle)
        type(vec3), intent(in) :: v
        real(rk), intent(in) :: angle
        real(rk) :: s
        real(rk) :: c

        s = sin(angle)
        c = cos(angle)
        rotate_y = vec3(v%x * c + v%z * s, v%y, -v%x * s + v%z * c)
    end function rotate_y

    pure type(vec3) function rotate_z(v, angle)
        type(vec3), intent(in) :: v
        real(rk), intent(in) :: angle
        real(rk) :: s
        real(rk) :: c

        s = sin(angle)
        c = cos(angle)
        rotate_z = vec3(v%x * c - v%y * s, v%x * s + v%y * c, v%z)
    end function rotate_z

    pure type(vec3) function rotate_euler(v, rotation)
        type(vec3), intent(in) :: v
        type(vec3), intent(in) :: rotation
        type(vec3) :: r

        r = rotate_x(v, rotation%x)
        r = rotate_y(r, rotation%y)
        rotate_euler = rotate_z(r, rotation%z)
    end function rotate_euler

    pure type(vec3) function apply_transform(point, transform)
        type(vec3), intent(in) :: point
        type(transform3), intent(in) :: transform
        type(vec3) :: p

        p = scale3(point, transform%scale)
        p = rotate_euler(p, transform%rotation)
        apply_transform = add3(p, transform%position)
    end function apply_transform

    pure type(vec3) function world_to_camera(point, camera)
        type(vec3), intent(in) :: point
        type(camera3), intent(in) :: camera
        type(vec3) :: p

        p = sub3(point, camera%position)
        p = rotate_z(p, -camera%roll)
        p = rotate_x(p, -camera%pitch)
        world_to_camera = rotate_y(p, -camera%yaw)
    end function world_to_camera

    logical function project_point(point, camera, width, height, screen, depth, screen_scale)
        type(vec3), intent(in) :: point
        type(camera3), intent(in) :: camera
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(vec2), intent(out) :: screen
        real(rk), intent(out) :: depth
        real(rk), intent(out), optional :: screen_scale
        type(vec3) :: p
        real(rk) :: scale_pixels

        p = world_to_camera(point, camera)
        depth = p%z
        if (p%z <= camera%near_z) then
            screen = vec2(0.0_rk, 0.0_rk)
            if (present(screen_scale)) screen_scale = 0.0_rk
            project_point = .false.
            return
        end if

        scale_pixels = 0.5_rk * real(min(width, height), rk) * camera%focal_length
        screen%x = 0.5_rk * real(width, rk) + scale_pixels * p%x / p%z
        screen%y = 0.5_rk * real(height, rk) - scale_pixels * p%y / p%z
        if (present(screen_scale)) screen_scale = scale_pixels / p%z
        project_point = screen%x > -real(width, rk) .and. screen%x < 2.0_rk * real(width, rk) .and. &
                        screen%y > -real(height, rk) .and. screen%y < 2.0_rk * real(height, rk)
    end function project_point

    pure real(rk) function clamp(value, lo, hi)
        real(rk), intent(in) :: value
        real(rk), intent(in) :: lo
        real(rk), intent(in) :: hi
        clamp = max(lo, min(hi, value))
    end function clamp

    pure real(rk) function lerp(a, b, t)
        real(rk), intent(in) :: a
        real(rk), intent(in) :: b
        real(rk), intent(in) :: t
        lerp = a + (b - a) * t
    end function lerp

end module vector_math
