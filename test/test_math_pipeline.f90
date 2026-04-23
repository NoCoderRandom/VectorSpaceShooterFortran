program test_math_pipeline
    use vector_math, only: rk, vec2, vec3, transform3, camera3, project_point, apply_transform
    use model_library, only: wire_model, screen_line, build_cube_model, append_model_lines
    implicit none

    type(camera3) :: cam
    type(vec2) :: screen
    type(vec3) :: moved
    type(transform3) :: xf
    type(wire_model) :: cube
    type(screen_line) :: lines(128)
    real(rk) :: depth
    real(rk) :: scale_px
    integer :: line_count

    cam%position = vec3(0.0_rk, 0.0_rk, 0.0_rk)
    cam%near_z = 0.2_rk
    cam%focal_length = 1.0_rk

    if (.not. project_point(vec3(0.0_rk, 0.0_rk, 10.0_rk), cam, 1280, 720, screen, depth, scale_px)) then
        error stop "Point in front of camera should project"
    end if
    call assert_close(screen%x, 640.0_rk, 1.0e-6_rk, "center x")
    call assert_close(screen%y, 360.0_rk, 1.0e-6_rk, "center y")
    call assert_close(depth, 10.0_rk, 1.0e-6_rk, "depth")

    if (project_point(vec3(0.0_rk, 0.0_rk, 0.1_rk), cam, 1280, 720, screen, depth, scale_px)) then
        error stop "Point behind near plane should be rejected"
    end if

    xf%position = vec3(1.0_rk, 2.0_rk, 3.0_rk)
    xf%scale = 2.0_rk
    moved = apply_transform(vec3(1.0_rk, 0.0_rk, 0.0_rk), xf)
    call assert_close(moved%x, 3.0_rk, 1.0e-6_rk, "transform x")
    call assert_close(moved%y, 2.0_rk, 1.0e-6_rk, "transform y")
    call assert_close(moved%z, 3.0_rk, 1.0e-6_rk, "transform z")

    call build_cube_model(cube)
    xf%position = vec3(0.0_rk, 0.0_rk, 8.0_rk)
    xf%rotation = vec3(0.25_rk, 0.35_rk, 0.0_rk)
    xf%scale = 1.0_rk
    line_count = 0
    call append_model_lines(cube, xf, cam, 1280, 720, lines, line_count, size(lines), 255, 1.0_rk)
    if (line_count <= 0) error stop "Projected cube should emit visible lines"
    if (line_count > cube%edge_count) error stop "Line count exceeds source edges"

    print '(a)', "test_math_pipeline: ok"

contains

    subroutine assert_close(actual, expected, tolerance, label)
        real(rk), intent(in) :: actual
        real(rk), intent(in) :: expected
        real(rk), intent(in) :: tolerance
        character(len=*), intent(in) :: label

        if (abs(actual - expected) > tolerance) then
            print '(a,1x,a,1x,es12.4,1x,a,1x,es12.4)', "assert_close failed", trim(label), actual, "expected", expected
            error stop
        end if
    end subroutine assert_close

end program test_math_pipeline
