module platform
    use iso_c_binding, only: c_char, c_float, c_int, c_int32_t, c_null_char
    implicit none
    private

    integer, parameter, public :: key_a = 4
    integer, parameter, public :: key_d = 7
    integer, parameter, public :: key_f = 9
    integer, parameter, public :: key_p = 19
    integer, parameter, public :: key_r = 21
    integer, parameter, public :: key_s = 22
    integer, parameter, public :: key_w = 26
    integer, parameter, public :: key_return = 40
    integer, parameter, public :: key_escape = 41
    integer, parameter, public :: key_tab = 43
    integer, parameter, public :: key_space = 44
    integer, parameter, public :: key_f12 = 69
    integer, parameter, public :: key_right = 79
    integer, parameter, public :: key_left = 80
    integer, parameter, public :: key_down = 81
    integer, parameter, public :: key_up = 82
    integer, parameter, public :: key_lshift = 225

    public :: platform_init
    public :: platform_shutdown
    public :: platform_pump_events
    public :: platform_key_down
    public :: platform_ticks
    public :: platform_delay
    public :: platform_get_draw_size
    public :: platform_begin_frame
    public :: platform_present
    public :: platform_draw_line
    public :: platform_audio_beep
    public :: platform_save_screenshot
    public :: platform_vsync_active
    public :: platform_mouse_state

    integer, parameter, public :: mouse_button_left = 1
    integer, parameter, public :: mouse_button_right = 4
    integer, parameter, public :: mouse_button_middle = 2

    interface
        function c_vs_init(title, width, height) bind(c, name="vs_init") result(ok)
            import :: c_char, c_int
            character(kind=c_char), dimension(*), intent(in) :: title
            integer(c_int), value :: width
            integer(c_int), value :: height
            integer(c_int) :: ok
        end function

        subroutine c_vs_shutdown() bind(c, name="vs_shutdown")
        end subroutine

        function c_vs_pump_events() bind(c, name="vs_pump_events") result(quit)
            import :: c_int
            integer(c_int) :: quit
        end function

        function c_vs_key_down(scancode) bind(c, name="vs_key_down") result(down)
            import :: c_int
            integer(c_int), value :: scancode
            integer(c_int) :: down
        end function

        function c_vs_ticks() bind(c, name="vs_ticks") result(ticks)
            import :: c_int32_t
            integer(c_int32_t) :: ticks
        end function

        subroutine c_vs_delay(ms) bind(c, name="vs_delay")
            import :: c_int
            integer(c_int), value :: ms
        end subroutine

        subroutine c_vs_get_draw_size(width, height) bind(c, name="vs_get_draw_size")
            import :: c_int
            integer(c_int), intent(out) :: width
            integer(c_int), intent(out) :: height
        end subroutine

        subroutine c_vs_begin_frame() bind(c, name="vs_begin_frame")
        end subroutine

        subroutine c_vs_present() bind(c, name="vs_present")
        end subroutine

        subroutine c_vs_draw_line(x1, y1, x2, y2, r, g, b, a) bind(c, name="vs_draw_line")
            import :: c_int
            integer(c_int), value :: x1
            integer(c_int), value :: y1
            integer(c_int), value :: x2
            integer(c_int), value :: y2
            integer(c_int), value :: r
            integer(c_int), value :: g
            integer(c_int), value :: b
            integer(c_int), value :: a
        end subroutine

        subroutine c_vs_audio_beep(freq, seconds, volume) bind(c, name="vs_audio_beep")
            import :: c_float
            real(c_float), value :: freq
            real(c_float), value :: seconds
            real(c_float), value :: volume
        end subroutine

        function c_vs_save_screenshot(path) bind(c, name="vs_save_screenshot") result(ok)
            import :: c_char, c_int
            character(kind=c_char), dimension(*), intent(in) :: path
            integer(c_int) :: ok
        end function

        function c_vs_vsync_active() bind(c, name="vs_vsync_active") result(on)
            import :: c_int
            integer(c_int) :: on
        end function

        subroutine c_vs_mouse_state(x, y, buttons, moved) bind(c, name="vs_mouse_state")
            import :: c_int
            integer(c_int), intent(out) :: x
            integer(c_int), intent(out) :: y
            integer(c_int), intent(out) :: buttons
            integer(c_int), intent(out) :: moved
        end subroutine
    end interface

contains

    logical function platform_init(title, width, height)
        character(len=*), intent(in) :: title
        integer, intent(in) :: width
        integer, intent(in) :: height
        character(kind=c_char), allocatable :: ctitle(:)

        call make_c_string(trim(title), ctitle)
        platform_init = c_vs_init(ctitle, int(width, c_int), int(height, c_int)) /= 0
    end function platform_init

    subroutine platform_shutdown()
        call c_vs_shutdown()
    end subroutine platform_shutdown

    logical function platform_pump_events()
        platform_pump_events = c_vs_pump_events() /= 0
    end function platform_pump_events

    logical function platform_key_down(scancode)
        integer, intent(in) :: scancode
        platform_key_down = c_vs_key_down(int(scancode, c_int)) /= 0
    end function platform_key_down

    integer function platform_ticks()
        platform_ticks = int(c_vs_ticks())
    end function platform_ticks

    subroutine platform_delay(ms)
        integer, intent(in) :: ms
        call c_vs_delay(int(ms, c_int))
    end subroutine platform_delay

    subroutine platform_get_draw_size(width, height)
        integer, intent(out) :: width
        integer, intent(out) :: height
        integer(c_int) :: cw
        integer(c_int) :: ch

        call c_vs_get_draw_size(cw, ch)
        width = int(cw)
        height = int(ch)
    end subroutine platform_get_draw_size

    subroutine platform_begin_frame()
        call c_vs_begin_frame()
    end subroutine platform_begin_frame

    subroutine platform_present()
        call c_vs_present()
    end subroutine platform_present

    subroutine platform_draw_line(x1, y1, x2, y2, r, g, b, a)
        integer, intent(in) :: x1
        integer, intent(in) :: y1
        integer, intent(in) :: x2
        integer, intent(in) :: y2
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: b
        integer, intent(in), optional :: a
        integer :: alpha

        alpha = 255
        if (present(a)) alpha = a
        call c_vs_draw_line(int(x1, c_int), int(y1, c_int), int(x2, c_int), int(y2, c_int), &
            int(clamp_i(r, 0, 255), c_int), int(clamp_i(g, 0, 255), c_int), &
            int(clamp_i(b, 0, 255), c_int), int(clamp_i(alpha, 0, 255), c_int))
    end subroutine platform_draw_line

    subroutine platform_audio_beep(freq, seconds, volume)
        real, intent(in) :: freq
        real, intent(in) :: seconds
        real, intent(in) :: volume

        call c_vs_audio_beep(real(freq, c_float), real(seconds, c_float), real(volume, c_float))
    end subroutine platform_audio_beep

    logical function platform_save_screenshot(path)
        character(len=*), intent(in) :: path
        character(kind=c_char), allocatable :: cpath(:)

        call make_c_string(trim(path), cpath)
        platform_save_screenshot = c_vs_save_screenshot(cpath) /= 0
    end function platform_save_screenshot

    logical function platform_vsync_active()
        platform_vsync_active = c_vs_vsync_active() /= 0
    end function platform_vsync_active

    subroutine platform_mouse_state(x, y, buttons, moved)
        integer, intent(out) :: x
        integer, intent(out) :: y
        integer, intent(out) :: buttons
        logical, intent(out) :: moved
        integer(c_int) :: cx
        integer(c_int) :: cy
        integer(c_int) :: cb
        integer(c_int) :: cm

        call c_vs_mouse_state(cx, cy, cb, cm)
        x = int(cx)
        y = int(cy)
        buttons = int(cb)
        moved = cm /= 0
    end subroutine platform_mouse_state

    subroutine make_c_string(text, out)
        character(len=*), intent(in) :: text
        character(kind=c_char), allocatable, intent(out) :: out(:)
        integer :: i
        integer :: n

        n = len_trim(text)
        allocate(out(n + 1))
        do i = 1, n
            out(i) = text(i:i)
        end do
        out(n + 1) = c_null_char
    end subroutine make_c_string

    pure integer function clamp_i(value, lo, hi)
        integer, intent(in) :: value
        integer, intent(in) :: lo
        integer, intent(in) :: hi
        clamp_i = max(lo, min(hi, value))
    end function clamp_i

end module platform
