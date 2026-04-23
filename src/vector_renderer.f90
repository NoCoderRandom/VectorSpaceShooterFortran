module vector_renderer
    use model_library, only: screen_line
    use platform, only: platform_draw_line
    implicit none
    private

    public :: draw_line_glow
    public :: draw_screen_lines
    public :: draw_box
    public :: draw_reticle
    public :: draw_meter

contains

    subroutine draw_screen_lines(lines, line_count)
        type(screen_line), intent(in) :: lines(:)
        integer, intent(in) :: line_count
        integer :: i
        integer :: thickness
        real :: near_factor

        do i = 1, line_count
            near_factor = max(0.0, min(1.0, real(28.0 / max(1.0, real(lines(i)%depth)))))
            thickness = merge(2, 1, near_factor > 0.82)
            call draw_line_glow(lines(i)%x1, lines(i)%y1, lines(i)%x2, lines(i)%y2, &
                lines(i)%r, lines(i)%g, lines(i)%b, lines(i)%a, thickness)
        end do
    end subroutine draw_screen_lines

    subroutine draw_line_glow(x1, y1, x2, y2, r, g, b, a, thickness)
        integer, intent(in) :: x1
        integer, intent(in) :: y1
        integer, intent(in) :: x2
        integer, intent(in) :: y2
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: b
        integer, intent(in), optional :: a
        integer, intent(in), optional :: thickness
        integer :: alpha
        integer :: thick
        integer :: dx
        integer :: dy
        integer :: ox
        integer :: oy

        alpha = 255
        if (present(a)) alpha = a
        thick = 1
        if (present(thickness)) thick = max(1, thickness)

        call platform_draw_line(x1 - 3, y1, x2 - 3, y2, r / 3, g / 3, b / 3, max(12, alpha / 7))
        call platform_draw_line(x1 + 3, y1, x2 + 3, y2, r / 3, g / 3, b / 3, max(12, alpha / 7))
        call platform_draw_line(x1, y1 - 3, x2, y2 - 3, r / 3, g / 3, b / 3, max(12, alpha / 7))
        call platform_draw_line(x1, y1 + 3, x2, y2 + 3, r / 3, g / 3, b / 3, max(12, alpha / 7))

        do dy = -thick + 1, thick - 1
            do dx = -thick + 1, thick - 1
                ox = dx
                oy = dy
                call platform_draw_line(x1 + ox, y1 + oy, x2 + ox, y2 + oy, r, g, b, alpha)
            end do
        end do
    end subroutine draw_line_glow

    subroutine draw_box(x1, y1, x2, y2, r, g, b, alpha)
        integer, intent(in) :: x1
        integer, intent(in) :: y1
        integer, intent(in) :: x2
        integer, intent(in) :: y2
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: b
        integer, intent(in) :: alpha

        call draw_line_glow(x1, y1, x2, y1, r, g, b, alpha)
        call draw_line_glow(x2, y1, x2, y2, r, g, b, alpha)
        call draw_line_glow(x2, y2, x1, y2, r, g, b, alpha)
        call draw_line_glow(x1, y2, x1, y1, r, g, b, alpha)
    end subroutine draw_box

    subroutine draw_reticle(cx, cy, radius, intensity)
        integer, intent(in) :: cx
        integer, intent(in) :: cy
        integer, intent(in) :: radius
        real, intent(in) :: intensity
        integer :: a
        integer :: r

        a = max(80, min(255, nint(180.0 + 75.0 * intensity)))
        r = radius + nint(4.0 * intensity)
        call draw_line_glow(cx - r - 18, cy, cx - r - 5, cy, 0, 255, 230, a, 1)
        call draw_line_glow(cx + r + 5, cy, cx + r + 18, cy, 0, 255, 230, a, 1)
        call draw_line_glow(cx, cy - r - 18, cx, cy - r - 5, 0, 255, 230, a, 1)
        call draw_line_glow(cx, cy + r + 5, cx, cy + r + 18, 0, 255, 230, a, 1)
        call draw_line_glow(cx - r, cy - r, cx - r + 8, cy - r, 0, 180, 255, a / 2, 1)
        call draw_line_glow(cx + r, cy - r, cx + r - 8, cy - r, 0, 180, 255, a / 2, 1)
        call draw_line_glow(cx - r, cy + r, cx - r + 8, cy + r, 0, 180, 255, a / 2, 1)
        call draw_line_glow(cx + r, cy + r, cx + r - 8, cy + r, 0, 180, 255, a / 2, 1)
        call draw_line_glow(cx - 3, cy, cx + 3, cy, 255, 255, 255, a, 1)
        call draw_line_glow(cx, cy - 3, cx, cy + 3, 255, 255, 255, a, 1)
    end subroutine draw_reticle

    subroutine draw_meter(x, y, width, height, value, r, g, b)
        integer, intent(in) :: x
        integer, intent(in) :: y
        integer, intent(in) :: width
        integer, intent(in) :: height
        real, intent(in) :: value
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: b
        integer :: fill_width

        call draw_box(x, y, x + width, y + height, r / 2, g / 2, b / 2, 140)
        fill_width = max(0, min(width - 4, nint(real(width - 4) * max(0.0, min(1.0, value)))))
        if (fill_width > 0) then
            call draw_line_glow(x + 2, y + height / 2, x + 2 + fill_width, y + height / 2, r, g, b, 230, max(1, height / 6))
        end if
    end subroutine draw_meter

end module vector_renderer
