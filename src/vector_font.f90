module vector_font
    use vector_renderer, only: draw_line_glow
    implicit none
    private

    public :: draw_text
    public :: draw_centered_text
    public :: text_width

contains

    subroutine draw_text(text, x, y, unit, r, g, b, alpha)
        character(len=*), intent(in) :: text
        integer, intent(in) :: x
        integer, intent(in) :: y
        integer, intent(in) :: unit
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: b
        integer, intent(in) :: alpha
        integer :: i
        integer :: row
        integer :: col
        integer :: px
        integer :: py
        integer :: thick
        character(len=5) :: rows(7)
        character(len=1) :: ch

        thick = max(1, unit / 4)
        do i = 1, len_trim(text)
            ch = to_upper(text(i:i))
            call glyph_rows(ch, rows)
            do row = 1, 7
                do col = 1, 5
                    if (rows(row)(col:col) /= " ") then
                        px = x + (i - 1) * 6 * unit + (col - 1) * unit
                        py = y + (row - 1) * unit
                        call draw_line_glow(px, py + unit / 2, px + max(1, unit - 2), py + unit / 2, r, g, b, alpha, thick)
                    end if
                end do
            end do
        end do
    end subroutine draw_text

    subroutine draw_centered_text(text, cx, y, unit, r, g, b, alpha)
        character(len=*), intent(in) :: text
        integer, intent(in) :: cx
        integer, intent(in) :: y
        integer, intent(in) :: unit
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: b
        integer, intent(in) :: alpha

        call draw_text(text, cx - text_width(text, unit) / 2, y, unit, r, g, b, alpha)
    end subroutine draw_centered_text

    pure integer function text_width(text, unit)
        character(len=*), intent(in) :: text
        integer, intent(in) :: unit
        text_width = max(0, len_trim(text)) * 6 * unit
    end function text_width

    pure character(len=1) function to_upper(ch)
        character(len=1), intent(in) :: ch
        integer :: code

        code = iachar(ch)
        if (code >= iachar("a") .and. code <= iachar("z")) then
            to_upper = achar(code - 32)
        else
            to_upper = ch
        end if
    end function to_upper

    pure subroutine glyph_rows(ch, rows)
        character(len=1), intent(in) :: ch
        character(len=5), intent(out) :: rows(7)

        rows = [character(len=5) :: "     ", "     ", "     ", "     ", "     ", "     ", "     "]

        select case (ch)
        case ("A")
            rows = [character(len=5) :: " ### ", "#   #", "#   #", "#####", "#   #", "#   #", "#   #"]
        case ("B")
            rows = [character(len=5) :: "#### ", "#   #", "#   #", "#### ", "#   #", "#   #", "#### "]
        case ("C")
            rows = [character(len=5) :: " ### ", "#   #", "#    ", "#    ", "#    ", "#   #", " ### "]
        case ("D")
            rows = [character(len=5) :: "#### ", "#   #", "#   #", "#   #", "#   #", "#   #", "#### "]
        case ("E")
            rows = [character(len=5) :: "#####", "#    ", "#    ", "#### ", "#    ", "#    ", "#####"]
        case ("F")
            rows = [character(len=5) :: "#####", "#    ", "#    ", "#### ", "#    ", "#    ", "#    "]
        case ("G")
            rows = [character(len=5) :: " ### ", "#   #", "#    ", "#  ##", "#   #", "#   #", " ####"]
        case ("H")
            rows = [character(len=5) :: "#   #", "#   #", "#   #", "#####", "#   #", "#   #", "#   #"]
        case ("I")
            rows = [character(len=5) :: "#####", "  #  ", "  #  ", "  #  ", "  #  ", "  #  ", "#####"]
        case ("J")
            rows = [character(len=5) :: "#####", "   # ", "   # ", "   # ", "   # ", "#  # ", " ##  "]
        case ("K")
            rows = [character(len=5) :: "#   #", "#  # ", "# #  ", "##   ", "# #  ", "#  # ", "#   #"]
        case ("L")
            rows = [character(len=5) :: "#    ", "#    ", "#    ", "#    ", "#    ", "#    ", "#####"]
        case ("M")
            rows = [character(len=5) :: "#   #", "## ##", "# # #", "# # #", "#   #", "#   #", "#   #"]
        case ("N")
            rows = [character(len=5) :: "#   #", "##  #", "##  #", "# # #", "#  ##", "#  ##", "#   #"]
        case ("O")
            rows = [character(len=5) :: " ### ", "#   #", "#   #", "#   #", "#   #", "#   #", " ### "]
        case ("P")
            rows = [character(len=5) :: "#### ", "#   #", "#   #", "#### ", "#    ", "#    ", "#    "]
        case ("Q")
            rows = [character(len=5) :: " ### ", "#   #", "#   #", "#   #", "# # #", "#  # ", " ## #"]
        case ("R")
            rows = [character(len=5) :: "#### ", "#   #", "#   #", "#### ", "# #  ", "#  # ", "#   #"]
        case ("S")
            rows = [character(len=5) :: " ####", "#    ", "#    ", " ### ", "    #", "    #", "#### "]
        case ("T")
            rows = [character(len=5) :: "#####", "  #  ", "  #  ", "  #  ", "  #  ", "  #  ", "  #  "]
        case ("U")
            rows = [character(len=5) :: "#   #", "#   #", "#   #", "#   #", "#   #", "#   #", " ### "]
        case ("V")
            rows = [character(len=5) :: "#   #", "#   #", "#   #", "#   #", "#   #", " # # ", "  #  "]
        case ("W")
            rows = [character(len=5) :: "#   #", "#   #", "#   #", "# # #", "# # #", "## ##", "#   #"]
        case ("X")
            rows = [character(len=5) :: "#   #", "#   #", " # # ", "  #  ", " # # ", "#   #", "#   #"]
        case ("Y")
            rows = [character(len=5) :: "#   #", "#   #", " # # ", "  #  ", "  #  ", "  #  ", "  #  "]
        case ("Z")
            rows = [character(len=5) :: "#####", "    #", "   # ", "  #  ", " #   ", "#    ", "#####"]
        case ("0")
            rows = [character(len=5) :: " ### ", "#   #", "#  ##", "# # #", "##  #", "#   #", " ### "]
        case ("1")
            rows = [character(len=5) :: "  #  ", " ##  ", "# #  ", "  #  ", "  #  ", "  #  ", "#####"]
        case ("2")
            rows = [character(len=5) :: " ### ", "#   #", "    #", "   # ", "  #  ", " #   ", "#####"]
        case ("3")
            rows = [character(len=5) :: "#### ", "    #", "    #", " ### ", "    #", "    #", "#### "]
        case ("4")
            rows = [character(len=5) :: "#   #", "#   #", "#   #", "#####", "    #", "    #", "    #"]
        case ("5")
            rows = [character(len=5) :: "#####", "#    ", "#    ", "#### ", "    #", "    #", "#### "]
        case ("6")
            rows = [character(len=5) :: " ### ", "#    ", "#    ", "#### ", "#   #", "#   #", " ### "]
        case ("7")
            rows = [character(len=5) :: "#####", "    #", "   # ", "  #  ", " #   ", " #   ", " #   "]
        case ("8")
            rows = [character(len=5) :: " ### ", "#   #", "#   #", " ### ", "#   #", "#   #", " ### "]
        case ("9")
            rows = [character(len=5) :: " ### ", "#   #", "#   #", " ####", "    #", "    #", " ### "]
        case (":")
            rows = [character(len=5) :: "     ", "  #  ", "  #  ", "     ", "  #  ", "  #  ", "     "]
        case (",")
            rows = [character(len=5) :: "     ", "     ", "     ", "     ", "     ", "  #  ", " #   "]
        case ("-")
            rows = [character(len=5) :: "     ", "     ", "     ", " ### ", "     ", "     ", "     "]
        case (">")
            rows = [character(len=5) :: "#    ", " #   ", "  #  ", "   # ", "  #  ", " #   ", "#    "]
        case ("/")
            rows = [character(len=5) :: "    #", "   # ", "   # ", "  #  ", " #   ", " #   ", "#    "]
        case (".")
            rows = [character(len=5) :: "     ", "     ", "     ", "     ", "     ", " ##  ", " ##  "]
        case ("!")
            rows = [character(len=5) :: "  #  ", "  #  ", "  #  ", "  #  ", "  #  ", "     ", "  #  "]
        case ("+")
            rows = [character(len=5) :: "     ", "  #  ", "  #  ", "#####", "  #  ", "  #  ", "     "]
        case ("=")
            rows = [character(len=5) :: "     ", "     ", "#####", "     ", "#####", "     ", "     "]
        end select
    end subroutine glyph_rows

end module vector_font
