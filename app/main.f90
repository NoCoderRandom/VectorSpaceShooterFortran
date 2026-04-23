program vector_space_shooter
    use game, only: run_game
    implicit none

    integer :: i
    character(len=64) :: arg
    logical :: demo_mode
    logical :: screenshot_mode

    demo_mode = .false.
    screenshot_mode = .false.

    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        select case (trim(arg))
        case ("--demo")
            demo_mode = .true.
        case ("--screenshot")
            screenshot_mode = .true.
        case ("--help", "-h")
            call print_help()
            stop
        end select
    end do

    call run_game(demo_mode, screenshot_mode)

contains

    subroutine print_help()
        print '(a)', "Vector Strike 77"
        print '(a)', "Usage: vector_space_shooter [--demo] [--screenshot]"
        print '(a)', "Controls: WASD/arrows aim, Space/F fire, P pause, F12 capture, ESC quit."
    end subroutine print_help

end program vector_space_shooter
