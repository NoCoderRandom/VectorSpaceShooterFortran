module game
    use iso_fortran_env, only: output_unit
    use vector_math, only: rk, vec2, vec3, transform3, camera3, make_vec3, add3, scale3, project_point, clamp, pi
    use platform, only: platform_init, platform_shutdown, platform_pump_events, platform_key_down, &
        platform_ticks, platform_delay, platform_get_draw_size, platform_begin_frame, platform_present, &
        platform_audio_beep, platform_save_screenshot, platform_vsync_active, platform_mouse_state, &
        mouse_button_left, mouse_button_right, &
        key_a, key_d, key_f, key_p, key_r, key_s, key_w, &
        key_return, key_escape, key_space, key_f12, key_right, key_left, key_down, key_up, key_lshift
    use model_library, only: wire_model, screen_line, build_player_model, build_enemy_model, build_gate_model, &
        build_shield_gate_model, append_model_lines
    use vector_renderer, only: draw_line_glow, draw_screen_lines, draw_box, draw_reticle, draw_meter
    use vector_font, only: draw_text, draw_centered_text
    implicit none
    private

    integer, parameter :: state_title = 0
    integer, parameter :: state_play = 1
    integer, parameter :: state_game_over = 2
    integer, parameter :: state_victory = 3

    integer, parameter :: max_stars = 220
    integer, parameter :: max_enemies = 48
    integer, parameter :: max_particles = 420
    integer, parameter :: max_lines = 4096

    integer, parameter :: waves_per_sector = 3
    integer, parameter :: max_sector = 3

    character(len=*), parameter :: high_score_path = "highscore.dat"

    real(rk), parameter :: target_fps = 60.0_rk

    type :: star_t
        real(rk) :: x = 0.0_rk
        real(rk) :: y = 0.0_rk
        real(rk) :: z = 1.0_rk
        real(rk) :: speed = 1.0_rk
        integer :: shade = 160
    end type star_t

    type :: enemy_t
        logical :: active = .false.
        real(rk) :: x = 0.0_rk
        real(rk) :: y = 0.0_rk
        real(rk) :: z = 20.0_rk
        real(rk) :: vx = 0.0_rk
        real(rk) :: vy = 0.0_rk
        real(rk) :: speed = 4.0_rk
        real(rk) :: age = 0.0_rk
        real(rk) :: phase = 0.0_rk
        real(rk) :: flash = 0.0_rk
        integer :: pattern = 1
        integer :: hp = 1
    end type enemy_t

    type :: particle_t
        logical :: active = .false.
        type(vec3) :: position = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        type(vec3) :: velocity = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        real(rk) :: ttl = 0.0_rk
        real(rk) :: max_ttl = 1.0_rk
        integer :: r = 255
        integer :: g = 255
        integer :: b = 255
    end type particle_t

    type :: game_state_t
        integer :: state = state_title
        integer :: score = 0
        integer :: high_score = 0
        integer :: wave = 1
        integer :: sector = 1
        integer :: sector_wave = 1
        integer :: lives = 3
        integer :: kills = 0
        integer :: kills_sector_wave = 0
        integer :: spawn_serial = 0
        real(rk) :: shield = 1.0_rk
        real(rk) :: reticle_x = 0.0_rk
        real(rk) :: reticle_y = 0.0_rk
        real(rk) :: fire_cooldown = 0.0_rk
        real(rk) :: shot_flash = 0.0_rk
        real(rk) :: laser_x = 0.0_rk
        real(rk) :: laser_y = 0.0_rk
        real(rk) :: spawn_timer = 0.0_rk
        real(rk) :: danger_timer = 0.0_rk
        real(rk) :: message_timer = 0.0_rk
        real(rk) :: screen_shake = 0.0_rk
        real(rk) :: time = 0.0_rk
        real(rk) :: sector_intro_timer = 0.0_rk
        real(rk) :: demo_palette_timer = 0.0_rk
        logical :: demo_mode = .false.
        logical :: paused = .false.
        character(len=48) :: message = ""
        type(star_t) :: stars(max_stars)
        type(enemy_t) :: enemies(max_enemies)
        type(particle_t) :: particles(max_particles)
        type(wire_model) :: player_model
        type(wire_model) :: enemy_model
        type(wire_model) :: gate_model
        type(wire_model) :: shield_gate_model
        type(screen_line) :: lines(max_lines)
    end type game_state_t

    public :: run_game

contains

    subroutine run_game(demo_mode, screenshot_mode)
        logical, intent(in) :: demo_mode
        logical, intent(in) :: screenshot_mode
        type(game_state_t), allocatable :: gs
        logical :: running
        logical :: quit_requested
        logical :: escape_pressed
        logical :: start_pressed
        logical :: restart_pressed
        logical :: pause_pressed
        logical :: capture_pressed
        logical :: prev_start
        logical :: fire_pressed
        logical :: prev_fire
        logical :: prev_restart
        logical :: prev_pause
        logical :: prev_capture
        logical :: return_down
        logical :: space_down
        logical :: fire_down
        logical :: left_mouse_down
        integer :: mouse_x
        integer :: mouse_y
        integer :: mouse_buttons
        logical :: mouse_moved
        integer :: width
        integer :: height
        integer :: last_ticks
        integer :: now_ticks
        integer :: frame_start
        integer :: elapsed_ms
        real(rk) :: dt
        integer :: screenshot_frames
        logical :: vsync_on

        if (.not. platform_init("Vector Strike 77 - Modern Fortran SDL2", 1280, 720)) then
            write(output_unit, '(a)') "Unable to initialize SDL2 platform."
            return
        end if

        allocate(gs)
        call init_game(gs, demo_mode)
        if (demo_mode .or. screenshot_mode) call reset_play(gs, .true.)

        prev_start = .false.
        prev_fire = .false.
        prev_restart = .false.
        prev_pause = .false.
        prev_capture = .false.
        running = .true.
        screenshot_frames = merge(150, huge(1), screenshot_mode)
        last_ticks = platform_ticks()
        vsync_on = platform_vsync_active()

        do while (running)
            frame_start = platform_ticks()
            quit_requested = platform_pump_events()
            escape_pressed = platform_key_down(key_escape)
            if (quit_requested .or. escape_pressed) exit

            call platform_get_draw_size(width, height)
            if (width <= 0 .or. height <= 0) then
                call platform_delay(16)
                cycle
            end if

            now_ticks = platform_ticks()
            dt = real(max(1, now_ticks - last_ticks), rk) / 1000.0_rk
            dt = min(dt, 1.0_rk / 25.0_rk)
            last_ticks = now_ticks
            if (screenshot_mode) dt = 1.0_rk / target_fps

            return_down = platform_key_down(key_return)
            space_down = platform_key_down(key_space)
            fire_down = platform_key_down(key_f)
            restart_pressed = platform_key_down(key_r)
            pause_pressed = platform_key_down(key_p)
            capture_pressed = platform_key_down(key_f12)
            call platform_mouse_state(mouse_x, mouse_y, mouse_buttons, mouse_moved)
            left_mouse_down = iand(mouse_buttons, mouse_button_left) /= 0
            start_pressed = return_down .or. space_down .or. left_mouse_down
            fire_pressed = space_down .or. fire_down .or. left_mouse_down

            if (gs%state == state_title .and. start_pressed .and. .not. prev_start) then
                call reset_play(gs, .false.)
                call platform_audio_beep(320.0, 0.10, 0.18)
                call platform_audio_beep(640.0, 0.12, 0.14)
            end if

            if (gs%state == state_game_over .or. gs%state == state_victory) then
                if ((restart_pressed .and. .not. prev_restart) .or. (start_pressed .and. .not. prev_start)) then
                    call reset_play(gs, .false.)
                end if
            end if

            if (pause_pressed .and. .not. prev_pause .and. gs%state == state_play) then
                gs%paused = .not. gs%paused
                call platform_audio_beep(220.0, 0.05, 0.08)
            end if

            if (capture_pressed .and. .not. prev_capture) then
                call render_game(gs, width, height)
                if (platform_save_screenshot("capture.bmp")) then
                    gs%message = "CAPTURE BMP SAVED"
                    gs%message_timer = 1.4_rk
                    call platform_audio_beep(900.0, 0.04, 0.10)
                end if
            end if

            if (.not. gs%paused) then
                call update_game(gs, dt, width, height, fire_pressed .and. .not. prev_fire, &
                    mouse_x, mouse_y, mouse_moved, mouse_buttons)
            end if

            call render_game(gs, width, height)
            call platform_present()

            prev_start = start_pressed
            prev_fire = fire_pressed
            prev_restart = restart_pressed
            prev_pause = pause_pressed
            prev_capture = capture_pressed

            if (screenshot_mode) then
                screenshot_frames = screenshot_frames - 1
                if (screenshot_frames <= 0) then
                    call render_game(gs, width, height)
                    if (platform_save_screenshot("capture.bmp")) then
                        write(output_unit, '(a)') "Saved capture.bmp"
                    else
                        write(output_unit, '(a)') "Failed to save capture.bmp"
                    end if
                    exit
                end if
            else if (.not. vsync_on) then
                elapsed_ms = platform_ticks() - frame_start
                if (elapsed_ms < 16) call platform_delay(16 - elapsed_ms)
            end if
        end do

        call platform_shutdown()
        if (allocated(gs)) deallocate(gs)
    end subroutine run_game

    subroutine init_game(gs, demo_mode)
        type(game_state_t), intent(inout) :: gs
        logical, intent(in) :: demo_mode
        integer :: n
        integer, allocatable :: seed(:)
        integer :: i

        call random_seed(size=n)
        allocate(seed(n))
        do i = 1, n
            seed(i) = 104729 + i * 7919
        end do
        call random_seed(put=seed)
        deallocate(seed)

        call build_player_model(gs%player_model)
        call build_enemy_model(gs%enemy_model)
        call build_gate_model(gs%gate_model)
        call build_shield_gate_model(gs%shield_gate_model)
        call init_stars(gs)
        gs%high_score = load_high_score()
        gs%demo_mode = demo_mode
        gs%state = merge(state_play, state_title, demo_mode)
    end subroutine init_game

    integer function load_high_score() result(hs)
        integer :: unit_no
        integer :: ios
        integer :: value

        hs = 0
        open(newunit=unit_no, file=high_score_path, status="old", action="read", iostat=ios)
        if (ios /= 0) return
        read(unit_no, *, iostat=ios) value
        close(unit_no)
        if (ios == 0 .and. value >= 0) hs = value
    end function load_high_score

    subroutine save_high_score(value)
        integer, intent(in) :: value
        integer :: unit_no
        integer :: ios

        open(newunit=unit_no, file=high_score_path, status="replace", action="write", iostat=ios)
        if (ios /= 0) return
        write(unit_no, '(i0)') value
        close(unit_no)
    end subroutine save_high_score

    subroutine reset_play(gs, demo_mode)
        type(game_state_t), intent(inout) :: gs
        logical, intent(in) :: demo_mode
        integer :: i

        gs%state = state_play
        gs%demo_mode = demo_mode
        gs%score = 0
        gs%wave = 1
        gs%sector = 1
        gs%sector_wave = 1
        gs%lives = 3
        gs%kills = 0
        gs%kills_sector_wave = 0
        gs%spawn_serial = 0
        gs%shield = 1.0_rk
        gs%reticle_x = 0.0_rk
        gs%reticle_y = 0.0_rk
        gs%fire_cooldown = 0.15_rk
        gs%shot_flash = 0.0_rk
        gs%spawn_timer = 0.05_rk
        gs%danger_timer = 0.0_rk
        gs%message_timer = 2.0_rk
        gs%sector_intro_timer = 2.6_rk
        gs%demo_palette_timer = 0.0_rk
        if (demo_mode) then
            gs%message = "DEMO WAVE ONLINE"
        else
            gs%message = "PHOSPHOR-LEAD CLEARED FOR LAUNCH"
        end if
        gs%paused = .false.
        do i = 1, max_enemies
            gs%enemies(i)%active = .false.
        end do
        do i = 1, max_particles
            gs%particles(i)%active = .false.
        end do
        do i = 1, 5
            call spawn_enemy(gs, 18.0_rk + real(i, rk) * 4.2_rk)
        end do
    end subroutine reset_play

    subroutine init_stars(gs)
        type(game_state_t), intent(inout) :: gs
        integer :: i
        real(rk) :: rx
        real(rk) :: ry
        real(rk) :: rz
        real(rk) :: rs

        do i = 1, max_stars
            call random_number(rx)
            call random_number(ry)
            call random_number(rz)
            call random_number(rs)
            gs%stars(i)%x = (rx * 2.0_rk - 1.0_rk) * 22.0_rk
            gs%stars(i)%y = (ry * 2.0_rk - 1.0_rk) * 12.0_rk
            gs%stars(i)%z = 3.0_rk + rz * 75.0_rk
            gs%stars(i)%speed = 2.0_rk + rs * 5.0_rk
            gs%stars(i)%shade = 80 + int(rs * 160.0_rk)
        end do
    end subroutine init_stars

    subroutine update_game(gs, dt, width, height, fire_edge, mouse_x, mouse_y, mouse_moved, mouse_buttons)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer, intent(in) :: width
        integer, intent(in) :: height
        logical, intent(in) :: fire_edge
        integer, intent(in) :: mouse_x
        integer, intent(in) :: mouse_y
        logical, intent(in) :: mouse_moved
        integer, intent(in) :: mouse_buttons

        gs%time = gs%time + dt
        gs%fire_cooldown = max(0.0_rk, gs%fire_cooldown - dt)
        gs%shot_flash = max(0.0_rk, gs%shot_flash - dt)
        gs%danger_timer = max(0.0_rk, gs%danger_timer - dt)
        gs%message_timer = max(0.0_rk, gs%message_timer - dt)
        gs%screen_shake = max(0.0_rk, gs%screen_shake - dt * 1.8_rk)
        gs%sector_intro_timer = max(0.0_rk, gs%sector_intro_timer - dt)

        call update_stars(gs, dt, merge(9.0_rk, 3.2_rk, gs%state == state_play))

        select case (gs%state)
        case (state_title)
            call update_title_scene(gs, dt)
        case (state_play)
            call update_player_control(gs, dt, width, height, mouse_x, mouse_y, mouse_moved, mouse_buttons)
            call update_enemies(gs, dt)
            call update_particles(gs, dt)
            call update_spawning(gs, dt)
            call update_demo_autopilot(gs, dt, width, height)
            if ((fire_edge .or. (gs%demo_mode .and. gs%fire_cooldown <= 0.0_rk)) .and. gs%fire_cooldown <= 0.0_rk) then
                call fire_weapon(gs, width, height)
            end if
        case (state_game_over)
            call update_enemies(gs, dt * 0.35_rk)
            call update_particles(gs, dt)
        case (state_victory)
            call update_enemies(gs, dt * 0.25_rk)
            call update_particles(gs, dt)
        end select
    end subroutine update_game

    subroutine update_stars(gs, dt, speed_scale)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk), intent(in) :: speed_scale
        integer :: i
        real(rk) :: rx
        real(rk) :: ry

        do i = 1, max_stars
            gs%stars(i)%z = gs%stars(i)%z - dt * speed_scale * gs%stars(i)%speed
            if (gs%stars(i)%z < 1.0_rk) then
                call random_number(rx)
                call random_number(ry)
                gs%stars(i)%x = (rx * 2.0_rk - 1.0_rk) * 22.0_rk
                gs%stars(i)%y = (ry * 2.0_rk - 1.0_rk) * 12.0_rk
                gs%stars(i)%z = gs%stars(i)%z + 78.0_rk
            end if
        end do
    end subroutine update_stars

    subroutine update_title_scene(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt

        gs%spawn_timer = gs%spawn_timer - dt
        if (gs%spawn_timer <= 0.0_rk) then
            gs%spawn_timer = 1.25_rk
            call spawn_enemy(gs, 28.0_rk)
        end if
        call update_enemies(gs, dt * 0.6_rk)
        call update_particles(gs, dt)
    end subroutine update_title_scene

    subroutine update_player_control(gs, dt, width, height, mouse_x, mouse_y, mouse_moved, mouse_buttons)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer, intent(in) :: mouse_x
        integer, intent(in) :: mouse_y
        logical, intent(in) :: mouse_moved
        integer, intent(in) :: mouse_buttons
        real(rk) :: axis_x
        real(rk) :: axis_y
        real(rk) :: aim_speed
        real(rk) :: target_x
        real(rk) :: target_y
        real(rk) :: sensitivity
        logical :: move_right
        logical :: move_left
        logical :: move_up
        logical :: move_down
        logical :: precision
        logical :: right_mouse

        axis_x = 0.0_rk
        axis_y = 0.0_rk

        move_right = platform_key_down(key_d)
        if (platform_key_down(key_right)) move_right = .true.
        move_left = platform_key_down(key_a)
        if (platform_key_down(key_left)) move_left = .true.
        move_up = platform_key_down(key_w)
        if (platform_key_down(key_up)) move_up = .true.
        move_down = platform_key_down(key_s)
        if (platform_key_down(key_down)) move_down = .true.
        right_mouse = iand(mouse_buttons, mouse_button_right) /= 0
        precision = platform_key_down(key_lshift) .or. right_mouse

        if (move_right) axis_x = axis_x + 1.0_rk
        if (move_left) axis_x = axis_x - 1.0_rk
        if (move_up) axis_y = axis_y + 1.0_rk
        if (move_down) axis_y = axis_y - 1.0_rk

        aim_speed = merge(0.72_rk, 1.85_rk, precision)

        if (mouse_moved .and. width > 0 .and. height > 0) then
            sensitivity = merge(0.55_rk, 1.0_rk, precision)
            target_x = (real(mouse_x, rk) - 0.5_rk * real(width, rk)) / (0.42_rk * real(width, rk))
            target_y = (0.5_rk * real(height, rk) - real(mouse_y, rk)) / (0.38_rk * real(height, rk))
            target_x = gs%reticle_x + (target_x - gs%reticle_x) * sensitivity
            target_y = gs%reticle_y + (target_y - gs%reticle_y) * sensitivity
            gs%reticle_x = clamp(target_x, -0.92_rk, 0.92_rk)
            gs%reticle_y = clamp(target_y, -0.72_rk, 0.72_rk)
        else
            gs%reticle_x = clamp(gs%reticle_x + axis_x * aim_speed * dt, -0.92_rk, 0.92_rk)
            gs%reticle_y = clamp(gs%reticle_y + axis_y * aim_speed * dt, -0.72_rk, 0.72_rk)
        end if

        if (width < height) then
            gs%reticle_y = clamp(gs%reticle_y, -0.58_rk, 0.58_rk)
        end if
    end subroutine update_player_control

    subroutine update_demo_autopilot(gs, dt, width, height)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer :: index
        real(rk) :: tx
        real(rk) :: ty
        real(rk) :: blend
        real(rk) :: jitter_x
        real(rk) :: jitter_y

        if (.not. gs%demo_mode) return

        call find_demo_target(gs, width, height, index, tx, ty)
        jitter_x = 0.035_rk * sin(gs%time * 4.3_rk + 1.1_rk)
        jitter_y = 0.030_rk * sin(gs%time * 3.1_rk + 2.7_rk)
        if (index > 0) then
            blend = min(1.0_rk, dt * 4.2_rk)
            gs%reticle_x = gs%reticle_x + (tx + jitter_x - gs%reticle_x) * blend
            gs%reticle_y = gs%reticle_y + (ty + jitter_y - gs%reticle_y) * blend
        else
            gs%reticle_x = 0.18_rk * sin(gs%time * 0.8_rk) + jitter_x
            gs%reticle_y = 0.12_rk * cos(gs%time * 0.7_rk) + jitter_y
        end if
    end subroutine update_demo_autopilot

    subroutine find_demo_target(gs, width, height, index, tx, ty)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer, intent(out) :: index
        real(rk), intent(out) :: tx
        real(rk), intent(out) :: ty
        integer :: i
        real(rk) :: best_z
        type(vec3) :: pos
        type(vec2) :: screen
        type(camera3) :: cam
        real(rk) :: depth
        real(rk) :: scale_px

        index = 0
        tx = 0.0_rk
        ty = 0.0_rk
        best_z = huge(1.0_rk)
        cam = scene_camera(gs)
        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) cycle
            pos = enemy_position(gs%enemies(i))
            if (pos%z < 3.0_rk .or. pos%z > best_z) cycle
            if (.not. project_point(pos, cam, width, height, screen, depth, scale_px)) cycle
            index = i
            best_z = pos%z
            tx = clamp((screen%x - 0.5_rk * real(width, rk)) / (0.42_rk * real(width, rk)), -0.88_rk, 0.88_rk)
            ty = clamp((0.5_rk * real(height, rk) - screen%y) / (0.38_rk * real(height, rk)), -0.68_rk, 0.68_rk)
        end do
    end subroutine find_demo_target

    subroutine update_enemies(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer :: i
        type(vec3) :: pos
        logical :: breached

        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) cycle
            gs%enemies(i)%age = gs%enemies(i)%age + dt
            gs%enemies(i)%z = gs%enemies(i)%z - gs%enemies(i)%speed * dt
            gs%enemies(i)%flash = max(0.0_rk, gs%enemies(i)%flash - dt * 4.0_rk)
            pos = enemy_position(gs%enemies(i))
            breached = pos%z < 1.25_rk
            if (breached) then
                gs%enemies(i)%active = .false.
                if (gs%state == state_play) call player_hit(gs, pos)
            end if
            if (pos%z < 8.0_rk .and. gs%danger_timer <= 0.0_rk .and. gs%state == state_play) then
                gs%danger_timer = 0.55_rk
                call platform_audio_beep(120.0, 0.08, 0.07)
            end if
        end do
    end subroutine update_enemies

    subroutine update_particles(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer :: i

        do i = 1, max_particles
            if (.not. gs%particles(i)%active) cycle
            gs%particles(i)%ttl = gs%particles(i)%ttl - dt
            if (gs%particles(i)%ttl <= 0.0_rk) then
                gs%particles(i)%active = .false.
            else
                gs%particles(i)%position = add3(gs%particles(i)%position, scale3(gs%particles(i)%velocity, dt))
                gs%particles(i)%velocity = scale3(gs%particles(i)%velocity, 0.992_rk)
            end if
        end do
    end subroutine update_particles

    subroutine update_spawning(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk) :: interval
        real(rk) :: rz

        gs%spawn_timer = gs%spawn_timer - dt
        if (gs%spawn_timer > 0.0_rk) return

        call random_number(rz)
        interval = max(0.42_rk, 1.20_rk - real(gs%wave, rk) * 0.075_rk)
        gs%spawn_timer = interval + rz * 0.45_rk
        call spawn_enemy(gs, 24.0_rk + rz * 13.0_rk)
    end subroutine update_spawning

    subroutine spawn_enemy(gs, z_override)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in), optional :: z_override
        integer :: slot
        integer :: i
        real(rk) :: rx
        real(rk) :: ry
        real(rk) :: rz
        real(rk) :: sign

        slot = 0
        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) then
                slot = i
                exit
            end if
        end do
        if (slot == 0) return

        gs%spawn_serial = gs%spawn_serial + 1
        call random_number(rx)
        call random_number(ry)
        call random_number(rz)
        sign = merge(1.0_rk, -1.0_rk, rx > 0.5_rk)

        gs%enemies(slot)%active = .true.
        gs%enemies(slot)%pattern = pick_pattern(gs%wave, gs%spawn_serial)
        gs%enemies(slot)%age = 0.0_rk
        gs%enemies(slot)%phase = rz * 2.0_rk * pi
        gs%enemies(slot)%flash = 0.0_rk
        gs%enemies(slot)%hp = merge(2, 1, gs%wave >= 4 .and. mod(gs%spawn_serial, 4) == 0)
        gs%enemies(slot)%speed = 3.6_rk + real(gs%wave, rk) * 0.22_rk + rz * 1.35_rk
        if (present(z_override)) then
            gs%enemies(slot)%z = z_override
        else
            gs%enemies(slot)%z = 25.0_rk + rz * 14.0_rk
        end if

        select case (gs%enemies(slot)%pattern)
        case (1)
            gs%enemies(slot)%x = (rx * 2.0_rk - 1.0_rk) * 3.0_rk
            gs%enemies(slot)%y = (ry * 2.0_rk - 1.0_rk) * 1.55_rk
            gs%enemies(slot)%vx = 0.0_rk
            gs%enemies(slot)%vy = 0.0_rk
        case (2)
            gs%enemies(slot)%x = (rx * 2.0_rk - 1.0_rk) * 2.4_rk
            gs%enemies(slot)%y = (ry * 2.0_rk - 1.0_rk) * 1.35_rk
            gs%enemies(slot)%vx = sign * 0.35_rk
            gs%enemies(slot)%vy = 0.0_rk
        case (3)
            gs%enemies(slot)%x = sign * (3.4_rk + rx)
            gs%enemies(slot)%y = (ry * 2.0_rk - 1.0_rk) * 1.25_rk
            gs%enemies(slot)%vx = -sign * (0.65_rk + rz * 0.25_rk)
            gs%enemies(slot)%vy = (ry - 0.5_rk) * 0.16_rk
        case (4)
            gs%enemies(slot)%x = (rx * 2.0_rk - 1.0_rk) * 2.2_rk
            gs%enemies(slot)%y = 2.3_rk + ry * 0.6_rk
            gs%enemies(slot)%vx = 0.0_rk
            gs%enemies(slot)%vy = -1.6_rk
            gs%enemies(slot)%speed = gs%enemies(slot)%speed * 1.15_rk
        case default
            gs%enemies(slot)%x = sign * 2.1_rk
            gs%enemies(slot)%y = (ry * 2.0_rk - 1.0_rk) * 0.8_rk
            gs%enemies(slot)%vx = sign * 1.8_rk
            gs%enemies(slot)%vy = 0.0_rk
        end select
    end subroutine spawn_enemy

    pure integer function pick_pattern(wave, serial) result(pattern)
        integer, intent(in) :: wave
        integer, intent(in) :: serial
        integer :: pool

        if (wave >= 5) then
            pool = 5
        else if (wave >= 3) then
            pool = 4
        else
            pool = 3
        end if
        pattern = 1 + mod(serial - 1, pool)
    end function pick_pattern

    pure type(vec3) function enemy_position(enemy)
        type(enemy_t), intent(in) :: enemy
        real(rk) :: weave
        real(rk) :: dive
        real(rk) :: orbit_r
        real(rk) :: orbit_w

        enemy_position = vec3(enemy%x, enemy%y, enemy%z)
        select case (enemy%pattern)
        case (1)
            enemy_position%y = enemy_position%y + 0.13_rk * sin(enemy%age * 2.4_rk + enemy%phase)
        case (2)
            weave = sin(enemy%age * 3.7_rk + enemy%phase)
            enemy_position%x = enemy_position%x + weave * 0.95_rk
            enemy_position%y = enemy_position%y + 0.30_rk * cos(enemy%age * 2.5_rk + enemy%phase)
        case (3)
            enemy_position%x = enemy_position%x + enemy%vx * enemy%age
            enemy_position%y = enemy_position%y + enemy%vy * enemy%age + 0.22_rk * sin(enemy%age * 2.9_rk + enemy%phase)
        case (4)
            dive = max(0.0_rk, 1.0_rk - enemy%z / 18.0_rk)
            enemy_position%y = enemy_position%y + enemy%vy * enemy%age * (0.35_rk + dive)
            enemy_position%x = enemy_position%x + 0.18_rk * sin(enemy%age * 1.9_rk + enemy%phase)
        case default
            orbit_r = 1.0_rk + 0.25_rk * sin(enemy%age * 1.5_rk)
            orbit_w = enemy%age * 2.4_rk + enemy%phase
            enemy_position%x = enemy_position%x + orbit_r * cos(orbit_w)
            enemy_position%y = enemy_position%y + orbit_r * 0.7_rk * sin(orbit_w)
        end select
    end function enemy_position

    subroutine fire_weapon(gs, width, height)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(camera3) :: cam
        type(vec2) :: screen
        type(vec3) :: pos
        real(rk) :: depth
        real(rk) :: scale_px
        real(rk) :: cx
        real(rk) :: cy
        real(rk) :: dist
        real(rk) :: radius_px
        real(rk) :: best_depth
        integer :: best
        integer :: i

        block
            real(rk) :: jitter
            call random_number(jitter)
            if (gs%demo_mode) then
                gs%fire_cooldown = 0.12_rk + jitter * 0.10_rk
            else
                gs%fire_cooldown = 0.135_rk
            end if
        end block
        gs%shot_flash = 0.11_rk
        gs%laser_x = gs%reticle_x
        gs%laser_y = gs%reticle_y
        call platform_audio_beep(780.0, 0.045, 0.16)
        call platform_audio_beep(1140.0, 0.025, 0.08)

        best = 0
        best_depth = huge(1.0_rk)
        cx = 0.5_rk * real(width, rk) + gs%reticle_x * 0.42_rk * real(width, rk)
        cy = 0.5_rk * real(height, rk) - gs%reticle_y * 0.38_rk * real(height, rk)
        cam = scene_camera(gs)

        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) cycle
            pos = enemy_position(gs%enemies(i))
            if (.not. project_point(pos, cam, width, height, screen, depth, scale_px)) cycle
            radius_px = max(20.0_rk, gs%enemy_model%radius * enemy_scale(pos%z) * scale_px * 0.85_rk)
            dist = sqrt((screen%x - cx) ** 2 + (screen%y - cy) ** 2)
            if (dist <= radius_px .and. depth < best_depth) then
                best = i
                best_depth = depth
            end if
        end do

        if (best > 0) then
            call damage_enemy(gs, best)
        end if
    end subroutine fire_weapon

    subroutine damage_enemy(gs, index)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: index
        type(vec3) :: pos

        if (.not. gs%enemies(index)%active) return
        gs%enemies(index)%hp = gs%enemies(index)%hp - 1
        gs%enemies(index)%flash = 1.0_rk
        pos = enemy_position(gs%enemies(index))

        if (gs%enemies(index)%hp <= 0) then
            gs%enemies(index)%active = .false.
            gs%score = gs%score + nint(real(100 + gs%wave * 25 + max(0, nint((22.0_rk - pos%z) * 5.0_rk)), rk) &
                * sector_score_mult(gs%sector))
            gs%kills = gs%kills + 1
            gs%kills_sector_wave = gs%kills_sector_wave + 1
            if (gs%score > gs%high_score) then
                gs%high_score = gs%score
                if (.not. gs%demo_mode) call save_high_score(gs%high_score)
            end if
            call spawn_explosion(gs, pos, 34, 255, 180, 40)
            gs%screen_shake = max(gs%screen_shake, 0.22_rk)
            gs%message = "TARGET BROKEN"
            gs%message_timer = 0.55_rk
            call platform_audio_beep(180.0, 0.09, 0.15)
            call platform_audio_beep(90.0, 0.13, 0.12)
            if (gs%kills_sector_wave >= sector_wave_quota(gs%sector_wave)) then
                call advance_wave(gs)
            end if
        else
            gs%score = gs%score + 25
            call spawn_explosion(gs, pos, 12, 255, 255, 180)
            call platform_audio_beep(360.0, 0.04, 0.12)
        end if
    end subroutine damage_enemy

    pure integer function sector_wave_quota(sector_wave) result(quota)
        integer, intent(in) :: sector_wave
        quota = 6 + sector_wave * 2
    end function sector_wave_quota

    pure real(rk) function sector_score_mult(sector) result(mult)
        integer, intent(in) :: sector
        select case (sector)
        case (1); mult = 1.0_rk
        case (2); mult = 1.5_rk
        case default; mult = 2.0_rk
        end select
    end function sector_score_mult

    subroutine advance_wave(gs)
        type(game_state_t), intent(inout) :: gs

        gs%kills_sector_wave = 0
        gs%wave = gs%wave + 1
        gs%sector_wave = gs%sector_wave + 1

        if (gs%sector_wave > waves_per_sector) then
            if (gs%sector >= max_sector) then
                gs%state = state_victory
                gs%message = "GATE IS DARK"
                gs%message_timer = 99.0_rk
                call platform_audio_beep(180.0, 0.18, 0.18)
                call platform_audio_beep(270.0, 0.22, 0.16)
                call platform_audio_beep(360.0, 0.30, 0.14)
                if (.not. gs%demo_mode .and. gs%score > gs%high_score) then
                    gs%high_score = gs%score
                    call save_high_score(gs%high_score)
                end if
                return
            end if
            gs%sector = gs%sector + 1
            gs%sector_wave = 1
            gs%shield = min(1.0_rk, gs%shield + 0.5_rk)
            gs%sector_intro_timer = 2.8_rk
            gs%message = sector_name(gs%sector)
            gs%message_timer = 2.4_rk
            call platform_audio_beep(320.0, 0.10, 0.16)
            call platform_audio_beep(480.0, 0.12, 0.14)
            call platform_audio_beep(640.0, 0.14, 0.12)
        else
            gs%message = "WAVE UP"
            gs%message_timer = 1.1_rk
            call platform_audio_beep(460.0, 0.08, 0.14)
            call platform_audio_beep(690.0, 0.09, 0.12)
        end if
    end subroutine advance_wave

    pure function sector_name(sector) result(name)
        integer, intent(in) :: sector
        character(len=48) :: name
        select case (sector)
        case (1); name = "SECTOR I - OUTER PICKET"
        case (2); name = "SECTOR II - ASTEROID LANE"
        case default; name = "SECTOR III - STRONGHOLD"
        end select
    end function sector_name

    pure subroutine sector_palette_primary(sector, r, g, b)
        integer, intent(in) :: sector
        integer, intent(out) :: r
        integer, intent(out) :: g
        integer, intent(out) :: b
        select case (sector)
        case (1); r = 0;   g = 200; b = 255
        case (2); r = 210; g = 110; b = 255
        case default; r = 255; g = 130; b = 60
        end select
    end subroutine sector_palette_primary

    pure subroutine sector_palette_accent(sector, r, g, b)
        integer, intent(in) :: sector
        integer, intent(out) :: r
        integer, intent(out) :: g
        integer, intent(out) :: b
        select case (sector)
        case (1); r = 255; g = 220; b = 90
        case (2); r = 120; g = 255; b = 200
        case default; r = 255; g = 240; b = 110
        end select
    end subroutine sector_palette_accent

    pure subroutine sector_palette_dim(sector, r, g, b)
        integer, intent(in) :: sector
        integer, intent(out) :: r
        integer, intent(out) :: g
        integer, intent(out) :: b
        select case (sector)
        case (1); r = 0;   g = 80;  b = 120
        case (2); r = 80;  g = 30;  b = 110
        case default; r = 110; g = 50; b = 20
        end select
    end subroutine sector_palette_dim

    subroutine player_hit(gs, pos)
        type(game_state_t), intent(inout) :: gs
        type(vec3), intent(in) :: pos

        call spawn_explosion(gs, pos, 42, 0, 220, 255)
        gs%shield = gs%shield - 0.34_rk
        gs%screen_shake = max(gs%screen_shake, 0.55_rk)
        gs%message = "SHIELD IMPACT"
        gs%message_timer = 0.95_rk
        call platform_audio_beep(70.0, 0.18, 0.20)

        if (gs%shield <= 0.0_rk) then
            gs%lives = gs%lives - 1
            gs%shield = 1.0_rk
            gs%message = "HULL BREACH"
            gs%message_timer = 1.3_rk
            call platform_audio_beep(55.0, 0.28, 0.20)
        end if

        if (gs%lives <= 0) then
            gs%state = state_game_over
            gs%message = "GAME OVER"
            gs%message_timer = 99.0_rk
            call platform_audio_beep(140.0, 0.18, 0.12)
            call platform_audio_beep(90.0, 0.25, 0.10)
        end if
    end subroutine player_hit

    subroutine spawn_explosion(gs, origin, count, r, g, b)
        type(game_state_t), intent(inout) :: gs
        type(vec3), intent(in) :: origin
        integer, intent(in) :: count
        integer, intent(in) :: r
        integer, intent(in) :: g
        integer, intent(in) :: b
        integer :: spawned
        integer :: i
        real(rk) :: rx
        real(rk) :: ry
        real(rk) :: rz
        real(rk) :: speed
        type(vec3) :: dir

        spawned = 0
        do i = 1, max_particles
            if (spawned >= count) exit
            if (gs%particles(i)%active) cycle
            call random_number(rx)
            call random_number(ry)
            call random_number(rz)
            dir = vec3(rx * 2.0_rk - 1.0_rk, ry * 2.0_rk - 1.0_rk, rz * 2.0_rk - 1.0_rk)
            speed = 2.8_rk + rz * 5.0_rk
            gs%particles(i)%active = .true.
            gs%particles(i)%position = origin
            gs%particles(i)%velocity = scale3(dir, speed)
            gs%particles(i)%ttl = 0.35_rk + rx * 0.55_rk
            gs%particles(i)%max_ttl = gs%particles(i)%ttl
            gs%particles(i)%r = r
            gs%particles(i)%g = g
            gs%particles(i)%b = b
            spawned = spawned + 1
        end do
    end subroutine spawn_explosion

    subroutine render_game(gs, width, height)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height

        call platform_begin_frame()
        call render_stars(gs, width, height)
        call render_depth_grid(gs, width, height)
        call render_environment(gs, width, height)
        call render_particles(gs, width, height)
        call render_enemies(gs, width, height)

        select case (gs%state)
        case (state_title)
            call render_title(gs, width, height)
        case (state_play)
            call render_cockpit(gs, width, height)
            call render_hud(gs, width, height)
            if (gs%paused) call draw_centered_text("PAUSED", width / 2, height / 2 - 45, max(5, width / 190), 255, 255, 80, 230)
        case (state_game_over)
            call render_cockpit(gs, width, height)
            call render_hud(gs, width, height)
            call render_game_over(gs, width, height)
        end select
    end subroutine render_game

    subroutine render_stars(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(camera3) :: cam
        type(vec2) :: s1
        type(vec2) :: s2
        type(vec3) :: p1
        type(vec3) :: p2
        real(rk) :: d1
        real(rk) :: d2
        integer :: i
        integer :: shade
        logical :: ok1
        logical :: ok2

        cam = scene_camera(gs)
        do i = 1, max_stars
            p1 = vec3(gs%stars(i)%x, gs%stars(i)%y, gs%stars(i)%z)
            p2 = vec3(gs%stars(i)%x * 1.002_rk, gs%stars(i)%y * 1.002_rk, gs%stars(i)%z + 0.55_rk)
            ok1 = project_point(p1, cam, width, height, s1, d1)
            ok2 = project_point(p2, cam, width, height, s2, d2)
            if (ok1 .and. ok2) then
                shade = max(30, min(255, int(real(gs%stars(i)%shade, rk) * (1.0_rk - min(0.82_rk, d1 / 92.0_rk)))))
                call draw_line_glow(nint(s1%x), nint(s1%y), nint(s2%x), nint(s2%y), shade / 2, shade, shade, 130, 1)
            end if
        end do
    end subroutine render_stars

    subroutine render_depth_grid(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(camera3) :: cam
        type(vec2) :: a
        type(vec2) :: b
        type(vec3) :: p1
        type(vec3) :: p2
        real(rk) :: d1
        real(rk) :: d2
        real(rk) :: z
        real(rk) :: phase
        integer :: i
        logical :: ok1
        logical :: ok2

        cam = scene_camera(gs)
        phase = modulo(gs%time * 5.2_rk, 4.0_rk)

        do i = 0, 18
            z = 4.0_rk + real(i, rk) * 4.0_rk - phase
            if (z <= 2.0_rk) cycle
            p1 = vec3(-9.0_rk, -2.35_rk, z)
            p2 = vec3( 9.0_rk, -2.35_rk, z)
            ok1 = project_point(p1, cam, width, height, a, d1)
            ok2 = project_point(p2, cam, width, height, b, d2)
            if (ok1 .and. ok2) then
                call draw_line_glow(nint(a%x), nint(a%y), nint(b%x), nint(b%y), 0, 80, 120, 58, 1)
            end if
        end do

        do i = -5, 5
            p1 = vec3(real(i, rk) * 1.8_rk, -2.35_rk, 3.0_rk)
            p2 = vec3(real(i, rk) * 1.8_rk, -2.35_rk, 76.0_rk)
            ok1 = project_point(p1, cam, width, height, a, d1)
            ok2 = project_point(p2, cam, width, height, b, d2)
            if (ok1 .and. ok2) then
                call draw_line_glow(nint(a%x), nint(a%y), nint(b%x), nint(b%y), 0, 60, 105, 52, 1)
            end if
        end do
    end subroutine render_depth_grid

    subroutine render_environment(gs, width, height)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(transform3) :: xf
        type(camera3) :: cam
        integer :: line_count
        integer :: i
        real(rk) :: z
        real(rk) :: phase

        cam = scene_camera(gs)
        line_count = 0
        phase = modulo(gs%time * 4.3_rk, 16.0_rk)
        do i = 0, 5
            z = 8.0_rk + real(i, rk) * 16.0_rk - phase
            if (z < 3.5_rk) cycle
            xf%position = vec3(0.0_rk, -0.20_rk, z)
            xf%rotation = vec3(0.08_rk * sin(gs%time + i), gs%time * 0.08_rk, 0.0_rk)
            xf%scale = 1.15_rk + 0.08_rk * sin(gs%time + real(i, rk))
            call append_model_lines(gs%gate_model, xf, cam, width, height, gs%lines, line_count, max_lines, 92, 0.70_rk)
        end do

        if (mod(int(gs%time / 5.0_rk), 2) == 0) then
            xf%position = vec3(0.0_rk, 0.0_rk, 30.0_rk - modulo(gs%time * 3.0_rk, 24.0_rk))
            xf%rotation = vec3(0.0_rk, 0.0_rk, gs%time * 0.35_rk)
            xf%scale = 1.65_rk
            call append_model_lines(gs%shield_gate_model, xf, cam, width, height, gs%lines, line_count, max_lines, 80, 0.62_rk)
        end if

        call draw_screen_lines(gs%lines, line_count)
    end subroutine render_environment

    subroutine render_enemies(gs, width, height)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(camera3) :: cam
        type(transform3) :: xf
        type(vec3) :: pos
        integer :: i
        integer :: line_count
        real(rk) :: boost
        integer :: alpha

        cam = scene_camera(gs)
        line_count = 0
        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) cycle
            pos = enemy_position(gs%enemies(i))
            xf%position = pos
            xf%rotation = vec3(0.22_rk * sin(gs%enemies(i)%age), &
                               gs%enemies(i)%age * 0.55_rk + gs%enemies(i)%phase, &
                               0.26_rk * sin(gs%enemies(i)%age * 2.0_rk))
            xf%scale = enemy_scale(pos%z)
            boost = merge(2.25_rk, 1.0_rk, gs%enemies(i)%flash > 0.0_rk)
            alpha = max(105, min(255, nint(290.0_rk - pos%z * 3.2_rk)))
            call append_model_lines(gs%enemy_model, xf, cam, width, height, gs%lines, line_count, max_lines, alpha, boost)
        end do

        call draw_screen_lines(gs%lines, line_count)
    end subroutine render_enemies

    subroutine render_particles(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(camera3) :: cam
        type(vec2) :: a
        type(vec2) :: b
        type(vec3) :: tail
        real(rk) :: d1
        real(rk) :: d2
        real(rk) :: t
        integer :: i
        integer :: alpha

        cam = scene_camera(gs)
        do i = 1, max_particles
            if (.not. gs%particles(i)%active) cycle
            tail = add3(gs%particles(i)%position, scale3(gs%particles(i)%velocity, -0.055_rk))
            if (.not. project_point(gs%particles(i)%position, cam, width, height, a, d1)) cycle
            if (.not. project_point(tail, cam, width, height, b, d2)) cycle
            t = max(0.0_rk, min(1.0_rk, gs%particles(i)%ttl / gs%particles(i)%max_ttl))
            alpha = max(20, min(255, nint(255.0_rk * t)))
            call draw_line_glow(nint(a%x), nint(a%y), nint(b%x), nint(b%y), &
                gs%particles(i)%r, gs%particles(i)%g, gs%particles(i)%b, alpha, 1)
        end do
    end subroutine render_particles

    subroutine render_cockpit(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer :: cx
        integer :: cy
        integer :: bx
        integer :: by
        integer :: span
        real(rk) :: flash

        cx = nint(0.5_rk * real(width, rk) + gs%reticle_x * 0.42_rk * real(width, rk))
        cy = nint(0.5_rk * real(height, rk) - gs%reticle_y * 0.38_rk * real(height, rk))
        flash = real(gs%shot_flash / 0.11_rk)
        call draw_reticle(cx, cy, max(16, width / 45), real(flash))

        bx = width / 2
        by = height - max(42, height / 12)
        span = max(160, width / 5)
        call draw_line_glow(width / 2 - span, height - 24, width / 2 - span / 3, by, 0, 150, 255, 190, 2)
        call draw_line_glow(width / 2 + span, height - 24, width / 2 + span / 3, by, 0, 150, 255, 190, 2)
        call draw_line_glow(width / 2 - span / 3, by, width / 2 + span / 3, by, 0, 220, 255, 160, 1)
        call draw_line_glow(width / 2 - span, height - 24, width / 2 + span, height - 24, 0, 80, 140, 120, 1)

        if (gs%shot_flash > 0.0_rk) then
            call draw_line_glow(width / 2 - span / 4, by, cx, cy, 255, 250, 120, nint(230.0_rk * flash), 2)
            call draw_line_glow(width / 2 + span / 4, by, cx, cy, 255, 250, 120, nint(230.0_rk * flash), 2)
            call draw_line_glow(bx, by - 10, cx, cy, 255, 80, 40, nint(150.0_rk * flash), 1)
        end if
    end subroutine render_cockpit

    subroutine render_hud(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        character(len=32) :: score_text
        character(len=32) :: wave_text
        character(len=32) :: lives_text
        character(len=32) :: high_text
        integer :: unit

        unit = max(3, width / 285)
        write(score_text, '("SCORE ", I7.7)') gs%score
        write(wave_text, '("WAVE ", I2)') gs%wave
        write(lives_text, '("LIVES ", I1)') max(0, gs%lives)
        write(high_text, '("HIGH ", I7.7)') gs%high_score

        call draw_text(trim(score_text), 24, 22, unit, 0, 255, 210, 225)
        call draw_text(trim(high_text), 24, 58 + 7 * unit + 22 + 7 * unit, unit, 255, 210, 90, 180)
        call draw_text(trim(wave_text), width - 26 - 12 * 6 * unit, 22, unit, 255, 210, 60, 220)
        call draw_text(trim(lives_text), 24, 58 + 7 * unit, unit, 255, 120, 80, 220)
        call draw_text("SHIELD", width - 26 - 17 * 6 * unit, 58 + 7 * unit, unit, 0, 180, 255, 210)
        call draw_meter(width - 26 - 80 * unit / 2, 62 + 15 * unit, 36 * unit, max(8, 3 * unit), real(gs%shield), 0, 220, 255)

        if (gs%message_timer > 0.0_rk) then
            call draw_centered_text(trim(gs%message), width / 2, height / 6, max(4, width / 230), 255, 255, 120, &
                max(40, min(255, nint(230.0_rk * min(1.0_rk, gs%message_timer)))))
        end if
    end subroutine render_hud

    subroutine render_title(gs, width, height)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(transform3) :: xf
        type(camera3) :: cam
        integer :: line_count
        integer :: title_unit
        integer :: small_unit
        integer :: pulse

        cam = scene_camera(gs)
        line_count = 0
        xf%position = vec3(0.0_rk, 0.0_rk, 12.0_rk)
        xf%rotation = vec3(0.15_rk * sin(gs%time), gs%time * 0.75_rk, gs%time * 0.25_rk)
        xf%scale = 1.45_rk
        call append_model_lines(gs%player_model, xf, cam, width, height, gs%lines, line_count, max_lines, 190, 1.25_rk)
        call draw_screen_lines(gs%lines, line_count)

        title_unit = max(7, width / 125)
        small_unit = max(3, width / 310)
        pulse = nint(170.0_rk + 60.0_rk * sin(gs%time * 3.2_rk))

        call draw_centered_text("VECTOR STRIKE", width / 2, height / 5, title_unit, 0, 255, 235, 245)
        call draw_centered_text("77", width / 2, height / 5 + 9 * title_unit, title_unit, 255, 185, 40, 235)
        call draw_centered_text("MODERN FORTRAN / SDL2 VECTOR ARCADE", width / 2, height / 2 + height / 8, small_unit, 255, 255, 255, 190)
        if (gs%high_score > 0) then
            block
                character(len=32) :: hs_text
                write(hs_text, '("HIGH ", I7.7)') gs%high_score
                call draw_centered_text(trim(hs_text), width / 2, height / 2 + height / 8 + 10 * small_unit, &
                    small_unit, 255, 210, 90, 200)
            end block
        end if
        call draw_centered_text("PRESS ENTER OR SPACE", width / 2, height - height / 5, max(4, width / 230), 255, 235, 80, pulse)
        call draw_centered_text("MOUSE OR WASD AIM   CLICK OR SPACE FIRE   F12 CAPTURE", width / 2, height - height / 8, small_unit, 0, 190, 255, 185)
    end subroutine render_title

    subroutine render_game_over(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        character(len=36) :: score_text
        character(len=36) :: high_text
        character(len=36) :: banner
        integer :: big
        integer :: small
        logical :: new_record

        big = max(7, width / 135)
        small = max(4, width / 250)
        new_record = gs%score > 0 .and. gs%score >= gs%high_score
        write(score_text, '("FINAL SCORE ", I7.7)') gs%score
        write(high_text, '("HIGH SCORE  ", I7.7)') gs%high_score
        call draw_box(width / 2 - width / 4, height / 2 - height / 5, width / 2 + width / 4, height / 2 + height / 5, 255, 60, 80, 120)
        call draw_centered_text("GAME OVER", width / 2, height / 2 - big * 6, big, 255, 80, 80, 245)
        call draw_centered_text(trim(score_text), width / 2, height / 2 + small * 3, small, 255, 220, 90, 220)
        call draw_centered_text(trim(high_text), width / 2, height / 2 + small * 9, small, 0, 230, 255, 210)
        if (new_record) then
            banner = "NEW RECORD"
            call draw_centered_text(trim(banner), width / 2, height / 2 - big * 12, small + 1, 255, 255, 120, 235)
        end if
        call draw_centered_text("PRESS R OR ENTER", width / 2, height / 2 + small * 16, small, 0, 230, 255, 220)
    end subroutine render_game_over

    type(camera3) function scene_camera(gs)
        type(game_state_t), intent(in) :: gs
        real(rk) :: shake

        shake = gs%screen_shake
        scene_camera%position = vec3(0.045_rk * shake * sin(gs%time * 73.0_rk), &
                                     0.032_rk * shake * cos(gs%time * 59.0_rk), -0.85_rk)
        scene_camera%yaw = 0.018_rk * shake * sin(gs%time * 47.0_rk)
        scene_camera%pitch = -0.035_rk + 0.012_rk * shake * cos(gs%time * 53.0_rk)
        scene_camera%roll = 0.010_rk * shake * sin(gs%time * 61.0_rk)
        scene_camera%focal_length = 0.96_rk
        scene_camera%near_z = 0.16_rk
    end function scene_camera

    pure real(rk) function enemy_scale(z)
        real(rk), intent(in) :: z
        enemy_scale = 0.64_rk + max(0.0_rk, min(0.24_rk, (24.0_rk - z) * 0.012_rk))
    end function enemy_scale

end module game
