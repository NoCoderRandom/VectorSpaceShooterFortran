module game
    use iso_fortran_env, only: output_unit
    use vector_math, only: rk, vec2, vec3, transform3, camera3, make_vec3, add3, scale3, project_point, clamp, pi
    use platform, only: platform_init, platform_shutdown, platform_pump_events, platform_key_down, &
        platform_ticks, platform_delay, platform_get_draw_size, platform_begin_frame, platform_present, &
        platform_audio_beep, platform_audio_noise, platform_save_screenshot, platform_vsync_active, platform_mouse_state, &
        mouse_button_left, mouse_button_right, &
        key_a, key_d, key_f, key_p, key_r, key_s, key_w, &
        key_return, key_escape, key_space, key_f12, key_right, key_left, key_down, key_up, key_lshift
    use model_library, only: wire_model, screen_line, build_player_model, build_enemy_model, build_gate_model, &
        build_hunter_model, build_skimmer_model, build_striker_model, build_warden_model, &
        build_harrower_model, build_seer_model, build_maw_model, build_shield_gate_model, &
        build_buoy_model, build_shard_model, build_spine_model, &
        build_rocket_model, build_lancer_model, build_pickup_model, build_cage_model, &
        build_wreck_model, build_arc_model, build_lattice_model, &
        build_boneforge_model, build_stormveil_model, build_maw_core_model, &
        append_model_lines
    use vector_renderer, only: draw_line_glow, draw_screen_lines, draw_box, draw_reticle, draw_meter
    use vector_font, only: draw_text, draw_centered_text
    implicit none
    private

    integer, parameter :: state_title = 0
    integer, parameter :: state_play = 1
    integer, parameter :: state_game_over = 2
    integer, parameter :: state_victory = 3
    integer, parameter :: state_transmission = 4

    integer, parameter :: tx_none = 0
    integer, parameter :: tx_opening = 1
    integer, parameter :: tx_sector_two = 2
    integer, parameter :: tx_sector_three = 3
    integer, parameter :: tx_victory = 4
    integer, parameter :: tx_defeat = 5

    integer, parameter :: max_stars = 220
    integer, parameter :: max_enemies = 48
    integer, parameter :: max_particles = 720
    integer, parameter :: max_lines = 4096
    integer, parameter :: max_hazards = 32
    integer, parameter :: max_hazard_kinds = 6
    integer, parameter :: max_rockets = 8
    integer, parameter :: pattern_lancer = 6
    integer, parameter :: max_shards = 20
    integer, parameter :: shard_shield = 1
    integer, parameter :: shard_hull = 2
    integer, parameter :: shard_amber = 3
    integer, parameter :: max_shard_kinds = 3
    integer, parameter :: variant_base = 0
    integer, parameter :: variant_juggernaut = 1
    integer, parameter :: variant_phantom = 2

    integer, parameter :: particle_spark = 1
    integer, parameter :: particle_ring = 2
    integer, parameter :: particle_chunk = 3
    integer, parameter :: particle_flash = 4

    integer, parameter :: hazard_buoy = 1
    integer, parameter :: hazard_shard = 2
    integer, parameter :: hazard_spine = 3
    integer, parameter :: hazard_wreck = 4
    integer, parameter :: hazard_arc = 5
    integer, parameter :: hazard_lattice = 6

    integer, parameter :: waves_per_sector = 3
    integer, parameter :: max_sector = 6

    integer, parameter :: storm_dark = 0
    integer, parameter :: storm_preflash = 1
    integer, parameter :: storm_flash = 2
    integer, parameter :: storm_afterglow = 3

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
        real(rk) :: fire_timer = 0.0_rk
        integer :: pattern = 1
        integer :: hp = 1
        integer :: hp_max = 1
        logical :: is_boss = .false.
        integer :: boss_kind = 0
        integer :: variant = variant_base
        logical :: cage_intact = .false.
    end type enemy_t

    type :: rocket_t
        logical :: active = .false.
        type(vec3) :: position = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        type(vec3) :: velocity = vec3(0.0_rk, 0.0_rk, -6.0_rk)
        real(rk) :: age = 0.0_rk
        real(rk) :: lifetime = 5.0_rk
        real(rk) :: speed = 7.0_rk
        real(rk) :: trail_phase = 0.0_rk
    end type rocket_t

    type :: shard_t
        logical :: active = .false.
        real(rk) :: x = 0.0_rk
        real(rk) :: y = 0.0_rk
        real(rk) :: z = 6.0_rk
        real(rk) :: vx = 0.0_rk
        real(rk) :: vy = 0.0_rk
        real(rk) :: vz = -2.0_rk
        real(rk) :: age = 0.0_rk
        real(rk) :: lifetime = 4.5_rk
        real(rk) :: spin = 0.0_rk
        integer :: kind = shard_amber
    end type shard_t

    type :: particle_t
        logical :: active = .false.
        type(vec3) :: position = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        type(vec3) :: velocity = vec3(0.0_rk, 0.0_rk, 0.0_rk)
        real(rk) :: ttl = 0.0_rk
        real(rk) :: max_ttl = 1.0_rk
        real(rk) :: angle = 0.0_rk
        real(rk) :: angular_velocity = 0.0_rk
        real(rk) :: size = 1.0_rk
        integer :: kind = particle_spark
        integer :: r = 255
        integer :: g = 255
        integer :: b = 255
    end type particle_t

    type :: hazard_t
        logical :: active = .false.
        real(rk) :: x = 0.0_rk
        real(rk) :: y = 0.0_rk
        real(rk) :: z = 30.0_rk
        real(rk) :: vx = 0.0_rk
        real(rk) :: vy = 0.0_rk
        real(rk) :: speed = 6.0_rk
        real(rk) :: rot = 0.0_rk
        real(rk) :: rot_speed = 0.4_rk
        real(rk) :: tilt = 0.0_rk
        real(rk) :: scale = 1.0_rk
        real(rk) :: radius = 1.0_rk
        integer :: kind = hazard_buoy
    end type hazard_t

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
        real(rk) :: shot_chain_timer = 0.0_rk
        integer :: shot_serial = 0
        logical :: precision_aim = .false.
        real(rk) :: laser_x = 0.0_rk
        real(rk) :: laser_y = 0.0_rk
        real(rk) :: laser_target_x = 0.0_rk
        real(rk) :: laser_target_y = 0.0_rk
        logical :: laser_target_locked = .false.
        real(rk) :: spawn_timer = 0.0_rk
        real(rk) :: danger_timer = 0.0_rk
        real(rk) :: ambient_timer = 0.0_rk
        real(rk) :: coil_chatter_timer = 0.0_rk
        real(rk) :: message_timer = 0.0_rk
        real(rk) :: screen_shake = 0.0_rk
        real(rk) :: time = 0.0_rk
        real(rk) :: ship_x = 0.0_rk
        real(rk) :: ship_y = 0.0_rk
        real(rk) :: ship_vx = 0.0_rk
        real(rk) :: ship_vy = 0.0_rk
        real(rk) :: ship_iframe = 0.0_rk
        real(rk) :: hazard_flash = 0.0_rk
        real(rk) :: brace_timer = 0.0_rk
        integer :: brace_side = 0
        real(rk) :: hazard_spawn_timer = 0.0_rk
        real(rk) :: demo_ship_phase = 0.0_rk
        real(rk) :: sector_intro_timer = 0.0_rk
        real(rk) :: demo_palette_timer = 0.0_rk
        real(rk) :: sector_palette_timer = 0.0_rk
        integer :: sector_palette_from = 1
        real(rk) :: boss_intro_timer = 0.0_rk
        real(rk) :: boss_victory_timer = 0.0_rk
        real(rk) :: boss_attack_timer = 0.0_rk
        real(rk) :: boss_attack_flash = 0.0_rk
        logical :: boss_fight = .false.
        integer :: boss_cleared_sector = 0
        integer :: transmission_id = tx_none
        integer :: transmission_next_state = state_play
        integer :: transmission_visible_lines = 0
        real(rk) :: transmission_timer = 0.0_rk
        logical :: demo_mode = .false.
        logical :: paused = .false.
        character(len=48) :: message = ""
        type(star_t) :: stars(max_stars)
        type(enemy_t) :: enemies(max_enemies)
        type(particle_t) :: particles(max_particles)
        type(hazard_t) :: hazards(max_hazards)
        type(rocket_t) :: rockets(max_rockets)
        type(shard_t) :: shards(max_shards)
        real(rk) :: lancer_spawn_timer = 0.0_rk
        real(rk) :: klaxon_timer = 0.0_rk
        real(rk) :: rocket_warning_dir = 0.0_rk
        real(rk) :: shield_pickup_flash = 0.0_rk
        real(rk) :: hull_pickup_flash = 0.0_rk
        real(rk) :: amber_pickup_flash = 0.0_rk
        real(rk) :: amber_pickup_score = 0.0_rk
        integer :: streak = 0
        integer :: max_streak = 0
        integer :: streak_at_wave_start = 0
        integer :: streak_milestones = 0
        real(rk) :: streak_flash = 0.0_rk
        real(rk) :: streak_break_flash = 0.0_rk
        real(rk) :: perfect_banner_timer = 0.0_rk
        type(wire_model) :: player_model
        type(wire_model) :: enemy_models(5)
        type(wire_model) :: boss_models(max_sector)
        type(wire_model) :: gate_model
        type(wire_model) :: shield_gate_model
        type(wire_model) :: hazard_models(max_hazard_kinds)
        type(wire_model) :: rocket_model
        type(wire_model) :: lancer_model
        type(wire_model) :: pickup_models(max_shard_kinds)
        type(wire_model) :: cage_model
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

            if (gs%state == state_transmission) then
                if ((fire_pressed .and. .not. prev_fire) .or. (start_pressed .and. .not. prev_start)) then
                    call continue_transmission(gs)
                end if
            end if

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
        call build_enemy_model(gs%enemy_models(1))
        call build_hunter_model(gs%enemy_models(2))
        call build_skimmer_model(gs%enemy_models(3))
        call build_striker_model(gs%enemy_models(4))
        call build_warden_model(gs%enemy_models(5))
        call build_harrower_model(gs%boss_models(1))
        call build_seer_model(gs%boss_models(2))
        call build_maw_model(gs%boss_models(3))
        call build_boneforge_model(gs%boss_models(4))
        call build_stormveil_model(gs%boss_models(5))
        call build_maw_core_model(gs%boss_models(6))
        call build_gate_model(gs%gate_model)
        call build_shield_gate_model(gs%shield_gate_model)
        call build_buoy_model(gs%hazard_models(hazard_buoy))
        call build_shard_model(gs%hazard_models(hazard_shard))
        call build_spine_model(gs%hazard_models(hazard_spine))
        call build_wreck_model(gs%hazard_models(hazard_wreck))
        call build_arc_model(gs%hazard_models(hazard_arc))
        call build_lattice_model(gs%hazard_models(hazard_lattice))
        call build_rocket_model(gs%rocket_model)
        call build_lancer_model(gs%lancer_model)
        call build_pickup_model(gs%pickup_models(shard_shield), 80, 220, 255)
        call build_pickup_model(gs%pickup_models(shard_hull), 120, 255, 140)
        call build_pickup_model(gs%pickup_models(shard_amber), 255, 200, 80)
        call build_cage_model(gs%cage_model)
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
        gs%sector_palette_from = 1
        gs%sector_palette_timer = 0.0_rk
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
        gs%shot_chain_timer = 0.0_rk
        gs%shot_serial = 0
        gs%precision_aim = .false.
        gs%laser_target_x = 0.0_rk
        gs%laser_target_y = 0.0_rk
        gs%laser_target_locked = .false.
        gs%spawn_timer = 0.05_rk
        gs%danger_timer = 0.0_rk
        gs%ambient_timer = 0.0_rk
        gs%coil_chatter_timer = 0.8_rk
        gs%message_timer = 2.0_rk
        gs%sector_intro_timer = 2.6_rk
        gs%demo_palette_timer = 0.0_rk
        gs%sector_palette_timer = 0.0_rk
        gs%boss_intro_timer = 0.0_rk
        gs%boss_victory_timer = 0.0_rk
        gs%boss_attack_timer = 0.0_rk
        gs%boss_attack_flash = 0.0_rk
        gs%boss_fight = .false.
        gs%boss_cleared_sector = 0
        gs%transmission_id = tx_none
        gs%transmission_next_state = state_play
        gs%transmission_visible_lines = 0
        gs%transmission_timer = 0.0_rk
        if (demo_mode) then
            gs%message = "DEMO WAVE ONLINE"
        else
            gs%message = "PHOSPHOR-LEAD CLEARED FOR LAUNCH"
        end if
        gs%paused = .false.
        gs%ship_x = 0.0_rk
        gs%ship_y = 0.0_rk
        gs%ship_vx = 0.0_rk
        gs%ship_vy = 0.0_rk
        gs%ship_iframe = 0.0_rk
        gs%hazard_flash = 0.0_rk
        gs%brace_timer = 0.0_rk
        gs%brace_side = 0
        gs%hazard_spawn_timer = 2.0_rk
        gs%demo_ship_phase = 0.0_rk
        do i = 1, max_enemies
            gs%enemies(i)%active = .false.
        end do
        do i = 1, max_particles
            gs%particles(i)%active = .false.
        end do
        do i = 1, max_hazards
            gs%hazards(i)%active = .false.
        end do
        do i = 1, max_rockets
            gs%rockets(i)%active = .false.
        end do
        do i = 1, max_shards
            gs%shards(i)%active = .false.
        end do
        gs%lancer_spawn_timer = 6.0_rk
        gs%klaxon_timer = 0.0_rk
        gs%rocket_warning_dir = 0.0_rk
        gs%shield_pickup_flash = 0.0_rk
        gs%hull_pickup_flash = 0.0_rk
        gs%amber_pickup_flash = 0.0_rk
        gs%amber_pickup_score = 0.0_rk
        gs%streak = 0
        gs%max_streak = 0
        gs%streak_at_wave_start = 0
        gs%streak_milestones = 0
        gs%streak_flash = 0.0_rk
        gs%streak_break_flash = 0.0_rk
        gs%perfect_banner_timer = 0.0_rk
        do i = 1, 5
            call spawn_enemy(gs, 18.0_rk + real(i, rk) * 4.2_rk)
        end do
        if (.not. demo_mode) call start_transmission(gs, tx_opening, state_play)
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
        gs%shot_chain_timer = max(0.0_rk, gs%shot_chain_timer - dt)
        if (gs%shot_flash <= 0.0_rk) gs%laser_target_locked = .false.
        gs%danger_timer = max(0.0_rk, gs%danger_timer - dt)
        gs%message_timer = max(0.0_rk, gs%message_timer - dt)
        gs%screen_shake = max(0.0_rk, gs%screen_shake - dt * 1.8_rk)
        gs%ship_iframe = max(0.0_rk, gs%ship_iframe - dt)
        gs%hazard_flash = max(0.0_rk, gs%hazard_flash - dt * 2.4_rk)
        gs%brace_timer = max(0.0_rk, gs%brace_timer - dt)
        gs%shield_pickup_flash = max(0.0_rk, gs%shield_pickup_flash - dt * 2.0_rk)
        gs%hull_pickup_flash = max(0.0_rk, gs%hull_pickup_flash - dt * 2.0_rk)
        gs%amber_pickup_flash = max(0.0_rk, gs%amber_pickup_flash - dt * 1.6_rk)
        gs%streak_flash = max(0.0_rk, gs%streak_flash - dt * 2.5_rk)
        gs%streak_break_flash = max(0.0_rk, gs%streak_break_flash - dt * 1.4_rk)
        gs%perfect_banner_timer = max(0.0_rk, gs%perfect_banner_timer - dt)
        if (gs%state == state_play) gs%sector_intro_timer = max(0.0_rk, gs%sector_intro_timer - dt)
        gs%sector_palette_timer = max(0.0_rk, gs%sector_palette_timer - dt)
        gs%boss_attack_flash = max(0.0_rk, gs%boss_attack_flash - dt * 5.0_rk)

        call update_boss_timers(gs, dt)

        call update_stars(gs, dt, merge(9.0_rk, 3.2_rk, gs%state == state_play))

        select case (gs%state)
        case (state_title)
            call update_title_scene(gs, dt)
        case (state_play)
            call update_player_control(gs, dt, width, height, mouse_x, mouse_y, mouse_moved, mouse_buttons)
            call update_ship_movement(gs, dt)
            call update_hazards(gs, dt)
            call update_enemies(gs, dt)
            call update_lancer_fire(gs, dt)
            call update_rockets(gs, dt)
            call update_shards(gs, dt)
            call update_particles(gs, dt)
            call update_spawning(gs, dt)
            call update_hazard_spawning(gs, dt)
            call update_lancer_spawning(gs, dt)
            call update_ambient_audio(gs, dt)
            call update_proximity_audio(gs)
            call update_rocket_audio(gs, dt)
            call update_coil_chatter(gs, dt)
            call update_boss_attack(gs, dt)
            call update_demo_autopilot(gs, dt, width, height)
            if (gs%state == state_play .and. &
                (fire_edge .or. (gs%demo_mode .and. gs%fire_cooldown <= 0.0_rk)) .and. gs%fire_cooldown <= 0.0_rk) then
                call fire_weapon(gs, width, height)
            end if
        case (state_game_over)
            call update_enemies(gs, dt * 0.35_rk)
            call update_particles(gs, dt)
        case (state_victory)
            call update_enemies(gs, dt * 0.25_rk)
            call update_particles(gs, dt)
        case (state_transmission)
            call update_transmission(gs, dt)
        end select
    end subroutine update_game

    subroutine start_transmission(gs, transmission_id, next_state)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: transmission_id
        integer, intent(in) :: next_state

        if (gs%demo_mode) then
            gs%state = next_state
            return
        end if

        gs%state = state_transmission
        gs%transmission_id = transmission_id
        gs%transmission_next_state = next_state
        gs%transmission_visible_lines = 0
        gs%transmission_timer = 0.18_rk
        gs%message_timer = 0.0_rk
    end subroutine start_transmission

    subroutine update_transmission(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer :: line_count

        line_count = transmission_line_count(gs%transmission_id)
        if (gs%transmission_visible_lines >= line_count) return

        gs%transmission_timer = gs%transmission_timer - dt
        if (gs%transmission_timer <= 0.0_rk) then
            gs%transmission_visible_lines = min(line_count, gs%transmission_visible_lines + 1)
            gs%transmission_timer = 0.72_rk
            call platform_audio_beep(880.0, 0.014, 0.040)
            call platform_audio_noise(1800.0, 0.018, 0.030, 3.0)
        end if
    end subroutine update_transmission

    subroutine continue_transmission(gs)
        type(game_state_t), intent(inout) :: gs
        integer :: line_count

        line_count = transmission_line_count(gs%transmission_id)
        if (gs%transmission_visible_lines < line_count) then
            gs%transmission_visible_lines = line_count
            return
        end if

        gs%state = gs%transmission_next_state
        gs%transmission_id = tx_none
        gs%transmission_visible_lines = 0
        gs%transmission_timer = 0.0_rk

        select case (gs%state)
        case (state_play)
            gs%message = sector_name(gs%sector)
            gs%message_timer = 1.2_rk
        case (state_game_over)
            gs%message = "GAME OVER"
            gs%message_timer = 99.0_rk
        case (state_victory)
            gs%message = "GATE IS DARK"
            gs%message_timer = 99.0_rk
        end select
        call platform_audio_beep(440.0, 0.06, 0.10)
    end subroutine continue_transmission

    subroutine update_boss_timers(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        logical :: intro_was_active

        if (gs%state /= state_play) return

        intro_was_active = gs%boss_intro_timer > 0.0_rk
        gs%boss_intro_timer = max(0.0_rk, gs%boss_intro_timer - dt)
        if (intro_was_active .and. gs%boss_intro_timer <= 0.0_rk .and. gs%boss_fight) then
            gs%message = trim(boss_name(gs%sector)) // " ENGAGED"
            gs%message_timer = 1.1_rk
            gs%boss_attack_timer = min(gs%boss_attack_timer, 1.25_rk)
            call platform_audio_beep(260.0, 0.14, 0.16)
            call platform_audio_beep(520.0, 0.09, 0.12)
        end if

        if (gs%boss_victory_timer > 0.0_rk) then
            gs%boss_victory_timer = max(0.0_rk, gs%boss_victory_timer - dt)
            if (gs%boss_victory_timer <= 0.0_rk) call complete_boss_victory(gs)
        end if
    end subroutine update_boss_timers

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
        logical :: aim_right
        logical :: aim_left
        logical :: aim_up
        logical :: aim_down
        logical :: precision
        logical :: right_mouse

        axis_x = 0.0_rk
        axis_y = 0.0_rk

        aim_right = platform_key_down(key_right)
        aim_left = platform_key_down(key_left)
        aim_up = platform_key_down(key_up)
        aim_down = platform_key_down(key_down)
        right_mouse = iand(mouse_buttons, mouse_button_right) /= 0
        precision = platform_key_down(key_lshift) .or. right_mouse
        gs%precision_aim = precision

        if (aim_right) axis_x = axis_x + 1.0_rk
        if (aim_left) axis_x = axis_x - 1.0_rk
        if (aim_up) axis_y = axis_y + 1.0_rk
        if (aim_down) axis_y = axis_y - 1.0_rk

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

    subroutine update_ship_movement(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk), parameter :: envelope_x = 1.55_rk
        real(rk), parameter :: envelope_y = 0.95_rk
        real(rk), parameter :: accel = 18.0_rk
        real(rk), parameter :: drag = 6.5_rk
        real(rk), parameter :: max_v = 5.5_rk
        real(rk) :: axis_x
        real(rk) :: axis_y
        logical :: right
        logical :: left
        logical :: up
        logical :: down

        if (gs%demo_mode) return

        right = platform_key_down(key_d)
        left = platform_key_down(key_a)
        up = platform_key_down(key_w)
        down = platform_key_down(key_s)

        axis_x = 0.0_rk
        axis_y = 0.0_rk
        if (right) axis_x = axis_x + 1.0_rk
        if (left) axis_x = axis_x - 1.0_rk
        if (up) axis_y = axis_y + 1.0_rk
        if (down) axis_y = axis_y - 1.0_rk

        gs%ship_vx = gs%ship_vx + axis_x * accel * dt
        gs%ship_vy = gs%ship_vy + axis_y * accel * dt
        gs%ship_vx = gs%ship_vx - gs%ship_vx * min(1.0_rk, drag * dt)
        gs%ship_vy = gs%ship_vy - gs%ship_vy * min(1.0_rk, drag * dt)
        gs%ship_vx = clamp(gs%ship_vx, -max_v, max_v)
        gs%ship_vy = clamp(gs%ship_vy, -max_v, max_v)

        gs%ship_x = gs%ship_x + gs%ship_vx * dt
        gs%ship_y = gs%ship_y + gs%ship_vy * dt
        if (gs%ship_x > envelope_x) then
            gs%ship_x = envelope_x
            gs%ship_vx = min(0.0_rk, gs%ship_vx)
        else if (gs%ship_x < -envelope_x) then
            gs%ship_x = -envelope_x
            gs%ship_vx = max(0.0_rk, gs%ship_vx)
        end if
        if (gs%ship_y > envelope_y) then
            gs%ship_y = envelope_y
            gs%ship_vy = min(0.0_rk, gs%ship_vy)
        else if (gs%ship_y < -envelope_y) then
            gs%ship_y = -envelope_y
            gs%ship_vy = max(0.0_rk, gs%ship_vy)
        end if
    end subroutine update_ship_movement

    subroutine update_hazards(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk), parameter :: collide_z_lo = 0.45_rk
        real(rk), parameter :: collide_z_hi = 1.95_rk
        real(rk), parameter :: brace_z_hi = 6.0_rk
        real(rk), parameter :: brace_threshold = 0.85_rk
        real(rk) :: dx
        real(rk) :: dy
        real(rk) :: hit_radius
        real(rk) :: z_was
        integer :: i
        type(vec3) :: pos

        if (gs%state /= state_play) return
        gs%brace_side = 0

        do i = 1, max_hazards
            if (.not. gs%hazards(i)%active) cycle
            z_was = gs%hazards(i)%z
            gs%hazards(i)%z = gs%hazards(i)%z - gs%hazards(i)%speed * dt
            gs%hazards(i)%x = gs%hazards(i)%x + gs%hazards(i)%vx * dt
            gs%hazards(i)%y = gs%hazards(i)%y + gs%hazards(i)%vy * dt
            gs%hazards(i)%rot = gs%hazards(i)%rot + gs%hazards(i)%rot_speed * dt
            gs%hazards(i)%tilt = gs%hazards(i)%tilt + gs%hazards(i)%rot_speed * 0.5_rk * dt

            if (gs%hazards(i)%z < -1.5_rk) then
                gs%hazards(i)%active = .false.
                cycle
            end if

            hit_radius = gs%hazards(i)%radius * gs%hazards(i)%scale
            dx = gs%hazards(i)%x - gs%ship_x
            dy = gs%hazards(i)%y - gs%ship_y

            if (gs%hazards(i)%z <= brace_z_hi .and. gs%hazards(i)%z > collide_z_hi) then
                if (abs(dx) < hit_radius + brace_threshold .and. abs(dy) < hit_radius + brace_threshold * 0.7_rk) then
                    gs%brace_timer = max(gs%brace_timer, 0.25_rk)
                    if (dx > 0.05_rk) then
                        gs%brace_side = 1
                    else if (dx < -0.05_rk) then
                        gs%brace_side = -1
                    else
                        gs%brace_side = 0
                    end if
                end if
            end if

            if (z_was > collide_z_hi .and. gs%hazards(i)%z <= collide_z_hi .and. gs%hazards(i)%z >= collide_z_lo) then
                if (abs(dx) < hit_radius .and. abs(dy) < hit_radius * 0.82_rk) then
                    if (gs%ship_iframe <= 0.0_rk) then
                        pos = vec3(gs%hazards(i)%x, gs%hazards(i)%y, gs%hazards(i)%z)
                        call hazard_impact(gs, pos, gs%hazards(i)%kind)
                        gs%ship_iframe = 0.85_rk
                        gs%ship_vx = gs%ship_vx - sign(3.5_rk, dx)
                        gs%ship_vy = gs%ship_vy - sign(1.8_rk, dy)
                    end if
                    gs%hazards(i)%active = .false.
                end if
            end if
        end do
    end subroutine update_hazards

    subroutine hazard_impact(gs, pos, kind)
        type(game_state_t), intent(inout) :: gs
        type(vec3), intent(in) :: pos
        integer, intent(in) :: kind
        integer :: r
        integer :: g
        integer :: b

        select case (kind)
        case (hazard_buoy)
            r = 140; g = 220; b = 255
        case (hazard_shard)
            r = 220; g = 100; b = 220
        case (hazard_spine)
            r = 255; g = 160; b = 80
        case default
            r = 200; g = 200; b = 255
        end select
        call spawn_explosion(gs, pos, 22, r, g, b)
        gs%hazard_flash = 1.0_rk
        call player_hit(gs, pos)
    end subroutine hazard_impact

    subroutine update_lancer_spawning(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer :: i
        integer :: lancer_count
        integer :: cap
        real(rk) :: interval
        real(rk) :: rx
        real(rk) :: ry
        real(rk) :: rs

        if (gs%state /= state_play) return
        if (gs%boss_fight .or. gs%boss_intro_timer > 0.0_rk) return
        if (gs%wave < 2 .and. gs%sector == 1) return

        lancer_count = 0
        do i = 1, max_enemies
            if (gs%enemies(i)%active .and. gs%enemies(i)%pattern == pattern_lancer) lancer_count = lancer_count + 1
        end do

        if (gs%sector <= 2) then
            cap = 2
        else
            cap = 3
        end if
        if (lancer_count >= cap) return

        gs%lancer_spawn_timer = gs%lancer_spawn_timer - dt
        if (gs%lancer_spawn_timer > 0.0_rk) return

        interval = merge(9.0_rk, 6.5_rk, gs%sector <= 2)
        call random_number(rx)
        gs%lancer_spawn_timer = interval + rx * 3.0_rk

        call random_number(ry)
        call random_number(rs)
        call spawn_lancer(gs, ry, rs)
    end subroutine update_lancer_spawning

    subroutine spawn_lancer(gs, rx_in, rs_in)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: rx_in
        real(rk), intent(in) :: rs_in
        integer :: slot
        integer :: i
        real(rk) :: sign

        slot = 0
        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) then
                slot = i
                exit
            end if
        end do
        if (slot == 0) return

        sign = merge(1.0_rk, -1.0_rk, rx_in > 0.5_rk)
        gs%spawn_serial = gs%spawn_serial + 1

        gs%enemies(slot)%active = .true.
        gs%enemies(slot)%pattern = pattern_lancer
        gs%enemies(slot)%age = 0.0_rk
        gs%enemies(slot)%phase = rs_in * 2.0_rk * pi
        gs%enemies(slot)%flash = 0.0_rk
        gs%enemies(slot)%hp = 2
        gs%enemies(slot)%hp_max = 2
        gs%enemies(slot)%is_boss = .false.
        gs%enemies(slot)%boss_kind = 0
        gs%enemies(slot)%x = sign * (1.6_rk + rx_in * 0.8_rk)
        gs%enemies(slot)%y = (rs_in - 0.5_rk) * 1.4_rk
        gs%enemies(slot)%z = 14.0_rk + rs_in * 4.0_rk
        gs%enemies(slot)%vx = -sign * 0.25_rk
        gs%enemies(slot)%vy = 0.0_rk
        gs%enemies(slot)%speed = 0.75_rk + rs_in * 0.45_rk
        gs%enemies(slot)%fire_timer = 2.6_rk + rx_in * 1.4_rk
    end subroutine spawn_lancer

    subroutine update_lancer_fire(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer :: i
        integer :: rockets_in_flight
        integer :: cap
        type(vec3) :: lancer_pos
        real(rk) :: reload

        if (gs%state /= state_play) return

        rockets_in_flight = 0
        do i = 1, max_rockets
            if (gs%rockets(i)%active) rockets_in_flight = rockets_in_flight + 1
        end do
        if (gs%sector <= 2) then
            cap = 3
        else
            cap = 5
        end if

        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) cycle
            if (gs%enemies(i)%pattern /= pattern_lancer) cycle
            if (gs%enemies(i)%is_boss) cycle
            gs%enemies(i)%fire_timer = gs%enemies(i)%fire_timer - dt
            if (gs%enemies(i)%fire_timer > 0.0_rk) cycle
            if (rockets_in_flight >= cap) then
                gs%enemies(i)%fire_timer = 0.6_rk
                cycle
            end if

            lancer_pos = enemy_position(gs%enemies(i))
            call spawn_rocket(gs, lancer_pos)
            rockets_in_flight = rockets_in_flight + 1
            reload = merge(3.6_rk, 2.8_rk, gs%sector <= 2)
            call random_number(reload)
            gs%enemies(i)%fire_timer = merge(3.6_rk, 2.6_rk, gs%sector <= 2) + reload * 1.4_rk
        end do
    end subroutine update_lancer_fire

    subroutine spawn_rocket(gs, origin)
        type(game_state_t), intent(inout) :: gs
        type(vec3), intent(in) :: origin
        integer :: slot
        integer :: i
        real(rk) :: dx
        real(rk) :: dy
        real(rk) :: dz
        real(rk) :: speed0

        slot = 0
        do i = 1, max_rockets
            if (.not. gs%rockets(i)%active) then
                slot = i
                exit
            end if
        end do
        if (slot == 0) return

        dx = gs%ship_x - origin%x
        dy = gs%ship_y - origin%y
        dz = -origin%z
        speed0 = 5.6_rk

        gs%rockets(slot)%active = .true.
        gs%rockets(slot)%position = origin
        gs%rockets(slot)%age = 0.0_rk
        gs%rockets(slot)%lifetime = 4.2_rk
        gs%rockets(slot)%speed = speed0
        gs%rockets(slot)%trail_phase = 0.0_rk
        gs%rockets(slot)%velocity = scale3(vec3(dx, dy, dz), speed0 / max(0.1_rk, sqrt(dx * dx + dy * dy + dz * dz)))

        call platform_audio_beep(190.0, 0.05, 0.09)
        call platform_audio_noise(1400.0, 0.10, 0.06, 2.2)
    end subroutine spawn_rocket

    subroutine update_rockets(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk), parameter :: turn_rate = 2.6_rk
        real(rk), parameter :: hit_radius = 0.75_rk
        real(rk) :: dx
        real(rk) :: dy
        real(rk) :: dz
        real(rk) :: inv
        real(rk) :: vx_new
        real(rk) :: vy_new
        real(rk) :: vz_new
        real(rk) :: speed_mag
        type(vec3) :: target
        integer :: i

        if (gs%state /= state_play) return

        do i = 1, max_rockets
            if (.not. gs%rockets(i)%active) cycle
            gs%rockets(i)%age = gs%rockets(i)%age + dt
            gs%rockets(i)%trail_phase = gs%rockets(i)%trail_phase + dt
            if (gs%rockets(i)%age > gs%rockets(i)%lifetime) then
                gs%rockets(i)%active = .false.
                cycle
            end if

            target = vec3(gs%ship_x, gs%ship_y, 0.0_rk)
            dx = target%x - gs%rockets(i)%position%x
            dy = target%y - gs%rockets(i)%position%y
            dz = target%z - gs%rockets(i)%position%z
            inv = 1.0_rk / max(0.1_rk, sqrt(dx * dx + dy * dy + dz * dz))

            vx_new = gs%rockets(i)%velocity%x + dx * inv * turn_rate * dt
            vy_new = gs%rockets(i)%velocity%y + dy * inv * turn_rate * dt
            vz_new = gs%rockets(i)%velocity%z + dz * inv * turn_rate * dt
            speed_mag = sqrt(vx_new * vx_new + vy_new * vy_new + vz_new * vz_new)
            if (speed_mag > 0.0_rk) then
                gs%rockets(i)%velocity%x = vx_new / speed_mag * gs%rockets(i)%speed
                gs%rockets(i)%velocity%y = vy_new / speed_mag * gs%rockets(i)%speed
                gs%rockets(i)%velocity%z = vz_new / speed_mag * gs%rockets(i)%speed
            end if

            gs%rockets(i)%position = add3(gs%rockets(i)%position, scale3(gs%rockets(i)%velocity, dt))

            if (gs%rockets(i)%position%z < 1.3_rk) then
                dx = gs%rockets(i)%position%x - gs%ship_x
                dy = gs%rockets(i)%position%y - gs%ship_y
                if (sqrt(dx * dx + dy * dy) < hit_radius .and. gs%rockets(i)%position%z > -0.8_rk) then
                    if (gs%ship_iframe <= 0.0_rk) then
                        call spawn_explosion(gs, gs%rockets(i)%position, 28, 255, 120, 60)
                        gs%ship_iframe = 0.7_rk
                        call player_hit(gs, gs%rockets(i)%position)
                    end if
                    gs%rockets(i)%active = .false.
                    cycle
                end if
                if (gs%rockets(i)%position%z < -1.2_rk) then
                    gs%rockets(i)%active = .false.
                end if
            end if
        end do
    end subroutine update_rockets

    subroutine update_rocket_audio(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer :: i
        real(rk) :: nearest_z
        real(rk) :: nearest_dir
        real(rk) :: pitch
        real(rk) :: interval

        if (gs%state /= state_play) return

        nearest_z = huge(1.0_rk)
        nearest_dir = 0.0_rk
        do i = 1, max_rockets
            if (.not. gs%rockets(i)%active) cycle
            if (gs%rockets(i)%position%z < nearest_z) then
                nearest_z = gs%rockets(i)%position%z
                nearest_dir = gs%rockets(i)%position%x - gs%ship_x
            end if
        end do

        if (nearest_z > 12.0_rk) then
            gs%klaxon_timer = 0.0_rk
            gs%rocket_warning_dir = 0.0_rk
            return
        end if

        gs%rocket_warning_dir = nearest_dir

        gs%klaxon_timer = gs%klaxon_timer - dt
        if (gs%klaxon_timer > 0.0_rk) return

        pitch = 380.0_rk + (12.0_rk - max(0.0_rk, nearest_z)) * 55.0_rk
        interval = merge(0.16_rk, 0.30_rk, nearest_z < 5.0_rk)
        gs%klaxon_timer = interval
        call platform_audio_beep(real(pitch), 0.040, 0.080)
    end subroutine update_rocket_audio

    subroutine update_shards(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk), parameter :: magnet_range = 4.5_rk
        real(rk), parameter :: magnet_strength = 2.6_rk
        real(rk), parameter :: pickup_radius = 0.85_rk
        real(rk) :: dx
        real(rk) :: dy
        real(rk) :: dz
        real(rk) :: dist
        real(rk) :: inv
        integer :: i

        if (gs%state /= state_play) return

        do i = 1, max_shards
            if (.not. gs%shards(i)%active) cycle
            gs%shards(i)%age = gs%shards(i)%age + dt
            if (gs%shards(i)%age >= gs%shards(i)%lifetime) then
                gs%shards(i)%active = .false.
                cycle
            end if

            dx = gs%ship_x - gs%shards(i)%x
            dy = gs%ship_y - gs%shards(i)%y
            dz = 0.0_rk - gs%shards(i)%z
            dist = sqrt(dx * dx + dy * dy + dz * dz)
            if (dist < magnet_range .and. dist > 0.05_rk) then
                inv = magnet_strength / dist
                gs%shards(i)%vx = gs%shards(i)%vx + dx * inv * dt
                gs%shards(i)%vy = gs%shards(i)%vy + dy * inv * dt
            end if
            gs%shards(i)%vz = gs%shards(i)%vz * max(0.0_rk, 1.0_rk - 0.35_rk * dt)
            gs%shards(i)%x = gs%shards(i)%x + gs%shards(i)%vx * dt
            gs%shards(i)%y = gs%shards(i)%y + gs%shards(i)%vy * dt
            gs%shards(i)%z = gs%shards(i)%z + gs%shards(i)%vz * dt
            gs%shards(i)%spin = gs%shards(i)%spin + dt * 4.5_rk

            if (gs%shards(i)%z < -2.0_rk) then
                gs%shards(i)%active = .false.
                cycle
            end if

            if (gs%shards(i)%z < 1.6_rk .and. gs%shards(i)%z > -0.6_rk) then
                dx = gs%shards(i)%x - gs%ship_x
                dy = gs%shards(i)%y - gs%ship_y
                if (sqrt(dx * dx + dy * dy) < pickup_radius) then
                    call collect_shard(gs, i)
                end if
            end if
        end do
    end subroutine update_shards

    subroutine collect_shard(gs, index)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: index
        integer :: amber_points

        if (.not. gs%shards(index)%active) return

        select case (gs%shards(index)%kind)
        case (shard_shield)
            gs%shield = min(1.0_rk, gs%shield + 0.28_rk)
            gs%shield_pickup_flash = 1.0_rk
            call platform_audio_beep(720.0, 0.055, 0.12)
            call platform_audio_beep(960.0, 0.040, 0.09)
        case (shard_hull)
            gs%lives = min(5, gs%lives + 1)
            gs%hull_pickup_flash = 1.0_rk
            gs%message = "HULL RESTORED"
            gs%message_timer = 1.1_rk
            call platform_audio_beep(520.0, 0.08, 0.14)
            call platform_audio_beep(780.0, 0.06, 0.10)
        case (shard_amber)
            amber_points = nint(180.0_rk * sector_score_mult(gs%sector))
            gs%score = gs%score + amber_points
            gs%amber_pickup_flash = 1.0_rk
            gs%amber_pickup_score = real(amber_points, rk)
            if (gs%score > gs%high_score) then
                gs%high_score = gs%score
                if (.not. gs%demo_mode) call save_high_score(gs%high_score)
            end if
            call platform_audio_beep(1050.0, 0.045, 0.10)
        end select

        gs%shards(index)%active = .false.
    end subroutine collect_shard

    subroutine maybe_drop_shard(gs, pos, kill_value)
        type(game_state_t), intent(inout) :: gs
        type(vec3), intent(in) :: pos
        integer, intent(in) :: kill_value
        real(rk) :: roll

        call random_number(roll)
        if (kill_value == 2) then
            call spawn_shard(gs, pos, shard_hull)
            return
        end if
        if (roll < 0.10_rk) then
            call spawn_shard(gs, pos, shard_shield)
        else if (roll < 0.32_rk) then
            call spawn_shard(gs, pos, shard_amber)
        end if
    end subroutine maybe_drop_shard

    subroutine spawn_shard(gs, pos, kind)
        type(game_state_t), intent(inout) :: gs
        type(vec3), intent(in) :: pos
        integer, intent(in) :: kind
        integer :: slot
        integer :: i
        real(rk) :: rx
        real(rk) :: ry
        real(rk) :: rz

        slot = 0
        do i = 1, max_shards
            if (.not. gs%shards(i)%active) then
                slot = i
                exit
            end if
        end do
        if (slot == 0) return

        call random_number(rx)
        call random_number(ry)
        call random_number(rz)

        gs%shards(slot)%active = .true.
        gs%shards(slot)%kind = kind
        gs%shards(slot)%x = pos%x + (rx - 0.5_rk) * 0.4_rk
        gs%shards(slot)%y = pos%y + (ry - 0.5_rk) * 0.4_rk
        gs%shards(slot)%z = max(3.0_rk, pos%z)
        gs%shards(slot)%vx = (rx - 0.5_rk) * 0.6_rk
        gs%shards(slot)%vy = (ry - 0.5_rk) * 0.6_rk
        gs%shards(slot)%vz = -1.6_rk - rz * 1.2_rk
        gs%shards(slot)%age = 0.0_rk
        gs%shards(slot)%lifetime = 4.8_rk + rz * 1.2_rk
        gs%shards(slot)%spin = rx * 2.0_rk * pi
    end subroutine spawn_shard

    subroutine render_shards(gs, width, height)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(camera3) :: cam
        type(transform3) :: xf
        integer :: line_count
        integer :: alpha
        integer :: i
        integer :: kind
        real(rk) :: fade
        real(rk) :: boost

        cam = scene_camera(gs)
        line_count = 0

        do i = 1, max_shards
            if (.not. gs%shards(i)%active) cycle
            kind = gs%shards(i)%kind
            if (kind < 1 .or. kind > max_shard_kinds) kind = shard_amber
            xf%position = vec3(gs%shards(i)%x, gs%shards(i)%y, gs%shards(i)%z)
            xf%rotation = vec3(gs%shards(i)%spin * 0.7_rk, gs%shards(i)%spin, gs%shards(i)%spin * 0.4_rk)
            xf%scale = 0.8_rk + 0.12_rk * sin(gs%shards(i)%spin * 2.0_rk)
            fade = 1.0_rk - max(0.0_rk, (gs%shards(i)%age - gs%shards(i)%lifetime + 0.8_rk)) / 0.8_rk
            fade = min(1.0_rk, max(0.15_rk, fade))
            alpha = max(60, min(255, nint(240.0_rk * fade)))
            boost = 1.0_rk + 0.4_rk * (0.5_rk + 0.5_rk * sin(gs%shards(i)%spin * 3.0_rk))
            call append_model_lines(gs%pickup_models(kind), xf, cam, width, height, &
                gs%lines, line_count, max_lines, alpha, boost)
        end do
        call draw_screen_lines(gs%lines, line_count)
    end subroutine render_shards

    subroutine update_hazard_spawning(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk) :: interval
        real(rk) :: rx
        real(rk) :: ry
        real(rk) :: rz

        if (gs%state /= state_play) return
        if (gs%boss_intro_timer > 0.0_rk) return

        gs%hazard_spawn_timer = gs%hazard_spawn_timer - dt
        if (gs%hazard_spawn_timer > 0.0_rk) return

        select case (gs%sector)
        case (1)
            interval = 2.4_rk
        case (2)
            interval = 0.95_rk
        case (3)
            interval = 1.7_rk
        case (4)
            interval = 1.1_rk
        case (5)
            interval = 1.6_rk
        case (6)
            interval = 0.70_rk
        case default
            interval = 1.5_rk
        end select

        call random_number(rx)
        call random_number(ry)
        call random_number(rz)
        gs%hazard_spawn_timer = interval * (0.75_rk + 0.5_rk * rx)
        call spawn_hazard(gs, ry, rz)
    end subroutine update_hazard_spawning

    subroutine spawn_hazard(gs, rx_in, ry_in)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: rx_in
        real(rk), intent(in) :: ry_in
        integer :: slot
        integer :: i
        integer :: kind
        real(rk) :: x_spread
        real(rk) :: y_spread
        real(rk) :: rs
        real(rk) :: rv

        slot = 0
        do i = 1, max_hazards
            if (.not. gs%hazards(i)%active) then
                slot = i
                exit
            end if
        end do
        if (slot == 0) return

        select case (gs%sector)
        case (1)
            kind = hazard_buoy
            x_spread = 2.2_rk
            y_spread = 1.10_rk
        case (2)
            kind = hazard_shard
            x_spread = 2.4_rk
            y_spread = 1.25_rk
        case (3)
            kind = hazard_spine
            x_spread = 2.0_rk
            y_spread = 0.50_rk
        case (4)
            kind = hazard_wreck
            x_spread = 2.6_rk
            y_spread = 1.40_rk
        case (5)
            kind = hazard_arc
            x_spread = 2.4_rk
            y_spread = 1.20_rk
        case (6)
            kind = hazard_lattice
            x_spread = 2.1_rk
            y_spread = 1.00_rk
        case default
            kind = hazard_shard
            x_spread = 2.3_rk
            y_spread = 1.20_rk
        end select

        call random_number(rs)
        call random_number(rv)

        gs%hazards(slot)%active = .true.
        gs%hazards(slot)%kind = kind
        gs%hazards(slot)%x = (rx_in * 2.0_rk - 1.0_rk) * x_spread
        gs%hazards(slot)%y = (ry_in * 2.0_rk - 1.0_rk) * y_spread
        if (kind == hazard_spine) gs%hazards(slot)%y = -0.40_rk + ry_in * 0.25_rk
        gs%hazards(slot)%z = 34.0_rk + rs * 8.0_rk
        gs%hazards(slot)%vx = (rv - 0.5_rk) * 0.15_rk
        gs%hazards(slot)%vy = 0.0_rk
        gs%hazards(slot)%speed = merge(7.5_rk, 6.2_rk, kind == hazard_shard) + rs * 1.4_rk
        if (kind == hazard_spine) gs%hazards(slot)%speed = 5.4_rk + rs * 0.8_rk
        gs%hazards(slot)%rot = rs * 2.0_rk * pi
        gs%hazards(slot)%rot_speed = (rv * 2.0_rk - 1.0_rk) * 1.6_rk
        gs%hazards(slot)%tilt = rs * pi
        select case (kind)
        case (hazard_buoy)
            gs%hazards(slot)%scale = 0.55_rk + rs * 0.20_rk
            gs%hazards(slot)%radius = 0.55_rk
        case (hazard_shard)
            gs%hazards(slot)%scale = 0.70_rk + rs * 0.35_rk
            gs%hazards(slot)%radius = 0.70_rk
        case (hazard_spine)
            gs%hazards(slot)%scale = 0.90_rk + rs * 0.30_rk
            gs%hazards(slot)%radius = 0.60_rk
            gs%hazards(slot)%rot_speed = 0.0_rk
        case (hazard_wreck)
            gs%hazards(slot)%scale = 1.00_rk + rs * 0.50_rk
            gs%hazards(slot)%radius = 1.05_rk
            gs%hazards(slot)%rot_speed = (rv * 2.0_rk - 1.0_rk) * 0.7_rk
            gs%hazards(slot)%speed = 4.8_rk + rs * 1.0_rk
        case (hazard_arc)
            gs%hazards(slot)%scale = 0.90_rk + rs * 0.40_rk
            gs%hazards(slot)%radius = 0.80_rk
            gs%hazards(slot)%rot_speed = 0.0_rk
            gs%hazards(slot)%speed = 7.0_rk + rs * 1.6_rk
            if (rv > 0.5_rk) gs%hazards(slot)%tilt = pi / 2.0_rk
        case (hazard_lattice)
            gs%hazards(slot)%scale = 0.85_rk + rs * 0.30_rk
            gs%hazards(slot)%radius = 0.75_rk
            gs%hazards(slot)%rot_speed = (rv * 2.0_rk - 1.0_rk) * 0.8_rk
            gs%hazards(slot)%speed = 8.5_rk + rs * 1.2_rk
        end select
    end subroutine spawn_hazard

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

        gs%demo_palette_timer = gs%demo_palette_timer + dt
        if (gs%demo_palette_timer >= 9.0_rk) then
            gs%demo_palette_timer = modulo(gs%demo_palette_timer, 9.0_rk)
            call set_sector(gs, 1 + mod(gs%sector, max_sector))
            gs%sector_wave = 1
            gs%kills_sector_wave = 0
            gs%sector_intro_timer = 2.8_rk
            gs%message = sector_name(gs%sector)
            gs%message_timer = 1.6_rk
        end if

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

        call update_demo_ship(gs, dt)
    end subroutine update_demo_autopilot

    subroutine update_demo_ship(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk), parameter :: envelope_x = 1.55_rk
        real(rk), parameter :: envelope_y = 0.95_rk
        real(rk) :: target_x
        real(rk) :: target_y
        real(rk) :: best_dist
        real(rk) :: dist
        real(rk) :: dx
        real(rk) :: dy
        integer :: i
        integer :: threat

        threat = 0
        best_dist = 8.0_rk
        do i = 1, max_hazards
            if (.not. gs%hazards(i)%active) cycle
            if (gs%hazards(i)%z < 0.6_rk .or. gs%hazards(i)%z > 9.0_rk) cycle
            dx = gs%hazards(i)%x - gs%ship_x
            dy = gs%hazards(i)%y - gs%ship_y
            if (abs(dx) > 2.0_rk .or. abs(dy) > 1.8_rk) cycle
            dist = gs%hazards(i)%z + abs(dx) * 0.4_rk + abs(dy) * 0.3_rk
            if (dist < best_dist) then
                best_dist = dist
                threat = i
            end if
        end do

        gs%demo_ship_phase = gs%demo_ship_phase + dt
        target_x = 0.85_rk * sin(gs%demo_ship_phase * 0.55_rk)
        target_y = 0.35_rk * sin(gs%demo_ship_phase * 0.40_rk + 1.2_rk)

        if (threat > 0) then
            dx = gs%hazards(threat)%x
            dy = gs%hazards(threat)%y
            if (dx >= 0.0_rk) then
                target_x = min(target_x, dx - 1.1_rk)
            else
                target_x = max(target_x, dx + 1.1_rk)
            end if
            if (abs(gs%hazards(threat)%y) > 0.4_rk) then
                if (dy >= 0.0_rk) then
                    target_y = min(target_y, dy - 0.7_rk)
                else
                    target_y = max(target_y, dy + 0.7_rk)
                end if
            end if
        end if

        target_x = clamp(target_x, -envelope_x, envelope_x)
        target_y = clamp(target_y, -envelope_y, envelope_y)

        gs%ship_vx = gs%ship_vx + (target_x - gs%ship_x) * 9.0_rk * dt
        gs%ship_vy = gs%ship_vy + (target_y - gs%ship_y) * 7.5_rk * dt
        gs%ship_vx = gs%ship_vx * max(0.0_rk, 1.0_rk - 4.5_rk * dt)
        gs%ship_vy = gs%ship_vy * max(0.0_rk, 1.0_rk - 4.5_rk * dt)
        gs%ship_vx = clamp(gs%ship_vx, -5.5_rk, 5.5_rk)
        gs%ship_vy = clamp(gs%ship_vy, -5.5_rk, 5.5_rk)
        gs%ship_x = clamp(gs%ship_x + gs%ship_vx * dt, -envelope_x, envelope_x)
        gs%ship_y = clamp(gs%ship_y + gs%ship_vy * dt, -envelope_y, envelope_y)
    end subroutine update_demo_ship

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
        do i = 1, max_rockets
            if (.not. gs%rockets(i)%active) cycle
            if (gs%rockets(i)%position%z < 1.5_rk) cycle
            if (gs%rockets(i)%position%z >= best_z) cycle
            if (.not. project_point(gs%rockets(i)%position, cam, width, height, screen, depth, scale_px)) cycle
            index = -i
            best_z = gs%rockets(i)%position%z
            tx = clamp((screen%x - 0.5_rk * real(width, rk)) / (0.42_rk * real(width, rk)), -0.88_rk, 0.88_rk)
            ty = clamp((0.5_rk * real(height, rk) - screen%y) / (0.38_rk * real(height, rk)), -0.68_rk, 0.68_rk)
        end do
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

    subroutine find_lock_target(gs, width, height, index, screen, radius_px, in_damage_range)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer, intent(out) :: index
        type(vec2), intent(out) :: screen
        real(rk), intent(out) :: radius_px
        logical, intent(out) :: in_damage_range
        type(camera3) :: cam
        type(vec2) :: candidate_screen
        type(vec3) :: pos
        real(rk) :: depth
        real(rk) :: scale_px
        real(rk) :: cx
        real(rk) :: cy
        real(rk) :: dist
        real(rk) :: candidate_radius
        real(rk) :: best_depth
        integer :: i

        index = 0
        screen = vec2(0.0_rk, 0.0_rk)
        radius_px = 0.0_rk
        in_damage_range = .false.
        best_depth = huge(1.0_rk)
        cx = 0.5_rk * real(width, rk) + gs%reticle_x * 0.42_rk * real(width, rk)
        cy = 0.5_rk * real(height, rk) - gs%reticle_y * 0.38_rk * real(height, rk)
        cam = scene_camera(gs)

        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) cycle
            if (.not. phantom_visible(gs%enemies(i), gs%time)) cycle
            pos = enemy_position(gs%enemies(i))
            if (.not. project_point(pos, cam, width, height, candidate_screen, depth, scale_px)) cycle
            if (gs%enemies(i)%is_boss) then
                candidate_radius = max(42.0_rk, gs%boss_models(gs%enemies(i)%boss_kind)%radius * &
                    boss_scale(gs%enemies(i)%boss_kind, pos%z) * scale_px * 0.88_rk)
            else if (gs%enemies(i)%pattern == pattern_lancer) then
                candidate_radius = max(22.0_rk, gs%lancer_model%radius * enemy_scale(pos%z) * scale_px * 0.85_rk)
            else
                candidate_radius = max(20.0_rk, gs%enemy_models(gs%enemies(i)%pattern)%radius * enemy_scale(pos%z) * scale_px * 0.85_rk)
            end if
            dist = sqrt((candidate_screen%x - cx) ** 2 + (candidate_screen%y - cy) ** 2)
            if (dist <= candidate_radius * 1.72_rk .and. depth < best_depth) then
                index = i
                screen = candidate_screen
                radius_px = candidate_radius
                in_damage_range = dist <= candidate_radius
                best_depth = depth
            end if
        end do
    end subroutine find_lock_target

    subroutine update_enemies(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer :: i
        type(vec3) :: pos
        logical :: breached

        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) cycle
            gs%enemies(i)%age = gs%enemies(i)%age + dt
            gs%enemies(i)%flash = max(0.0_rk, gs%enemies(i)%flash - dt * 4.0_rk)
            if (gs%enemies(i)%is_boss) then
                if (gs%boss_intro_timer > 0.0_rk) then
                    gs%enemies(i)%z = max(boss_hold_z(gs%enemies(i)%boss_kind), gs%enemies(i)%z - gs%enemies(i)%speed * dt)
                else
                    gs%enemies(i)%z = boss_hold_z(gs%enemies(i)%boss_kind)
                end if
                cycle
            end if

            gs%enemies(i)%z = gs%enemies(i)%z - gs%enemies(i)%speed * dt
            pos = enemy_position(gs%enemies(i))
            breached = pos%z < 1.25_rk
            if (breached) then
                gs%enemies(i)%active = .false.
                if (gs%state == state_play) call player_hit(gs, pos)
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
                gs%particles(i)%angle = gs%particles(i)%angle + gs%particles(i)%angular_velocity * dt
            end if
        end do
    end subroutine update_particles

    subroutine update_spawning(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk) :: interval
        real(rk) :: rz

        if (gs%state /= state_play) return
        if (gs%boss_fight .or. gs%boss_intro_timer > 0.0_rk .or. gs%boss_victory_timer > 0.0_rk) return

        gs%spawn_timer = gs%spawn_timer - dt
        if (gs%spawn_timer > 0.0_rk) return

        call random_number(rz)
        interval = max(0.42_rk, 1.20_rk - real(gs%wave, rk) * 0.075_rk)
        gs%spawn_timer = interval + rz * 0.45_rk
        call spawn_enemy(gs, 24.0_rk + rz * 13.0_rk)
    end subroutine update_spawning

    subroutine update_ambient_audio(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk) :: freq
        real(rk) :: volume

        if (gs%state /= state_play) return

        gs%ambient_timer = gs%ambient_timer - dt
        if (gs%ambient_timer > 0.0_rk) return

        gs%ambient_timer = 0.28_rk
        select case (gs%sector)
        case (1)
            freq = 110.0_rk
            volume = 0.018_rk
        case (2)
            freq = 82.0_rk
            volume = 0.014_rk + 0.008_rk * (0.5_rk + 0.5_rk * sin(gs%time * 2.0_rk * pi * 4.0_rk))
        case (3)
            freq = 92.0_rk
            volume = 0.020_rk
        case (4)
            freq = 74.0_rk
            volume = 0.017_rk
        case (5)
            freq = 58.0_rk
            volume = 0.012_rk
        case default
            freq = 68.0_rk
            volume = 0.024_rk
        end select
        if (gs%boss_fight) volume = volume + 0.008_rk
        call platform_audio_beep(real(freq), 0.34, real(volume))
    end subroutine update_ambient_audio

    subroutine update_proximity_audio(gs)
        type(game_state_t), intent(inout) :: gs
        type(vec3) :: pos
        real(rk) :: nearest_z
        real(rk) :: pitch
        real(rk) :: interval
        integer :: i

        if (gs%state /= state_play) return

        nearest_z = huge(1.0_rk)
        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active .or. gs%enemies(i)%is_boss) cycle
            pos = enemy_position(gs%enemies(i))
            if (pos%z > 1.25_rk .and. pos%z < nearest_z) nearest_z = pos%z
        end do

        if (nearest_z > 10.0_rk) then
            gs%danger_timer = 0.0_rk
            return
        end if

        if (gs%danger_timer > 0.0_rk) return

        pitch = 170.0_rk + (10.0_rk - nearest_z) * 58.0_rk
        interval = merge(0.18_rk, 0.38_rk, nearest_z < 5.0_rk)
        gs%danger_timer = interval
        call platform_audio_beep(real(pitch), 0.045, 0.075)
        if (nearest_z < 5.0_rk) call platform_audio_beep(real(pitch * 1.45_rk), 0.025, 0.050)
    end subroutine update_proximity_audio

    subroutine update_coil_chatter(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        real(rk) :: rx
        real(rk) :: ry
        real(rk) :: rz

        if (gs%state /= state_play .or. gs%boss_intro_timer > 0.0_rk) return

        gs%coil_chatter_timer = gs%coil_chatter_timer - dt
        if (gs%coil_chatter_timer > 0.0_rk) return

        call random_number(rx)
        call random_number(ry)
        call random_number(rz)
        gs%coil_chatter_timer = 1.3_rk + rx * 2.7_rk
        call platform_audio_beep(real(600.0_rk + ry * 300.0_rk), 0.040, real(0.030_rk + rz * 0.020_rk))
    end subroutine update_coil_chatter

    subroutine play_damage_audio()
        call platform_audio_beep(320.0, 0.045, 0.10)
        call platform_audio_beep(620.0, 0.030, 0.055)
    end subroutine play_damage_audio

    subroutine play_kill_audio()
        call platform_audio_beep(74.0, 0.18, 0.19)
        call platform_audio_beep(300.0, 0.08, 0.15)
        call platform_audio_noise(1350.0, 0.12, 0.045, 1.8)
    end subroutine play_kill_audio

    subroutine play_boss_kill_audio()
        call platform_audio_beep(40.0, 0.48, 0.22)
        call platform_audio_beep(90.0, 0.28, 0.20)
        call platform_audio_noise(1250.0, 0.28, 0.070, 1.4)
        call platform_audio_beep(220.0, 0.18, 0.14)
        call platform_audio_beep(330.0, 0.20, 0.13)
        call platform_audio_beep(440.0, 0.24, 0.12)
    end subroutine play_boss_kill_audio

    subroutine play_player_hit_audio()
        call platform_audio_beep(70.0, 0.18, 0.20)
        call platform_audio_noise(900.0, 0.11, 0.075, 2.2)
    end subroutine play_player_hit_audio

    subroutine play_game_over_audio()
        call platform_audio_beep(140.0, 0.18, 0.12)
        call platform_audio_beep(90.0, 0.25, 0.10)
        call platform_audio_beep(45.0, 0.36, 0.16)
    end subroutine play_game_over_audio

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
        gs%enemies(slot)%hp_max = gs%enemies(slot)%hp
        gs%enemies(slot)%is_boss = .false.
        gs%enemies(slot)%boss_kind = 0
        gs%enemies(slot)%variant = variant_base
        gs%enemies(slot)%cage_intact = .false.
        gs%enemies(slot)%speed = 3.6_rk + real(gs%wave, rk) * 0.22_rk + rz * 1.35_rk

        if (gs%enemies(slot)%pattern == 2 .and. gs%sector >= 3) then
            block
                real(rk) :: roll
                call random_number(roll)
                if (roll < 0.40_rk) then
                    gs%enemies(slot)%variant = variant_juggernaut
                    gs%enemies(slot)%cage_intact = .true.
                    gs%enemies(slot)%hp = 2
                    gs%enemies(slot)%hp_max = 2
                    gs%enemies(slot)%speed = gs%enemies(slot)%speed * 0.78_rk
                end if
            end block
        end if

        if (gs%enemies(slot)%pattern == 3 .and. gs%sector >= 5) then
            block
                real(rk) :: roll
                call random_number(roll)
                if (roll < 0.45_rk) then
                    gs%enemies(slot)%variant = variant_phantom
                end if
            end block
        end if
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
        if (enemy%is_boss) then
            select case (enemy%boss_kind)
            case (1)
                enemy_position%x = enemy_position%x + 1.45_rk * sin(enemy%age * 0.55_rk + enemy%phase)
                enemy_position%y = enemy_position%y + 0.28_rk * sin(enemy%age * 0.42_rk)
                enemy_position%z = enemy_position%z + 0.25_rk * cos(enemy%age * 0.38_rk)
            case (2)
                enemy_position%x = enemy_position%x + 0.95_rk * sin(enemy%age * 0.48_rk + enemy%phase)
                enemy_position%y = enemy_position%y + 0.55_rk * cos(enemy%age * 0.66_rk)
                enemy_position%z = enemy_position%z + 0.35_rk * sin(enemy%age * 0.36_rk)
            case default
                enemy_position%x = enemy_position%x + 0.62_rk * sin(enemy%age * 0.34_rk + enemy%phase)
                enemy_position%y = enemy_position%y + 0.18_rk * cos(enemy%age * 0.50_rk)
                enemy_position%z = enemy_position%z + 0.42_rk * sin(enemy%age * 0.28_rk)
            end select
            return
        end if

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
        case (pattern_lancer)
            enemy_position%x = enemy_position%x + enemy%vx * enemy%age * 0.85_rk &
                + 0.55_rk * sin(enemy%age * 1.3_rk + enemy%phase)
            enemy_position%y = enemy_position%y + 0.28_rk * sin(enemy%age * 1.0_rk + enemy%phase)
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
        type(vec2) :: screen
        real(rk) :: radius_px
        integer :: best
        integer :: rocket_index
        type(vec2) :: rocket_screen
        real(rk) :: rocket_dist
        logical :: in_damage_range
        real(rk) :: shot_pitch
        real(rk) :: detune

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
        gs%laser_target_locked = .false.
        if (gs%shot_chain_timer <= 0.0_rk) gs%shot_serial = 0
        gs%shot_serial = gs%shot_serial + 1
        detune = real(mod(gs%shot_serial, 7) - 3, rk) * 7.5_rk
        shot_pitch = merge(930.0_rk, 780.0_rk, gs%precision_aim) + detune
        gs%shot_chain_timer = 0.28_rk
        call platform_audio_beep(real(shot_pitch), 0.045, merge(0.13, 0.16, gs%precision_aim))
        call platform_audio_beep(real(shot_pitch * 1.46_rk), 0.024, merge(0.055, 0.075, gs%precision_aim))

        call find_rocket_target(gs, width, height, rocket_index, rocket_screen, rocket_dist)
        if (rocket_index > 0) then
            gs%laser_target_locked = .true.
            gs%laser_target_x = rocket_screen%x
            gs%laser_target_y = rocket_screen%y
            call intercept_rocket(gs, rocket_index)
            return
        end if

        call find_lock_target(gs, width, height, best, screen, radius_px, in_damage_range)
        if (best > 0) then
            gs%laser_target_locked = .true.
            gs%laser_target_x = screen%x
            gs%laser_target_y = screen%y
        end if

        if (best > 0 .and. in_damage_range) then
            call damage_enemy(gs, best)
        end if
    end subroutine fire_weapon

    subroutine find_rocket_target(gs, width, height, index, screen_out, best_dist)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer, intent(out) :: index
        type(vec2), intent(out) :: screen_out
        real(rk), intent(out) :: best_dist
        type(camera3) :: cam
        type(vec2) :: screen
        real(rk) :: depth
        real(rk) :: rx
        real(rk) :: ry
        real(rk) :: dx
        real(rk) :: dy
        real(rk) :: dist
        real(rk) :: radius
        integer :: i
        logical :: ok

        index = 0
        best_dist = huge(1.0_rk)
        screen_out = vec2(0.0_rk, 0.0_rk)

        cam = scene_camera(gs)
        rx = 0.5_rk * real(width, rk) + gs%reticle_x * 0.42_rk * real(width, rk)
        ry = 0.5_rk * real(height, rk) - gs%reticle_y * 0.38_rk * real(height, rk)
        radius = merge(46.0_rk, 62.0_rk, gs%precision_aim) * max(1.0_rk, real(width, rk) / 1280.0_rk)

        do i = 1, max_rockets
            if (.not. gs%rockets(i)%active) cycle
            if (gs%rockets(i)%position%z < 0.4_rk) cycle
            ok = project_point(gs%rockets(i)%position, cam, width, height, screen, depth)
            if (.not. ok) cycle
            dx = screen%x - rx
            dy = screen%y - ry
            dist = sqrt(dx * dx + dy * dy)
            if (dist < radius .and. dist < best_dist) then
                best_dist = dist
                index = i
                screen_out = screen
            end if
        end do
    end subroutine find_rocket_target

    subroutine intercept_rocket(gs, index)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: index
        type(vec3) :: pos

        if (.not. gs%rockets(index)%active) return
        pos = gs%rockets(index)%position
        gs%rockets(index)%active = .false.
        gs%score = gs%score + nint(120.0_rk * sector_score_mult(gs%sector) * streak_multiplier(gs%streak))
        call add_streak(gs, 1)
        call spawn_explosion(gs, pos, 16, 255, 220, 120)
        gs%message = "LANCE DOWN"
        gs%message_timer = 0.55_rk
        call platform_audio_beep(860.0, 0.05, 0.14)
        call platform_audio_beep(1320.0, 0.04, 0.10)
        if (gs%score > gs%high_score) then
            gs%high_score = gs%score
            if (.not. gs%demo_mode) call save_high_score(gs%high_score)
        end if
    end subroutine intercept_rocket

    subroutine damage_enemy(gs, index)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: index
        type(vec3) :: pos

        if (.not. gs%enemies(index)%active) return

        if (gs%enemies(index)%variant == variant_juggernaut .and. gs%enemies(index)%cage_intact) then
            gs%enemies(index)%cage_intact = .false.
            gs%enemies(index)%hp = gs%enemies(index)%hp - 1
            gs%enemies(index)%flash = 1.0_rk
            pos = enemy_position(gs%enemies(index))
            gs%score = gs%score + 40
            call spawn_explosion(gs, pos, 20, 120, 255, 120)
            call platform_audio_beep(1180.0, 0.04, 0.11)
            call platform_audio_noise(2200.0, 0.06, 0.05, 4.0)
            gs%message = "SHIELD BROKEN"
            gs%message_timer = 0.5_rk
            return
        end if

        gs%enemies(index)%hp = gs%enemies(index)%hp - 1
        gs%enemies(index)%flash = 1.0_rk
        pos = enemy_position(gs%enemies(index))

        if (gs%enemies(index)%hp <= 0) then
            gs%enemies(index)%active = .false.
            if (gs%enemies(index)%is_boss) then
                call defeat_boss(gs, index, pos)
                return
            end if
            gs%score = gs%score + nint(real(100 + gs%wave * 25 + max(0, nint((22.0_rk - pos%z) * 5.0_rk)), rk) &
                * sector_score_mult(gs%sector) &
                * merge(1.6_rk, 1.0_rk, gs%enemies(index)%variant /= variant_base) &
                * streak_multiplier(gs%streak))
            gs%kills = gs%kills + 1
            gs%kills_sector_wave = gs%kills_sector_wave + 1
            if (gs%enemies(index)%variant /= variant_base) then
                call add_streak(gs, 2)
            else
                call add_streak(gs, 1)
            end if
            if (gs%score > gs%high_score) then
                gs%high_score = gs%score
                if (.not. gs%demo_mode) call save_high_score(gs%high_score)
            end if
            call spawn_explosion(gs, pos, 34, 255, 180, 40)
            gs%screen_shake = max(gs%screen_shake, 0.22_rk)
            gs%message = "TARGET BROKEN"
            gs%message_timer = 0.55_rk
            call play_kill_audio()
            call maybe_drop_shard(gs, pos, 1)
            if (gs%kills_sector_wave >= sector_wave_quota(gs%sector_wave)) then
                call advance_wave(gs)
            end if
        else
            if (gs%enemies(index)%is_boss) then
                gs%score = gs%score + 35 * gs%sector
                call spawn_explosion(gs, pos, 18, 255, 255, 180)
                call play_damage_audio()
            else
                gs%score = gs%score + 25
                call spawn_explosion(gs, pos, 12, 255, 255, 180)
                call play_damage_audio()
            end if
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
        case (3); mult = 2.0_rk
        case (4); mult = 2.4_rk
        case (5); mult = 2.8_rk
        case default; mult = 3.2_rk
        end select
    end function sector_score_mult

    subroutine advance_wave(gs)
        type(game_state_t), intent(inout) :: gs
        logical :: perfect

        perfect = gs%streak > gs%streak_at_wave_start .and. gs%streak_at_wave_start >= 0
        if (perfect) then
            gs%score = gs%score + nint(600.0_rk * sector_score_mult(gs%sector))
            gs%perfect_banner_timer = 1.8_rk
            call platform_audio_beep(1050.0, 0.08, 0.16)
            call platform_audio_beep(1570.0, 0.08, 0.14)
            call platform_audio_beep(2100.0, 0.10, 0.12)
        end if
        gs%streak_at_wave_start = gs%streak
        gs%kills_sector_wave = 0

        if (gs%sector_wave >= waves_per_sector) then
            call start_boss_entrance(gs)
            return
        end if

        gs%wave = gs%wave + 1
        gs%sector_wave = gs%sector_wave + 1
        gs%message = "WAVE UP"
        gs%message_timer = 1.1_rk
        call platform_audio_beep(460.0, 0.08, 0.14)
        call platform_audio_beep(690.0, 0.09, 0.12)
    end subroutine advance_wave

    subroutine start_boss_entrance(gs)
        type(game_state_t), intent(inout) :: gs
        integer :: slot
        integer :: i
        real(rk) :: rz

        do i = 1, max_enemies
            gs%enemies(i)%active = .false.
        end do

        slot = 1
        call random_number(rz)
        gs%enemies(slot)%active = .true.
        gs%enemies(slot)%is_boss = .true.
        gs%enemies(slot)%boss_kind = gs%sector
        gs%enemies(slot)%hp_max = boss_hp(gs%sector)
        gs%enemies(slot)%hp = gs%enemies(slot)%hp_max
        gs%enemies(slot)%pattern = 0
        gs%enemies(slot)%age = 0.0_rk
        gs%enemies(slot)%phase = rz * 2.0_rk * pi
        gs%enemies(slot)%flash = 0.0_rk
        gs%enemies(slot)%x = 0.0_rk
        gs%enemies(slot)%y = 0.0_rk
        gs%enemies(slot)%z = 38.0_rk
        gs%enemies(slot)%vx = 0.0_rk
        gs%enemies(slot)%vy = 0.0_rk
        gs%enemies(slot)%speed = 13.5_rk
        gs%boss_fight = .true.
        gs%boss_intro_timer = 1.75_rk
        gs%boss_victory_timer = 0.0_rk
        gs%boss_attack_timer = 2.35_rk
        gs%boss_attack_flash = 0.0_rk
        gs%boss_cleared_sector = 0
        gs%sector_intro_timer = 0.0_rk
        gs%message = "WARNING - " // trim(boss_name(gs%sector))
        gs%message_timer = 2.0_rk
        gs%screen_shake = max(gs%screen_shake, 0.18_rk)
        call platform_audio_beep(110.0, 0.22, 0.18)
        call platform_audio_beep(165.0, 0.22, 0.15)
        call platform_audio_beep(220.0, 0.26, 0.12)
    end subroutine start_boss_entrance

    subroutine defeat_boss(gs, index, pos)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: index
        type(vec3), intent(in) :: pos
        integer :: ar
        integer :: ag
        integer :: ab
        integer :: pr
        integer :: pg
        integer :: pb
        integer :: burst
        real(rk) :: offset
        type(vec3) :: burst_pos

        call sector_palette_accent(gs%enemies(index)%boss_kind, ar, ag, ab)
        call sector_palette_primary(gs%enemies(index)%boss_kind, pr, pg, pb)
        gs%score = gs%score + nint(real(1200 + 500 * gs%enemies(index)%boss_kind, rk) &
            * sector_score_mult(gs%sector) * streak_multiplier(gs%streak))
        call add_streak(gs, 5)
        if (gs%score > gs%high_score) then
            gs%high_score = gs%score
            if (.not. gs%demo_mode) call save_high_score(gs%high_score)
        end if
        do burst = 1, 3
            offset = real(burst - 2, rk) * 0.46_rk
            burst_pos = add3(pos, vec3(offset, 0.28_rk * sin(real(burst, rk) * 1.7_rk), 0.18_rk * cos(real(burst, rk))))
            call spawn_explosion(gs, burst_pos, 36, 255, 130, 70)
        end do
        call spawn_explosion(gs, pos, 120, ar, ag, ab)
        call spawn_explosion(gs, pos, 72, pr, pg, pb)
        call maybe_drop_shard(gs, pos, 2)
        call spawn_shard(gs, pos, shard_amber)
        call spawn_shard(gs, pos, shard_amber)
        gs%screen_shake = max(gs%screen_shake, 1.05_rk)
        gs%message = trim(boss_name(gs%enemies(index)%boss_kind)) // " DOWN"
        gs%message_timer = 1.8_rk
        gs%boss_fight = .false.
        gs%boss_intro_timer = 0.0_rk
        gs%boss_attack_timer = 0.0_rk
        gs%boss_attack_flash = 0.0_rk
        gs%boss_victory_timer = 1.45_rk
        gs%boss_cleared_sector = gs%enemies(index)%boss_kind
        call play_boss_kill_audio()
    end subroutine defeat_boss

    subroutine complete_boss_victory(gs)
        type(game_state_t), intent(inout) :: gs

        if (gs%boss_cleared_sector >= max_sector) then
            gs%message = "GATE IS DARK"
            gs%message_timer = 99.0_rk
            call platform_audio_beep(180.0, 0.18, 0.18)
            call platform_audio_beep(270.0, 0.22, 0.16)
            call platform_audio_beep(360.0, 0.30, 0.14)
            if (.not. gs%demo_mode .and. gs%score > gs%high_score) then
                gs%high_score = gs%score
                call save_high_score(gs%high_score)
            end if
            if (gs%demo_mode) then
                gs%state = state_victory
            else
                call start_transmission(gs, tx_victory, state_victory)
            end if
            return
        end if

        call set_sector(gs, min(max_sector, gs%boss_cleared_sector + 1))
        gs%sector_wave = 1
        gs%wave = gs%wave + 1
        gs%kills_sector_wave = 0
        gs%shield = min(1.0_rk, gs%shield + 0.5_rk)
        gs%sector_intro_timer = 2.8_rk
        gs%message = sector_name(gs%sector)
        gs%message_timer = 2.4_rk
        gs%spawn_timer = 1.2_rk
        gs%boss_cleared_sector = 0
        call platform_audio_beep(320.0, 0.10, 0.16)
        call platform_audio_beep(480.0, 0.12, 0.14)
        call platform_audio_beep(640.0, 0.14, 0.12)
        if (.not. gs%demo_mode) then
            select case (gs%sector)
            case (2)
                call start_transmission(gs, tx_sector_two, state_play)
            case (3)
                call start_transmission(gs, tx_sector_three, state_play)
            end select
        end if
    end subroutine complete_boss_victory

    subroutine update_boss_attack(gs, dt)
        type(game_state_t), intent(inout) :: gs
        real(rk), intent(in) :: dt
        integer :: index
        type(vec3) :: pos

        if (.not. gs%boss_fight) return
        if (gs%boss_intro_timer > 0.0_rk .or. gs%boss_victory_timer > 0.0_rk) return

        index = active_boss_index(gs)
        if (index <= 0) return

        gs%boss_attack_timer = gs%boss_attack_timer - dt
        if (gs%boss_attack_timer > 0.0_rk) return

        gs%boss_attack_timer = max(1.45_rk, 2.25_rk - real(gs%sector, rk) * 0.22_rk)
        gs%boss_attack_flash = 0.26_rk
        pos = enemy_position(gs%enemies(index))
        call platform_audio_beep(95.0, 0.10, 0.16)
        call platform_audio_beep(190.0, 0.08, 0.12)
        call player_hit(gs, pos)
    end subroutine update_boss_attack

    pure function sector_name(sector) result(name)
        integer, intent(in) :: sector
        character(len=48) :: name
        select case (sector)
        case (1); name = "SECTOR I - OUTER PICKET"
        case (2); name = "SECTOR II - ASTEROID LANE"
        case (3); name = "SECTOR III - STRONGHOLD"
        case (4); name = "SECTOR IV - GRAVEYARD"
        case (5); name = "SECTOR V - VOID STORM"
        case default; name = "SECTOR VI - HIVE APPROACH"
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
        case (3); r = 255; g = 130; b = 60
        case (4); r = 220; g = 130; b = 60
        case (5); r = 180; g = 220; b = 255
        case default; r = 255; g = 60; b = 50
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
        case (3); r = 255; g = 240; b = 110
        case (4); r = 255; g = 180; b = 80
        case (5); r = 255; g = 255; b = 255
        case default; r = 255; g = 200; b = 120
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
        case (3); r = 110; g = 50;  b = 20
        case (4); r = 90;  g = 50;  b = 25
        case (5); r = 30;  g = 40;  b = 60
        case default; r = 90;  g = 20;  b = 20
        end select
    end subroutine sector_palette_dim

    subroutine set_sector(gs, new_sector)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: new_sector
        integer :: clamped_sector

        clamped_sector = max(1, min(max_sector, new_sector))
        if (clamped_sector == gs%sector) return

        gs%sector_palette_from = gs%sector
        gs%sector = clamped_sector
        gs%sector_palette_timer = 1.0_rk
    end subroutine set_sector

    subroutine blended_palette_primary(gs, r, g, b)
        type(game_state_t), intent(in) :: gs
        integer, intent(out) :: r
        integer, intent(out) :: g
        integer, intent(out) :: b

        call blended_palette(gs, sector_palette_primary, r, g, b)
    end subroutine blended_palette_primary

    subroutine blended_palette_accent(gs, r, g, b)
        type(game_state_t), intent(in) :: gs
        integer, intent(out) :: r
        integer, intent(out) :: g
        integer, intent(out) :: b

        call blended_palette(gs, sector_palette_accent, r, g, b)
    end subroutine blended_palette_accent

    subroutine blended_palette_dim(gs, r, g, b)
        type(game_state_t), intent(in) :: gs
        integer, intent(out) :: r
        integer, intent(out) :: g
        integer, intent(out) :: b

        call blended_palette(gs, sector_palette_dim, r, g, b)
    end subroutine blended_palette_dim

    subroutine blended_palette(gs, palette_proc, r, g, b)
        type(game_state_t), intent(in) :: gs
        interface
            pure subroutine palette_proc(sector, r, g, b)
                integer, intent(in) :: sector
                integer, intent(out) :: r
                integer, intent(out) :: g
                integer, intent(out) :: b
            end subroutine palette_proc
        end interface
        integer, intent(out) :: r
        integer, intent(out) :: g
        integer, intent(out) :: b
        integer :: nr
        integer :: ng
        integer :: nb
        integer :: or
        integer :: og
        integer :: ob
        real(rk) :: old_weight
        real(rk) :: new_weight

        call palette_proc(gs%sector, nr, ng, nb)
        if (gs%sector_palette_timer <= 0.0_rk) then
            r = nr
            g = ng
            b = nb
            return
        end if

        call palette_proc(gs%sector_palette_from, or, og, ob)
        old_weight = max(0.0_rk, min(1.0_rk, gs%sector_palette_timer))
        new_weight = 1.0_rk - old_weight
        r = max(0, min(255, nint(real(or, rk) * old_weight + real(nr, rk) * new_weight)))
        g = max(0, min(255, nint(real(og, rk) * old_weight + real(ng, rk) * new_weight)))
        b = max(0, min(255, nint(real(ob, rk) * old_weight + real(nb, rk) * new_weight)))
    end subroutine blended_palette

    pure function boss_name(sector) result(name)
        integer, intent(in) :: sector
        character(len=24) :: name
        select case (sector)
        case (1); name = "HARROWER"
        case (2); name = "SEER"
        case (3); name = "THE MAW"
        case (4); name = "BONEFORGE"
        case (5); name = "STORMVEIL"
        case default; name = "THE MAW CORE"
        end select
    end function boss_name

    pure integer function boss_hp(sector) result(hp)
        integer, intent(in) :: sector
        select case (sector)
        case (1); hp = 16
        case (2); hp = 20
        case (3); hp = 26
        case (4); hp = 30
        case (5); hp = 28
        case default; hp = 36
        end select
    end function boss_hp

    pure real(rk) function boss_hold_z(sector) result(z)
        integer, intent(in) :: sector
        select case (sector)
        case (1); z = 15.5_rk
        case (2); z = 14.8_rk
        case (3); z = 14.0_rk
        case (4); z = 13.5_rk
        case (5); z = 14.5_rk
        case default; z = 13.0_rk
        end select
    end function boss_hold_z

    subroutine player_hit(gs, pos)
        type(game_state_t), intent(inout) :: gs
        type(vec3), intent(in) :: pos

        call break_streak(gs)
        call spawn_explosion(gs, pos, 42, 0, 220, 255)
        gs%shield = gs%shield - 0.34_rk
        gs%screen_shake = max(gs%screen_shake, 0.55_rk)
        gs%message = "SHIELD IMPACT"
        gs%message_timer = 0.95_rk
        call play_player_hit_audio()

        if (gs%shield <= 0.0_rk) then
            gs%lives = gs%lives - 1
            gs%shield = 1.0_rk
            gs%message = "HULL BREACH"
            gs%message_timer = 1.3_rk
            call platform_audio_beep(55.0, 0.28, 0.20)
        end if

        if (gs%lives <= 0) then
            gs%message = "GAME OVER"
            gs%message_timer = 99.0_rk
            call play_game_over_audio()
            if (gs%demo_mode) then
                gs%state = state_game_over
            else
                call start_transmission(gs, tx_defeat, state_game_over)
            end if
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
        integer :: slot
        integer :: chunks
        real(rk) :: angle

        spawned = 0

        slot = reserve_particle(gs)
        if (slot > 0) then
            gs%particles(slot)%active = .true.
            gs%particles(slot)%kind = particle_ring
            gs%particles(slot)%position = origin
            gs%particles(slot)%velocity = vec3(0.0_rk, 0.0_rk, 0.0_rk)
            gs%particles(slot)%ttl = 0.42_rk
            gs%particles(slot)%max_ttl = gs%particles(slot)%ttl
            gs%particles(slot)%angle = 0.0_rk
            gs%particles(slot)%angular_velocity = 1.5_rk
            gs%particles(slot)%size = 0.55_rk + 0.014_rk * real(min(80, count), rk)
            gs%particles(slot)%r = r
            gs%particles(slot)%g = g
            gs%particles(slot)%b = b
        end if

        if (count >= 30) then
            slot = reserve_particle(gs)
            if (slot > 0) then
                gs%particles(slot)%active = .true.
                gs%particles(slot)%kind = particle_flash
                gs%particles(slot)%position = origin
                gs%particles(slot)%velocity = vec3(0.0_rk, 0.0_rk, 0.0_rk)
                gs%particles(slot)%ttl = 0.075_rk
                gs%particles(slot)%max_ttl = gs%particles(slot)%ttl
                gs%particles(slot)%angle = 0.0_rk
                gs%particles(slot)%angular_velocity = 0.0_rk
                gs%particles(slot)%size = 1.10_rk + 0.012_rk * real(min(90, count), rk)
                gs%particles(slot)%r = 255
                gs%particles(slot)%g = 255
                gs%particles(slot)%b = 255
            end if
        end if

        chunks = max(3, min(12, count / 5))
        do i = 1, chunks
            slot = reserve_particle(gs)
            if (slot <= 0) exit
            call random_number(rx)
            call random_number(ry)
            call random_number(rz)
            call random_number(angle)
            dir = vec3(rx * 2.0_rk - 1.0_rk, ry * 2.0_rk - 1.0_rk, rz * 2.0_rk - 1.0_rk)
            speed = 1.8_rk + rz * 3.8_rk
            gs%particles(slot)%active = .true.
            gs%particles(slot)%kind = particle_chunk
            gs%particles(slot)%position = origin
            gs%particles(slot)%velocity = scale3(dir, speed)
            gs%particles(slot)%ttl = 0.55_rk + rx * 0.45_rk
            gs%particles(slot)%max_ttl = gs%particles(slot)%ttl
            gs%particles(slot)%angle = angle * 2.0_rk * pi
            gs%particles(slot)%angular_velocity = (ry * 2.0_rk - 1.0_rk) * 8.0_rk
            gs%particles(slot)%size = 0.28_rk + 0.16_rk * rz
            gs%particles(slot)%r = max(80, r)
            gs%particles(slot)%g = max(70, g)
            gs%particles(slot)%b = max(50, b)
        end do

        do i = 1, max_particles
            if (spawned >= count) exit
            if (gs%particles(i)%active) cycle
            call random_number(rx)
            call random_number(ry)
            call random_number(rz)
            dir = vec3(rx * 2.0_rk - 1.0_rk, ry * 2.0_rk - 1.0_rk, rz * 2.0_rk - 1.0_rk)
            speed = 2.8_rk + rz * 5.0_rk
            gs%particles(i)%active = .true.
            gs%particles(i)%kind = particle_spark
            gs%particles(i)%position = origin
            gs%particles(i)%velocity = scale3(dir, speed)
            gs%particles(i)%ttl = 0.35_rk + rx * 0.55_rk
            gs%particles(i)%max_ttl = gs%particles(i)%ttl
            gs%particles(i)%angle = rz * 2.0_rk * pi
            gs%particles(i)%angular_velocity = 0.0_rk
            gs%particles(i)%size = 0.18_rk
            gs%particles(i)%r = r
            gs%particles(i)%g = g
            gs%particles(i)%b = b
            spawned = spawned + 1
        end do
    end subroutine spawn_explosion

    integer function reserve_particle(gs) result(slot)
        type(game_state_t), intent(in) :: gs
        integer :: i

        slot = 0
        do i = 1, max_particles
            if (.not. gs%particles(i)%active) then
                slot = i
                return
            end if
        end do
    end function reserve_particle

    subroutine render_game(gs, width, height)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height

        call platform_begin_frame()
        call render_stars(gs, width, height)
        call render_depth_grid(gs, width, height)
        if (gs%state /= state_transmission) then
            call render_environment(gs, width, height)
            call render_particles(gs, width, height)
            call render_enemies(gs, width, height)
            call render_rockets(gs, width, height)
            call render_shards(gs, width, height)
            call render_boss_attack(gs, width, height)
        end if

        select case (gs%state)
        case (state_title)
            call render_title(gs, width, height)
        case (state_play)
            call render_lock_on(gs, width, height)
            call render_rocket_warning(gs, width, height)
            call render_cockpit(gs, width, height)
            call render_sector_intro(gs, width, height)
            call render_hud(gs, width, height)
            call render_boss_hud(gs, width, height)
            if (gs%paused) call draw_centered_text("PAUSED", width / 2, height / 2 - 45, max(5, width / 190), 255, 255, 80, 230)
        case (state_game_over)
            call render_cockpit(gs, width, height)
            call render_hud(gs, width, height)
            call render_game_over(gs, width, height)
        case (state_victory)
            call render_cockpit(gs, width, height)
            call render_hud(gs, width, height)
            call render_victory(gs, width, height)
        case (state_transmission)
            call render_transmission(gs, width, height)
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
        integer :: pr
        integer :: pg
        integer :: pb
        integer :: sr
        integer :: sg
        integer :: sb
        logical :: ok1
        logical :: ok2

        cam = scene_camera(gs)
        call blended_palette_primary(gs, pr, pg, pb)
        do i = 1, max_stars
            p1 = vec3(gs%stars(i)%x, gs%stars(i)%y, gs%stars(i)%z)
            p2 = vec3(gs%stars(i)%x * 1.002_rk, gs%stars(i)%y * 1.002_rk, gs%stars(i)%z + 0.55_rk)
            ok1 = project_point(p1, cam, width, height, s1, d1)
            ok2 = project_point(p2, cam, width, height, s2, d2)
            if (ok1 .and. ok2) then
                shade = max(30, min(255, int(real(gs%stars(i)%shade, rk) * (1.0_rk - min(0.82_rk, d1 / 92.0_rk)))))
                sr = max(shade / 3, pr * shade / 255)
                sg = max(shade / 3, pg * shade / 255)
                sb = max(shade / 3, pb * shade / 255)
                call draw_line_glow(nint(s1%x), nint(s1%y), nint(s2%x), nint(s2%y), sr, sg, sb, 130, 1)
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
        integer :: dr
        integer :: dg
        integer :: db
        logical :: ok1
        logical :: ok2

        cam = scene_camera(gs)
        phase = modulo(gs%time * 5.2_rk, 4.0_rk)
        call blended_palette_dim(gs, dr, dg, db)

        do i = 0, 18
            z = 4.0_rk + real(i, rk) * 4.0_rk - phase
            if (z <= 2.0_rk) cycle
            p1 = vec3(-9.0_rk, -2.35_rk, z)
            p2 = vec3( 9.0_rk, -2.35_rk, z)
            ok1 = project_point(p1, cam, width, height, a, d1)
            ok2 = project_point(p2, cam, width, height, b, d2)
            if (ok1 .and. ok2) then
                call draw_line_glow(nint(a%x), nint(a%y), nint(b%x), nint(b%y), dr, dg, db, 58, 1)
            end if
        end do

        do i = -5, 5
            p1 = vec3(real(i, rk) * 1.8_rk, -2.35_rk, 3.0_rk)
            p2 = vec3(real(i, rk) * 1.8_rk, -2.35_rk, 76.0_rk)
            ok1 = project_point(p1, cam, width, height, a, d1)
            ok2 = project_point(p2, cam, width, height, b, d2)
            if (ok1 .and. ok2) then
                call draw_line_glow(nint(a%x), nint(a%y), nint(b%x), nint(b%y), &
                    max(0, dr * 3 / 4), max(0, dg * 3 / 4), max(0, db * 3 / 4), 52, 1)
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
        integer :: alpha
        real(rk) :: boost
        integer :: model_idx

        cam = scene_camera(gs)
        line_count = 0

        do i = 1, max_hazards
            if (.not. gs%hazards(i)%active) cycle
            xf%position = vec3(gs%hazards(i)%x, gs%hazards(i)%y, gs%hazards(i)%z)
            xf%rotation = vec3(gs%hazards(i)%tilt, gs%hazards(i)%rot, 0.25_rk * sin(gs%hazards(i)%rot))
            xf%scale = gs%hazards(i)%scale
            alpha = max(90, min(255, nint(280.0_rk - gs%hazards(i)%z * 4.2_rk)))
            boost = 1.0_rk
            if (gs%hazards(i)%z < 5.0_rk) boost = 1.25_rk
            model_idx = gs%hazards(i)%kind
            if (model_idx < 1 .or. model_idx > max_hazard_kinds) model_idx = hazard_shard
            call append_model_lines(gs%hazard_models(model_idx), xf, cam, width, height, &
                gs%lines, line_count, max_lines, alpha, boost)
        end do

        call draw_screen_lines(gs%lines, line_count)

        if (gs%brace_timer > 0.0_rk .and. gs%brace_side /= 0) then
            call render_brace_indicator(gs, width, height)
        end if
        if (gs%hazard_flash > 0.0_rk) then
            call render_impact_edges(gs, width, height)
        end if
    end subroutine render_environment

    subroutine render_brace_indicator(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer :: alpha
        integer :: cx
        integer :: cy
        integer :: size

        alpha = max(40, min(220, nint(240.0_rk * gs%brace_timer)))
        cy = height / 2
        size = max(16, width / 40)
        if (gs%brace_side > 0) then
            cx = width - max(30, width / 26)
            call draw_line_glow(cx, cy - size, cx + size, cy, 255, 120, 60, alpha, 2)
            call draw_line_glow(cx, cy + size, cx + size, cy, 255, 120, 60, alpha, 2)
            call draw_line_glow(cx - size / 2, cy, cx + size, cy, 255, 200, 80, alpha, 1)
        else
            cx = max(30, width / 26)
            call draw_line_glow(cx, cy - size, cx - size, cy, 255, 120, 60, alpha, 2)
            call draw_line_glow(cx, cy + size, cx - size, cy, 255, 120, 60, alpha, 2)
            call draw_line_glow(cx + size / 2, cy, cx - size, cy, 255, 200, 80, alpha, 1)
        end if
    end subroutine render_brace_indicator

    subroutine render_impact_edges(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer :: alpha
        integer :: inset

        alpha = max(0, min(255, nint(220.0_rk * gs%hazard_flash)))
        inset = max(4, width / 140)
        call draw_line_glow(inset, inset, width - inset, inset, 40, 220, 255, alpha, 2)
        call draw_line_glow(inset, height - inset, width - inset, height - inset, 40, 220, 255, alpha, 2)
        call draw_line_glow(inset, inset, inset, height - inset, 40, 220, 255, alpha, 2)
        call draw_line_glow(width - inset, inset, width - inset, height - inset, 40, 220, 255, alpha, 2)
    end subroutine render_impact_edges

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
        integer :: boss_kind

        cam = scene_camera(gs)
        line_count = 0
        do i = 1, max_enemies
            if (.not. gs%enemies(i)%active) cycle
            pos = enemy_position(gs%enemies(i))
            xf%position = pos
            if (gs%enemies(i)%is_boss) then
                boss_kind = gs%enemies(i)%boss_kind
                xf%rotation = vec3(0.10_rk * sin(gs%enemies(i)%age * 0.5_rk), &
                                   gs%enemies(i)%age * 0.18_rk + gs%enemies(i)%phase * 0.12_rk, &
                                   0.08_rk * sin(gs%enemies(i)%age * 0.7_rk))
                xf%scale = boss_scale(boss_kind, pos%z)
                boost = merge(2.6_rk, 1.15_rk, gs%enemies(i)%flash > 0.0_rk)
                alpha = max(170, min(255, nint(320.0_rk - pos%z * 3.0_rk)))
                call append_model_lines(gs%boss_models(boss_kind), xf, cam, width, height, gs%lines, line_count, max_lines, alpha, boost)
            else if (gs%enemies(i)%pattern == pattern_lancer) then
                xf%rotation = vec3(0.10_rk * sin(gs%enemies(i)%age * 0.9_rk), &
                                   gs%enemies(i)%age * 0.30_rk + gs%enemies(i)%phase, &
                                   0.06_rk * sin(gs%enemies(i)%age * 1.6_rk))
                xf%scale = enemy_scale(pos%z) * 1.15_rk
                boost = merge(2.25_rk, 1.0_rk, gs%enemies(i)%flash > 0.0_rk)
                if (gs%enemies(i)%fire_timer < 0.6_rk .and. gs%enemies(i)%fire_timer > 0.0_rk) &
                    boost = boost + 0.6_rk * (0.6_rk - gs%enemies(i)%fire_timer)
                alpha = max(110, min(255, nint(290.0_rk - pos%z * 3.2_rk)))
                call append_model_lines(gs%lancer_model, xf, cam, width, height, gs%lines, line_count, max_lines, alpha, boost)
            else
                xf%rotation = vec3(0.22_rk * sin(gs%enemies(i)%age), &
                                   gs%enemies(i)%age * 0.55_rk + gs%enemies(i)%phase, &
                                   0.26_rk * sin(gs%enemies(i)%age * 2.0_rk))
                xf%scale = enemy_scale(pos%z)
                boost = merge(2.25_rk, 1.0_rk, gs%enemies(i)%flash > 0.0_rk)
                alpha = max(105, min(255, nint(290.0_rk - pos%z * 3.2_rk)))
                if (gs%enemies(i)%variant == variant_phantom .and. .not. phantom_visible(gs%enemies(i), gs%time)) then
                    alpha = max(30, alpha / 5)
                    boost = boost * 0.6_rk
                end if
                call append_model_lines(gs%enemy_models(gs%enemies(i)%pattern), xf, cam, width, height, gs%lines, line_count, max_lines, alpha, boost)
                if (gs%enemies(i)%variant == variant_juggernaut .and. gs%enemies(i)%cage_intact) then
                    block
                        type(transform3) :: cage_xf
                        real(rk) :: cage_boost
                        cage_xf%position = pos
                        cage_xf%rotation = vec3(0.08_rk * sin(gs%enemies(i)%age * 0.6_rk), &
                                                gs%enemies(i)%age * 0.28_rk, &
                                                0.05_rk * sin(gs%enemies(i)%age * 0.9_rk))
                        cage_xf%scale = enemy_scale(pos%z) * 1.25_rk
                        cage_boost = 1.0_rk + 0.4_rk * (0.5_rk + 0.5_rk * sin(gs%time * 3.0_rk + gs%enemies(i)%phase))
                        call append_model_lines(gs%cage_model, cage_xf, cam, width, height, &
                            gs%lines, line_count, max_lines, max(120, alpha - 30), cage_boost)
                    end block
                end if
            end if
        end do

        call draw_screen_lines(gs%lines, line_count)
    end subroutine render_enemies

    subroutine render_rockets(gs, width, height)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(transform3) :: xf
        type(camera3) :: cam
        type(vec2) :: s1
        type(vec2) :: s2
        type(vec3) :: tail
        real(rk) :: d1
        real(rk) :: d2
        real(rk) :: yaw
        real(rk) :: pitch
        real(rk) :: boost
        integer :: line_count
        integer :: alpha
        integer :: i
        logical :: ok1
        logical :: ok2

        cam = scene_camera(gs)
        line_count = 0
        do i = 1, max_rockets
            if (.not. gs%rockets(i)%active) cycle
            yaw = atan2(gs%rockets(i)%velocity%x, -gs%rockets(i)%velocity%z)
            pitch = atan2(gs%rockets(i)%velocity%y, sqrt(gs%rockets(i)%velocity%x**2 + gs%rockets(i)%velocity%z**2))
            xf%position = gs%rockets(i)%position
            xf%rotation = vec3(-pitch, yaw, gs%rockets(i)%trail_phase * 6.0_rk)
            xf%scale = 1.0_rk
            boost = 1.2_rk + 0.35_rk * (0.5_rk + 0.5_rk * sin(gs%rockets(i)%trail_phase * 22.0_rk))
            alpha = max(150, min(255, nint(300.0_rk - gs%rockets(i)%position%z * 4.0_rk)))
            call append_model_lines(gs%rocket_model, xf, cam, width, height, &
                gs%lines, line_count, max_lines, alpha, boost)

            tail = add3(gs%rockets(i)%position, scale3(gs%rockets(i)%velocity, -0.12_rk))
            ok1 = project_point(gs%rockets(i)%position, cam, width, height, s1, d1)
            ok2 = project_point(tail, cam, width, height, s2, d2)
            if (ok1 .and. ok2) then
                call draw_line_glow(nint(s1%x), nint(s1%y), nint(s2%x), nint(s2%y), &
                    255, 220, 140, alpha, 2)
            end if
        end do
        call draw_screen_lines(gs%lines, line_count)
    end subroutine render_rockets

    subroutine render_rocket_warning(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer :: alpha
        integer :: inset
        integer :: cx
        integer :: cy
        integer :: size

        if (abs(gs%rocket_warning_dir) < 0.01_rk) return
        alpha = 180
        inset = max(6, width / 90)
        cy = height / 3
        size = max(18, width / 36)
        if (gs%rocket_warning_dir > 0.0_rk) then
            cx = width - inset - size
            call draw_line_glow(cx, cy - size, cx + size, cy, 255, 80, 60, alpha, 2)
            call draw_line_glow(cx, cy + size, cx + size, cy, 255, 80, 60, alpha, 2)
            call draw_line_glow(cx - size / 3, cy, cx + size, cy, 255, 200, 80, alpha, 1)
        else
            cx = inset + size
            call draw_line_glow(cx, cy - size, cx - size, cy, 255, 80, 60, alpha, 2)
            call draw_line_glow(cx, cy + size, cx - size, cy, 255, 80, 60, alpha, 2)
            call draw_line_glow(cx + size / 3, cy, cx - size, cy, 255, 200, 80, alpha, 1)
        end if
    end subroutine render_rocket_warning

    subroutine render_boss_attack(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(camera3) :: cam
        type(vec2) :: screen
        type(vec3) :: pos
        real(rk) :: depth
        real(rk) :: scale_px
        integer :: index
        integer :: alpha

        if (gs%boss_attack_flash <= 0.0_rk) return
        index = active_boss_index(gs)
        if (index <= 0) return

        cam = scene_camera(gs)
        pos = enemy_position(gs%enemies(index))
        if (.not. project_point(pos, cam, width, height, screen, depth, scale_px)) return
        alpha = max(0, min(255, nint(255.0_rk * min(1.0_rk, gs%boss_attack_flash / 0.26_rk))))
        call draw_line_glow(nint(screen%x), nint(screen%y), width / 2, height - max(42, height / 12), 255, 70, 40, alpha, 2)
        call draw_line_glow(nint(screen%x), nint(screen%y), width / 2, height - max(42, height / 12) - 20, 255, 220, 80, alpha / 2, 1)
    end subroutine render_boss_attack

    subroutine render_lock_on(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        type(vec2) :: screen
        real(rk) :: radius_px
        integer :: index
        integer :: bracket
        integer :: gap
        integer :: r
        integer :: g
        integer :: b
        integer :: cx
        integer :: cy
        logical :: in_damage_range

        call find_lock_target(gs, width, height, index, screen, radius_px, in_damage_range)
        if (index <= 0) return

        if (in_damage_range) then
            r = 255; g = 70; b = 55
        else
            r = 255; g = 220; b = 70
        end if
        cx = nint(screen%x)
        cy = nint(screen%y)
        gap = max(14, nint(radius_px * 0.92_rk))
        bracket = max(8, min(24, gap / 2))

        call draw_line_glow(cx - gap, cy - gap, cx - gap + bracket, cy - gap, r, g, b, 210, 1)
        call draw_line_glow(cx - gap, cy - gap, cx - gap, cy - gap + bracket, r, g, b, 210, 1)
        call draw_line_glow(cx + gap, cy - gap, cx + gap - bracket, cy - gap, r, g, b, 210, 1)
        call draw_line_glow(cx + gap, cy - gap, cx + gap, cy - gap + bracket, r, g, b, 210, 1)
        call draw_line_glow(cx - gap, cy + gap, cx - gap + bracket, cy + gap, r, g, b, 210, 1)
        call draw_line_glow(cx - gap, cy + gap, cx - gap, cy + gap - bracket, r, g, b, 210, 1)
        call draw_line_glow(cx + gap, cy + gap, cx + gap - bracket, cy + gap, r, g, b, 210, 1)
        call draw_line_glow(cx + gap, cy + gap, cx + gap, cy + gap - bracket, r, g, b, 210, 1)
    end subroutine render_lock_on

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
        real(rk) :: scale_px
        real(rk) :: t
        real(rk) :: age
        real(rk) :: theta1
        real(rk) :: theta2
        real(rk) :: radius
        real(rk) :: seg_len
        integer :: i
        integer :: j
        integer :: alpha
        integer :: x1
        integer :: y1
        integer :: x2
        integer :: y2

        cam = scene_camera(gs)
        do i = 1, max_particles
            if (.not. gs%particles(i)%active) cycle
            t = max(0.0_rk, min(1.0_rk, gs%particles(i)%ttl / gs%particles(i)%max_ttl))
            age = 1.0_rk - t
            alpha = max(20, min(255, nint(255.0_rk * t)))
            if (.not. project_point(gs%particles(i)%position, cam, width, height, a, d1, scale_px)) cycle

            select case (gs%particles(i)%kind)
            case (particle_ring)
                radius = max(5.0_rk, gs%particles(i)%size * scale_px * (0.25_rk + 1.85_rk * age))
                do j = 0, 19
                    theta1 = gs%particles(i)%angle + real(j, rk) * 2.0_rk * pi / 20.0_rk
                    theta2 = theta1 + 0.19_rk
                    x1 = nint(a%x + cos(theta1) * radius)
                    y1 = nint(a%y + sin(theta1) * radius)
                    x2 = nint(a%x + cos(theta2) * radius)
                    y2 = nint(a%y + sin(theta2) * radius)
                    call draw_line_glow(x1, y1, x2, y2, gs%particles(i)%r, gs%particles(i)%g, gs%particles(i)%b, alpha, 1)
                end do
            case (particle_chunk)
                seg_len = max(5.0_rk, gs%particles(i)%size * scale_px)
                theta1 = gs%particles(i)%angle
                x1 = nint(a%x - cos(theta1) * seg_len)
                y1 = nint(a%y - sin(theta1) * seg_len)
                x2 = nint(a%x + cos(theta1) * seg_len)
                y2 = nint(a%y + sin(theta1) * seg_len)
                call draw_line_glow(x1, y1, x2, y2, gs%particles(i)%r, gs%particles(i)%g, gs%particles(i)%b, alpha, 1)
                theta2 = theta1 + 0.5_rk * pi
                call draw_line_glow(nint(a%x - cos(theta2) * seg_len * 0.55_rk), &
                    nint(a%y - sin(theta2) * seg_len * 0.55_rk), &
                    nint(a%x + cos(theta2) * seg_len * 0.55_rk), &
                    nint(a%y + sin(theta2) * seg_len * 0.55_rk), &
                    gs%particles(i)%r, gs%particles(i)%g, gs%particles(i)%b, alpha / 2, 1)
            case (particle_flash)
                radius = max(18.0_rk, gs%particles(i)%size * scale_px * (0.65_rk + age))
                do j = 0, 9
                    theta1 = real(j, rk) * 2.0_rk * pi / 10.0_rk
                    call draw_line_glow(nint(a%x), nint(a%y), &
                        nint(a%x + cos(theta1) * radius), nint(a%y + sin(theta1) * radius), &
                        255, 255, 255, max(40, alpha), 1)
                end do
            case default
                tail = add3(gs%particles(i)%position, scale3(gs%particles(i)%velocity, -0.055_rk))
                if (.not. project_point(tail, cam, width, height, b, d2)) cycle
                call draw_line_glow(nint(a%x), nint(a%y), nint(b%x), nint(b%y), &
                    gs%particles(i)%r, gs%particles(i)%g, gs%particles(i)%b, alpha, 1)
            end select
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
        integer :: pr
        integer :: pg
        integer :: pb
        integer :: ar
        integer :: ag
        integer :: ab
        integer :: dr
        integer :: dg
        integer :: db
        integer :: tx
        integer :: ty
        integer :: tick
        integer :: x
        real(rk) :: flash

        call blended_palette_primary(gs, pr, pg, pb)
        call blended_palette_accent(gs, ar, ag, ab)
        call blended_palette_dim(gs, dr, dg, db)
        cx = nint(0.5_rk * real(width, rk) + gs%reticle_x * 0.42_rk * real(width, rk))
        cy = nint(0.5_rk * real(height, rk) - gs%reticle_y * 0.38_rk * real(height, rk))
        flash = real(gs%shot_flash / 0.11_rk)
        call draw_reticle(cx, cy, max(16, width / 45), real(flash))

        bx = width / 2
        by = height - max(42, height / 12)
        span = max(160, width / 5)
        call draw_line_glow(width / 2 - span, height - 24, width / 2 - span / 3, by, pr, pg, pb, 190, 2)
        call draw_line_glow(width / 2 + span, height - 24, width / 2 + span / 3, by, pr, pg, pb, 190, 2)
        call draw_line_glow(width / 2 - span / 3, by, width / 2 + span / 3, by, ar, ag, ab, 160, 1)
        call draw_line_glow(width / 2 - span, height - 24, width / 2 + span, height - 24, dr, dg, db, 120, 1)
        call draw_line_glow(18, height / 2 + height / 10, 18, height - 64, pr, pg, pb, 70, 1)
        call draw_line_glow(width - 18, height / 2 + height / 10, width - 18, height - 64, pr, pg, pb, 70, 1)
        do tick = -5, 5
            x = width / 2 + tick * span / 12
            call draw_line_glow(x, by + 4, x, by + 10 + 2 * abs(tick), dr, dg, db, 95, 1)
        end do

        if (gs%shot_flash > 0.0_rk) then
            tx = cx
            ty = cy
            if (gs%laser_target_locked) then
                tx = nint(gs%laser_target_x)
                ty = nint(gs%laser_target_y)
            end if
            call draw_line_glow(width / 2 - span / 4, by, tx, ty, ar, ag, ab, nint(230.0_rk * flash), 2)
            call draw_line_glow(width / 2 + span / 4, by, tx, ty, ar, ag, ab, nint(230.0_rk * flash), 2)
            call draw_line_glow(bx, by - 10, tx, ty, pr, pg, pb, nint(150.0_rk * flash), 1)
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
        character(len=32) :: sector_text
        character(len=32) :: streak_text
        integer :: unit
        integer :: pr
        integer :: pg
        integer :: pb
        integer :: ar
        integer :: ag
        integer :: ab
        integer :: streak_y
        integer :: streak_alpha
        integer :: sr
        integer :: sg
        integer :: sb

        unit = max(3, width / 285)
        call blended_palette_primary(gs, pr, pg, pb)
        call blended_palette_accent(gs, ar, ag, ab)
        write(score_text, '("SCORE ", I7.7)') gs%score
        write(wave_text, '("WAVE ", I2)') gs%wave
        write(lives_text, '("HULL ", I1)') max(0, gs%lives)
        write(high_text, '("HIGH ", I7.7)') gs%high_score
        write(sector_text, '("S", I1, " W", I1, "/", I1)') gs%sector, gs%sector_wave, waves_per_sector

        call draw_text(trim(score_text), 24, 22, unit, pr, pg, pb, 225)
        call draw_text(trim(high_text), 24, 58 + 7 * unit + 22 + 7 * unit, unit, 255, 210, 90, 180)
        call draw_text(trim(wave_text), width - 26 - 12 * 6 * unit, 22, unit, 255, 210, 60, 220)
        call draw_text(trim(sector_text), width - 26 - 9 * 6 * unit, 58 + 7 * unit, unit, ar, ag, ab, 210)
        call draw_text(trim(lives_text), 24, 58 + 7 * unit, unit, 255, 120, 80, 220)
        call draw_text("SHIELD", width - 26 - 17 * 6 * unit, 58 + 7 * unit, unit, pr, pg, pb, 210)
        call draw_meter(width - 26 - 80 * unit / 2, 62 + 15 * unit, 36 * unit, max(8, 3 * unit), real(gs%shield), pr, pg, pb)

        streak_y = 22 + 7 * unit + 6
        if (gs%streak_break_flash > 0.0_rk) then
            sr = 255
            sg = max(40, nint(120.0_rk * (1.0_rk - gs%streak_break_flash)))
            sb = max(40, nint(120.0_rk * (1.0_rk - gs%streak_break_flash)))
            streak_alpha = max(140, min(255, nint(180.0_rk + 75.0_rk * gs%streak_break_flash)))
            call draw_text("STREAK BROKEN", 24, streak_y, unit, sr, sg, sb, streak_alpha)
        else if (gs%streak > 0) then
            write(streak_text, '("STREAK x", I0)') gs%streak
            sr = ar
            sg = ag
            sb = ab
            streak_alpha = 200 + min(55, nint(55.0_rk * gs%streak_flash))
            call draw_text(trim(streak_text), 24, streak_y, unit, sr, sg, sb, streak_alpha)
        end if

        if (gs%perfect_banner_timer > 0.0_rk) then
            call draw_centered_text("PERFECT WAVE", width / 2, height / 4 + 10, max(5, width / 170), &
                255, 220, 100, max(60, min(255, nint(255.0_rk * min(1.0_rk, gs%perfect_banner_timer)))))
        end if

        if (gs%message_timer > 0.0_rk) then
            call draw_centered_text(trim(gs%message), width / 2, height / 6, max(4, width / 230), 255, 255, 120, &
                max(40, min(255, nint(230.0_rk * min(1.0_rk, gs%message_timer)))))
        end if
    end subroutine render_hud

    subroutine render_boss_hud(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        integer :: index
        integer :: unit
        integer :: bar_w
        integer :: bar_h
        integer :: pr
        integer :: pg
        integer :: pb
        integer :: ar
        integer :: ag
        integer :: ab
        real :: health

        index = active_boss_index(gs)
        if (index <= 0) return

        call sector_palette_primary(gs%enemies(index)%boss_kind, pr, pg, pb)
        call sector_palette_accent(gs%enemies(index)%boss_kind, ar, ag, ab)
        unit = max(3, width / 310)
        bar_w = max(220, width / 3)
        bar_h = max(8, 3 * unit)
        health = real(max(0, gs%enemies(index)%hp)) / real(max(1, gs%enemies(index)%hp_max))

        call draw_centered_text(trim(boss_name(gs%enemies(index)%boss_kind)), width / 2, 22, unit, ar, ag, ab, 230)
        call draw_meter(width / 2 - bar_w / 2, 34 + 8 * unit, bar_w, bar_h, health, pr, pg, pb)
        if (gs%boss_intro_timer > 0.0_rk) then
            call draw_centered_text("BOSS INBOUND", width / 2, height / 4, max(5, width / 190), 255, 80, 60, 220)
        end if
    end subroutine render_boss_hud

    subroutine render_sector_intro(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        real(rk), parameter :: intro_duration = 2.8_rk
        integer :: unit
        integer :: alpha
        integer :: pr
        integer :: pg
        integer :: pb
        integer :: ar
        integer :: ag
        integer :: ab
        integer :: y
        real(rk) :: phase
        real(rk) :: fade

        if (gs%sector_intro_timer <= 0.0_rk) return

        call blended_palette_primary(gs, pr, pg, pb)
        call blended_palette_accent(gs, ar, ag, ab)
        phase = max(0.0_rk, min(1.0_rk, (intro_duration - gs%sector_intro_timer) / intro_duration))
        fade = sin(pi * phase)
        alpha = max(0, min(255, nint(245.0_rk * fade)))
        if (alpha <= 8) return

        unit = max(5, width / 185)
        y = height / 3
        call draw_centered_text(trim(sector_name(gs%sector)), width / 2, y, unit, pr, pg, pb, alpha)
        call draw_line_glow(width / 2 - width / 5, y + 10 * unit, width / 2 + width / 5, y + 10 * unit, ar, ag, ab, alpha / 2, 1)
    end subroutine render_sector_intro

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
        call draw_centered_text("PHOSPHOR WING / SECTOR CAMPAIGN", width / 2, height / 2 + height / 8 + 9 * small_unit, &
            small_unit, 0, 220, 255, 190)
        if (gs%high_score > 0) then
            block
                character(len=32) :: hs_text
                write(hs_text, '("HIGH ", I7.7)') gs%high_score
                call draw_centered_text(trim(hs_text), width / 2, height / 2 + height / 8 + 19 * small_unit, &
                    small_unit, 255, 210, 90, 200)
            end block
        end if
        call draw_centered_text("PRESS ENTER OR SPACE", width / 2, height - height / 5, max(4, width / 230), 255, 235, 80, pulse)
        call draw_centered_text("MOUSE OR WASD AIM   CLICK OR SPACE FIRE   F12 CAPTURE", width / 2, height - height / 8, small_unit, 0, 190, 255, 185)
    end subroutine render_title

    subroutine render_transmission(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        character(len=48) :: line
        integer :: panel_w
        integer :: panel_h
        integer :: left
        integer :: top
        integer :: unit
        integer :: line_count
        integer :: i
        integer :: pulse
        integer :: y

        panel_w = min(width - 80, max(width * 4 / 5, 900))
        panel_h = min(height - 100, max(height / 2, 360))
        left = width / 2 - panel_w / 2
        top = height / 2 - panel_h / 2
        unit = max(2, min(max(3, width / 340), max(2, (panel_w - 60) / (48 * 6))))
        line_count = transmission_line_count(gs%transmission_id)

        call draw_box(left, top, left + panel_w, top + panel_h, 0, 210, 255, 130)
        call draw_line_glow(left + 24, top + 44, left + panel_w - 24, top + 44, 0, 210, 255, 90, 1)
        do i = 0, 4
            y = top + 78 + i * max(22, 9 * unit)
            call draw_line_glow(left + 28, y, left + panel_w - 28, y, 0, 80, 120, 38, 1)
        end do

        call draw_centered_text("PHOSPHOR COMMAND", width / 2, top + 16, unit, 0, 230, 255, 220)
        do i = 1, min(gs%transmission_visible_lines, line_count)
            line = transmission_line(gs%transmission_id, i)
            call draw_text(trim(line), left + 34, top + 76 + (i - 1) * 10 * unit, unit, 255, 255, 210, 225)
        end do

        if (gs%transmission_visible_lines >= line_count .and. line_count > 0) then
            pulse = nint(170.0_rk + 55.0_rk * sin(gs%time * 4.4_rk))
            call draw_centered_text("PRESS SPACE / FIRE TO CONTINUE", width / 2, top + panel_h - 44, &
                max(2, unit - 1), 255, 220, 80, pulse)
        end if
    end subroutine render_transmission

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

    subroutine render_victory(gs, width, height)
        type(game_state_t), intent(in) :: gs
        integer, intent(in) :: width
        integer, intent(in) :: height
        character(len=36) :: score_text
        character(len=36) :: high_text
        integer :: big
        integer :: small
        integer :: y0
        integer :: pr
        integer :: pg
        integer :: pb
        integer :: ar
        integer :: ag
        integer :: ab

        call blended_palette_primary(gs, pr, pg, pb)
        call blended_palette_accent(gs, ar, ag, ab)
        big = max(6, width / 145)
        small = max(2, min(max(3, width / 320), max(2, (width - 80) / (36 * 6))))
        y0 = height / 2 - height / 5
        write(score_text, '("FINAL SCORE ", I7.7)') gs%score
        write(high_text, '("HIGH SCORE  ", I7.7)') gs%high_score

        call draw_box(width / 2 - width / 4, height / 2 - height / 4, width / 2 + width / 4, height / 2 + height / 4, pr, pg, pb, 130)
        call draw_centered_text("VICTORY", width / 2, y0 - big * 4, big, ar, ag, ab, 245)
        call draw_centered_text(">> GATE IS DARK. COIL IS SILENT", width / 2, y0 + small * 4, small, pr, pg, pb, 225)
        call draw_centered_text(">> SOL CONFIRMS EVACUATION COMPLETE", width / 2, y0 + small * 10, small, pr, pg, pb, 225)
        call draw_centered_text(">> PHOSPHOR-LEAD, COME HOME", width / 2, y0 + small * 16, small, pr, pg, pb, 225)
        call draw_centered_text(">> WE LOG THIS ONE IN LEGEND", width / 2, y0 + small * 22, small, pr, pg, pb, 225)
        call draw_centered_text(trim(score_text), width / 2, y0 + small * 31, small, 255, 220, 90, 220)
        call draw_centered_text(trim(high_text), width / 2, y0 + small * 37, small, 0, 230, 255, 210)
        call draw_centered_text("PRESS R OR ENTER", width / 2, y0 + small * 44, small, ar, ag, ab, 220)
    end subroutine render_victory

    type(camera3) function scene_camera(gs)
        type(game_state_t), intent(in) :: gs
        real(rk) :: shake
        real(rk) :: iframe_blink

        shake = gs%screen_shake
        iframe_blink = 0.0_rk
        if (gs%ship_iframe > 0.0_rk) iframe_blink = 0.015_rk * sin(gs%time * 62.0_rk)
        scene_camera%position = vec3(gs%ship_x + 0.045_rk * shake * sin(gs%time * 73.0_rk) + iframe_blink, &
                                     gs%ship_y + 0.032_rk * shake * cos(gs%time * 59.0_rk), -0.85_rk)
        scene_camera%yaw = 0.018_rk * shake * sin(gs%time * 47.0_rk) + gs%ship_vx * 0.012_rk
        scene_camera%pitch = -0.035_rk + 0.012_rk * shake * cos(gs%time * 53.0_rk) - gs%ship_vy * 0.010_rk
        scene_camera%roll = 0.010_rk * shake * sin(gs%time * 61.0_rk) - gs%ship_vx * 0.018_rk
        scene_camera%focal_length = 0.96_rk
        scene_camera%near_z = 0.16_rk
    end function scene_camera

    pure real(rk) function enemy_scale(z)
        real(rk), intent(in) :: z
        enemy_scale = 0.64_rk + max(0.0_rk, min(0.24_rk, (24.0_rk - z) * 0.012_rk))
    end function enemy_scale

    pure real(rk) function boss_scale(sector, z) result(scale)
        integer, intent(in) :: sector
        real(rk), intent(in) :: z
        select case (sector)
        case (1); scale = 1.02_rk
        case (2); scale = 1.12_rk
        case default; scale = 1.24_rk
        end select
        scale = scale + max(0.0_rk, min(0.16_rk, (22.0_rk - z) * 0.007_rk))
    end function boss_scale

    pure real(rk) function streak_multiplier(streak) result(mult)
        integer, intent(in) :: streak
        mult = 1.0_rk + 0.10_rk * real(streak, rk)
        if (mult > 4.0_rk) mult = 4.0_rk
    end function streak_multiplier

    subroutine add_streak(gs, increment)
        type(game_state_t), intent(inout) :: gs
        integer, intent(in) :: increment
        integer :: milestone_bit
        integer, parameter :: milestones(4) = [10, 25, 50, 100]
        integer :: i

        gs%streak = gs%streak + increment
        if (gs%streak > gs%max_streak) gs%max_streak = gs%streak
        gs%streak_flash = 1.0_rk

        do i = 1, size(milestones)
            if (gs%streak >= milestones(i)) then
                milestone_bit = ishft(1, i - 1)
                if (iand(gs%streak_milestones, milestone_bit) == 0) then
                    gs%streak_milestones = ior(gs%streak_milestones, milestone_bit)
                    call platform_audio_beep(880.0, 0.06, 0.14)
                    call platform_audio_beep(1320.0, 0.06, 0.12)
                    call platform_audio_beep(1760.0, 0.08, 0.10)
                end if
            end if
        end do
    end subroutine add_streak

    subroutine break_streak(gs)
        type(game_state_t), intent(inout) :: gs
        if (gs%streak > 0) then
            gs%streak_break_flash = 1.0_rk
        end if
        gs%streak = 0
    end subroutine break_streak

    pure logical function phantom_visible(enemy, time) result(visible)
        type(enemy_t), intent(in) :: enemy
        real(rk), intent(in) :: time
        real(rk) :: phase

        if (enemy%variant /= variant_phantom) then
            visible = .true.
            return
        end if
        phase = sin(time * 6.8_rk + enemy%phase * 1.3_rk)
        visible = phase > -0.1_rk
    end function phantom_visible

    pure integer function active_boss_index(gs) result(index)
        type(game_state_t), intent(in) :: gs
        integer :: i

        index = 0
        do i = 1, max_enemies
            if (gs%enemies(i)%active .and. gs%enemies(i)%is_boss) then
                index = i
                return
            end if
        end do
    end function active_boss_index

    pure integer function transmission_line_count(transmission_id) result(line_count)
        integer, intent(in) :: transmission_id
        select case (transmission_id)
        case (tx_opening, tx_sector_two, tx_sector_three, tx_victory)
            line_count = 4
        case (tx_defeat)
            line_count = 3
        case default
            line_count = 0
        end select
    end function transmission_line_count

    pure function transmission_line(transmission_id, line_index) result(line)
        integer, intent(in) :: transmission_id
        integer, intent(in) :: line_index
        character(len=48) :: line

        line = ""
        select case (transmission_id)
        case (tx_opening)
            select case (line_index)
            case (1); line = ">> PHOSPHOR COMMAND TO PILOT 77"
            case (2); line = ">> COIL SCOUTS CROSSED THE PICKET LINE"
            case (3); line = ">> HOLD THE GATE. SOL CANNOT KNOW THEY FOUND US"
            case (4); line = ">> GOOD HUNTING, PHOSPHOR-LEAD"
            end select
        case (tx_sector_two)
            select case (line_index)
            case (1); line = ">> PICKET HELD. HARROWER DOWN"
            case (2); line = ">> ASTEROID LANE IS COIL TERRITORY NOW"
            case (3); line = ">> CUT THROUGH. DO NOT LINGER"
            case (4); line = ">> THE ROCKS ARE ALIVE"
            end select
        case (tx_sector_three)
            select case (line_index)
            case (1); line = ">> LAST TRANSMISSION, PHOSPHOR-LEAD"
            case (2); line = ">> THE MAW IS AWAKE AT THE GATE"
            case (3); line = ">> CLOSE IT OR THE COIL SEES HOME"
            case (4); line = ">> SOL IS WITH YOU"
            end select
        case (tx_victory)
            select case (line_index)
            case (1); line = ">> GATE IS DARK. COIL IS SILENT"
            case (2); line = ">> SOL CONFIRMS EVACUATION COMPLETE"
            case (3); line = ">> PHOSPHOR-LEAD, COME HOME"
            case (4); line = ">> WE LOG THIS ONE IN LEGEND"
            end select
        case (tx_defeat)
            select case (line_index)
            case (1); line = ">> PHOSPHOR SIGNAL LOST"
            case (2); line = ">> COIL ADVANCING ON SOL"
            case (3); line = ">> STAND BY FOR NEXT PILOT"
            end select
        end select
    end function transmission_line

end module game
