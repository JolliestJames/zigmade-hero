const std = @import("std");
const assert = std.debug.assert;
const platform = @import("zigmade_platform");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;

fn game_output_sound(
    sound_buffer: *platform.GameSoundBuffer,
    game_state: *platform.GameState,
) !void {
    const tone_volume: i16 = 1_000;
    var wave_period = @divTrunc(
        @as(i32, @intCast(sound_buffer.samples_per_second)),
        game_state.tone_hertz,
    );
    var sample_out = sound_buffer.samples;

    for (0..@intCast(sound_buffer.sample_count)) |i| {
        var sine_value = @sin(game_state.t_sine);
        var sample_value = @as(i16, @intFromFloat(
            sine_value * @as(
                f16,
                @floatFromInt(tone_volume),
            ),
        ));

        //if (INTERNAL) {
        //    sample_value = 0;
        //}

        sample_out[2 * i] = sample_value;
        sample_out[2 * i + 1] = sample_value;

        game_state.t_sine +=
            2.0 *
            std.math.pi *
            1.0 /
            @as(f32, @floatFromInt(wave_period));

        if (game_state.t_sine > 2.0 * std.math.pi) {
            game_state.t_sine -= 2.0 * std.math.pi;
        }
    }
}

fn render_weird_gradient(
    buffer: *platform.GameOffscreenBuffer,
    blue_offset: i32,
    green_offset: i32,
) !void {
    var row: [*]u8 = @alignCast(@ptrCast(buffer.memory));

    for (0..@intCast(buffer.height)) |y| {
        var pixel: [*]u32 = @ptrCast(@alignCast(row));

        for (0..@intCast(buffer.width)) |x| {
            var blue: u32 = @as(u8, @truncate(x + @as(
                u32,
                @bitCast(blue_offset),
            )));
            var green: u32 = @as(u8, @truncate(y + @as(
                u32,
                @bitCast(green_offset),
            )));

            pixel[x] = (green << 16) | blue;
        }

        row += @as(usize, @intCast(buffer.pitch));
    }
}

fn render_player(
    buffer: *platform.GameOffscreenBuffer,
    game_state: *platform.GameState,
) !void {
    var end_of_buffer: [*]u8 =
        @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
        @as(usize, @intCast(buffer.pitch * buffer.height));

    comptime var color = 0xffffffff;
    var top = game_state.player_y;

    for (0..10) |i| {
        var x = @as(u32, @bitCast(game_state.player_x));
        x +%= @intCast(i);
        var pixel: [*]u8 =
            @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
            (x * @as(usize, @intCast(buffer.bytes_per_pixel))) +
            @as(u32, @bitCast(top *% buffer.pitch));

        for (0..10) |_| {
            if (@intFromPtr(pixel) >= @intFromPtr(buffer.memory) and
                @intFromPtr(pixel + 4) <= @intFromPtr(end_of_buffer))
            {
                @as([*]u32, @alignCast(@ptrCast(pixel)))[0] = color;
            }

            pixel += @as(usize, @intCast(buffer.pitch));
        }
    }
}

// GAME NEEDS FOUR THINGS
// - timing
// - controller/keyboard input
// - bitmap buffer
// - sound buffer
pub export fn update_and_render(
    memory: *platform.GameMemory,
    input: *platform.GameInput,
    offscreen_buffer: *platform.GameOffscreenBuffer,
) void {
    assert(@sizeOf(@TypeOf(input.controllers[0].buttons.map)) ==
        @sizeOf(platform.GameButtonState) * input.controllers[0].buttons.array.len);
    assert(@sizeOf(platform.GameState) <= memory.permanent_storage_size);

    var game_state: *platform.GameState = @as(
        *platform.GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    if (!memory.is_initialized) {
        var file_name = "./src/zigmade/zigmade.zig";
        var file = memory.debug_platform_read_entire_file.?(file_name);

        if (file.contents != undefined) {
            _ = memory.debug_platform_write_entire_file.?(
                "test.out",
                file.size,
                file.contents,
            );

            memory.debug_platform_free_file_memory.?(file.contents);
        }

        game_state.tone_hertz = 512;
        game_state.t_sine = 0.0;
        game_state.player_x = 100;
        game_state.player_y = 100;

        // TODO: This may be more appropriate to do in the platform layer
        memory.is_initialized = true;
    }

    for (0..input.controllers.len) |controller_index| {
        var controller: *platform.GameControllerInput =
            try platform.get_controller(input, controller_index);

        if (controller.is_analog) {
            // NOTE: Use analog movement tuning
            game_state.blue_offset +%= @as(i32, @intFromFloat(
                4.0 * (controller.stick_average_x),
            ));
            game_state.tone_hertz = 512 +
                @as(i32, @intFromFloat(128.0 *
                (controller.stick_average_y)));
        } else {
            // NOTE: Use digital movement tuning
            if (controller.buttons.map.move_left.ended_down) {
                game_state.blue_offset -%= 1;
            }

            if (controller.buttons.map.move_right.ended_down) {
                game_state.blue_offset +%= 1;
            }
        }

        game_state.player_x +%= @intFromFloat(4.0 * controller.stick_average_x);
        game_state.player_y -%= @as(i32, @intFromFloat(4.0 * controller.stick_average_y));

        if (game_state.t_jump > 0) {
            game_state.player_y +%= @intFromFloat(5.0 * @sin(0.5 * std.math.pi * game_state.t_jump));
        }

        if (controller.buttons.map.action_down.ended_down) {
            game_state.t_jump = 4.0;
        }

        game_state.t_jump -= 0.033;
    }

    try render_weird_gradient(
        offscreen_buffer,
        game_state.blue_offset,
        game_state.green_offset,
    );
    try render_player(offscreen_buffer, game_state);
}

// NOTE: At the moment, this must be a very fast function
// It cannot be greater than ~1ms
// TODO: Reduce the pressure on this function's performance
// by measuring it or asking about it, etc.
pub export fn get_sound_samples(
    memory: *platform.GameMemory,
    sound_buffer: *platform.GameSoundBuffer,
) void {
    var game_state: *platform.GameState = @as(
        *platform.GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    try game_output_sound(
        sound_buffer,
        game_state,
    );
}
