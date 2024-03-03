const std = @import("std");

// TODO: Services that the platform layer provides to the game

// NOTE: Services that the game provides to the platform layer
// maybe expand in the future - sound on separate thread

// TODO: rendering will become a three-tiered abstraction
pub const GameOffscreenBuffer = struct {
    memory: ?*anyopaque = undefined,
    width: i32,
    height: i32,
    pitch: i32,
};

pub const GameSoundBuffer = struct {
    samples: [*]i16 = undefined,
    samples_per_second: i32,
    sample_count: i32,
};

pub const GameButtonState = extern struct {
    half_transition_count: i32,
    ended_down: bool,
};

// TODO: maybe there's a better way to represent button state
// in the controller since fields may not be nameless in Zig
pub const GameControllerInput = struct {
    is_analog: bool,
    start_x: f32,
    start_y: f32,
    min_x: f32,
    min_y: f32,
    max_x: f32,
    max_y: f32,
    end_x: f32,
    end_y: f32,
    buttons: extern union {
        array: [6]GameButtonState,
        map: extern struct {
            up: GameButtonState,
            down: GameButtonState,
            left: GameButtonState,
            right: GameButtonState,
            left_shoulder: GameButtonState,
            right_shoulder: GameButtonState,
        },
    },
};

pub const GameInput = struct {
    controllers: [4]GameControllerInput,
};

fn game_output_sound(
    sound_buffer: *GameSoundBuffer,
    tone_hertz: i32,
) !void {
    const t_sine = struct {
        var value: f32 = undefined;
    };

    const tone_volume: i16 = 3_000;
    var wave_period = @divTrunc(sound_buffer.samples_per_second, tone_hertz);
    var sample_out: [*]i16 = sound_buffer.samples;

    for (0..@as(usize, @intCast(sound_buffer.sample_count))) |i| {
        var sine_value: f32 = std.math.sin(t_sine.value);
        var sample_value: i16 = @as(i16, @intFromFloat(
            sine_value * @as(
                f32,
                @floatFromInt(tone_volume),
            ),
        ));

        sample_out[2 * i] = sample_value;
        sample_out[2 * i + 1] = sample_value;

        t_sine.value +=
            2.0 *
            std.math.pi *
            1.0 /
            @as(f32, @floatFromInt(wave_period));
    }
}

fn render_weird_gradient(
    buffer: *GameOffscreenBuffer,
    blue_offset: i32,
    green_offset: i32,
) !void {
    var row: [*]u8 = @ptrCast(buffer.memory);

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

            pixel[x] = (green << 8) | blue;
        }

        row += @as(usize, @intCast(buffer.pitch));
    }
}

// GAME NEEDS FOUR THINGS
// - timing
// - controller/keyboard input
// - bitmap buffer
// - sound buffer
pub fn game_update_and_render(
    input: *GameInput,
    offscreen_buffer: *GameOffscreenBuffer,
    sound_buffer: *GameSoundBuffer,
) !void {
    const state = struct {
        var blue_offset: i32 = 0;
        var green_offset: i32 = 0;
        var tone_hertz: i32 = 256;
    };

    var input_0: *GameControllerInput = &input.controllers[0];

    if (input_0.is_analog) {
        // NOTE: Use analog movement tuning
        state.blue_offset += @as(i32, @intFromFloat(4.0 * (input_0.end_x)));
        state.tone_hertz = 256 + @as(i32, @intFromFloat(128.0 * (input_0.end_y)));
    } else {
        // NOTE: Use digital movement tuning
    }

    if (input_0.buttons.map.down.ended_down) {
        state.green_offset += 1;
    }

    // TODO: Allow sample offsets here for more robust platform options
    try game_output_sound(
        sound_buffer,
        state.tone_hertz,
    );
    try render_weird_gradient(
        offscreen_buffer,
        state.blue_offset,
        state.green_offset,
    );
}
