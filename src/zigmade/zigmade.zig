const std = @import("std");

// TODO: Services that the platform layer provides to the game

// NOTE: Services that the game provides to the platform layer
// maybe expand in the future - sound on separate thread

// TODO: rendering will become a three-tiered abstraction
pub const GameOffscreenBuffer = struct {
    memory: ?*anyopaque = undefined,
    width: i32 = undefined,
    height: i32 = undefined,
    pitch: i32 = undefined,
};

pub const SoundBuffer = struct {
    samples_per_second: i32 = undefined,
    sample_count: i32 = undefined,
    samples: [*]i16 = undefined,
};

fn game_output_sound(
    sound_buffer: *SoundBuffer,
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
    offscreen_buffer: *GameOffscreenBuffer,
    blue_offset: i32,
    green_offset: i32,
    sound_buffer: *SoundBuffer,
    tone_hertz: i32,
) !void {
    // TODO: Allow sample offsets here for more robust platform options
    try game_output_sound(sound_buffer, tone_hertz);
    try render_weird_gradient(
        offscreen_buffer,
        blue_offset,
        green_offset,
    );
}
