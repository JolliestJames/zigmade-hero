const std = @import("std");
const assert = std.debug.assert;

pub inline fn Kilobytes(comptime value: comptime_int) comptime_int {
    return (1024 * value);
}

pub inline fn Megabytes(comptime value: comptime_int) comptime_int {
    return Kilobytes(value) * 1024;
}

pub inline fn Gigabytes(comptime value: comptime_int) comptime_int {
    return Megabytes(value) * 1024;
}

pub inline fn Terabytes(comptime value: comptime_int) comptime_int {
    return Gigabytes(value) * 1024;
}

pub inline fn safe_truncate_u64(value: u64) u32 {
    // TODO: consts for maximum values
    std.debug.assert(value <= 0xffffffff);
    return @as(u32, @truncate(value));
}

// NOTE: Services that the platform layer provides to the game

// IMORTANT: These are not for doing anything in the shipped game
// they are blocking and the write will not protect against
// lost data!
pub const DebugReadFileResult = struct {
    size: u32,
    contents: ?*anyopaque = undefined,
};

pub const Platform = struct {
    debug_platform_read_entire_file: *const fn ([*:0]const u8) DebugReadFileResult,
    debug_platform_free_file_memory: *const fn (?*anyopaque) void,
    debug_platform_write_entire_file: *const fn (
        [*:0]const u8,
        u32,
        ?*anyopaque,
    ) bool,
};

// NOTE: Services that the game provides to the platform layer
// maybe expand in the future - sound on separate thread

// TODO: rendering will become a three-tiered abstraction
pub const GameOffscreenBuffer = struct {
    memory: ?*void = undefined,
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

pub const GameButtonMap = extern struct {
    move_up: GameButtonState,
    move_down: GameButtonState,
    move_left: GameButtonState,
    move_right: GameButtonState,
    action_up: GameButtonState,
    action_down: GameButtonState,
    action_left: GameButtonState,
    action_right: GameButtonState,
    left_shoulder: GameButtonState,
    right_shoulder: GameButtonState,
    start: GameButtonState,
    back: GameButtonState,
};

pub const GameControllerInput = struct {
    is_connected: bool,
    is_analog: bool,
    stick_average_x: f32,
    stick_average_y: f32,

    buttons: extern union {
        array: [12]GameButtonState,
        map: GameButtonMap,
    },
};

pub const GameInput = struct {
    // TODO: Insert clock value here
    controllers: [5]GameControllerInput,
};

pub fn get_controller(input: *GameInput, index: usize) !*GameControllerInput {
    std.debug.assert(index < input.controllers.len);
    var result = &input.controllers[index];
    return (result);
}

pub const GameMemory = struct {
    is_initialized: bool = false,
    permanent_storage_size: u64,
    // NOTE: Required to be cleared to zero at startup
    permanent_storage: [*]u8 = undefined,
    transient_storage_size: u64,
    transient_storage: [*]u8 = undefined,
};

const GameState = struct {
    blue_offset: i32,
    green_offset: i32,
    tone_hertz: i32,
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
    platform: *const Platform,
    memory: *GameMemory,
    input: *GameInput,
    offscreen_buffer: *GameOffscreenBuffer,
    sound_buffer: *GameSoundBuffer,
) !void {
    assert(@sizeOf(@TypeOf(input.controllers[0].buttons.map)) ==
        @sizeOf(GameButtonState) * input.controllers[0].buttons.array.len);
    assert(@sizeOf(GameState) <= memory.permanent_storage_size);

    var game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    if (!memory.is_initialized) {
        var file_name = "./src/zigmade/zigmade.zig";
        var file = platform.debug_platform_read_entire_file(file_name);

        if (file.contents != undefined) {
            _ = platform.debug_platform_write_entire_file(
                "test.out",
                file.size,
                file.contents,
            );

            platform.debug_platform_free_file_memory(file.contents);
        }

        game_state.tone_hertz = 256;

        // TODO: This may be more appropriate to do in the platform layer
        memory.is_initialized = true;
    }

    for (0..input.controllers.len) |controller_index| {
        var controller: *GameControllerInput =
            try get_controller(input, controller_index);

        if (controller.is_analog) {
            // NOTE: Use analog movement tuning
            game_state.blue_offset += @as(i32, @intFromFloat(
                4.0 * (controller.stick_average_x),
            ));
            game_state.tone_hertz = 256 +
                @as(i32, @intFromFloat(128.0 *
                (controller.stick_average_y)));
        } else {
            // NOTE: Use digital movement tuning
            if (controller.buttons.map.move_left.ended_down) {
                game_state.blue_offset -= 1;
            }

            if (controller.buttons.map.move_right.ended_down) {
                game_state.blue_offset += 1;
            }
        }

        if (controller.buttons.map.action_down.ended_down) {
            game_state.green_offset += 1;
        }
    }

    // TODO: Allow sample offsets here for more robust platform options
    try game_output_sound(
        sound_buffer,
        game_state.tone_hertz,
    );
    try render_weird_gradient(
        offscreen_buffer,
        game_state.blue_offset,
        game_state.green_offset,
    );
}
