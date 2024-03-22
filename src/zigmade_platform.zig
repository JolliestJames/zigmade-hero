const std = @import("std");

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

// NOTE: Services that the game provides to the platform layer
// maybe expand in the future - sound on separate thread

pub fn get_controller(
    input: *GameInput,
    index: usize,
) !*GameControllerInput {
    std.debug.assert(index < input.controllers.len);
    var result = &input.controllers[index];
    return (result);
}

// TODO: rendering will become a three-tiered abstraction
pub const GameOffscreenBuffer = struct {
    memory: ?*void = undefined,
    width: i32,
    height: i32,
    pitch: i32,
    bytes_per_pixel: i32,
};

pub const GameSoundBuffer = struct {
    samples: [*]i16 = undefined,
    samples_per_second: u32,
    sample_count: u32,
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

pub const GameMemory = struct {
    is_initialized: bool = false,
    permanent_storage_size: u64 = 0,
    // NOTE: Required to be cleared to zero at startup
    permanent_storage: [*]u8 = undefined,
    transient_storage_size: u64 = 0,
    transient_storage: [*]u8 = undefined,
    debug_platform_read_entire_file: ?*const fn ([*:0]const u8) DebugReadFileResult = undefined,
    debug_platform_free_file_memory: ?*const fn (?*anyopaque) void = undefined,
    debug_platform_write_entire_file: ?*const fn (
        [*:0]const u8,
        u32,
        ?*anyopaque,
    ) bool = undefined,
};

pub const GameState = struct {
    blue_offset: i32,
    green_offset: i32,
    tone_hertz: i32,
    t_sine: f32,
    player_x: i32,
    player_y: i32,
    t_jump: f32,
};

// NOTE: Services that the platform layer provides to the game

// IMORTANT: These are not for doing anything in the shipped game
// they are blocking and the write will not protect against
// lost data!
pub const DebugReadFileResult = struct {
    size: u32,
    contents: ?*anyopaque = undefined,
};

pub const update_and_render_type = *const fn (
    *GameMemory,
    *GameInput,
    *GameOffscreenBuffer,
) callconv(.C) void;

pub const get_sound_samples_type = *const fn (
    *GameMemory,
    *GameSoundBuffer,
) callconv(.C) void;
