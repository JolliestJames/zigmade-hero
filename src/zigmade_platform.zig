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

pub inline fn safeTruncateu64(value: u64) u32 {
    // TODO: consts for maximum values
    std.debug.assert(value <= 0xffffffff);
    return @as(u32, @truncate(value));
}

pub const ThreadContext = struct {
    placeholder: i32,
};

// NOTE: Services that the platform layer provides to the game

// IMORTANT: These are not for doing anything in the shipped game
// they are blocking and the write will not protect against
// lost data!
pub const DebugReadFileResult = struct {
    size: u32 = 0,
    contents: ?*anyopaque = null,
};

// NOTE: Services that the game provides to the platform layer
// maybe expand in the future - sound on separate thread

pub inline fn getController(
    input: *GameInput,
    index: usize,
) !*GameControllerInput {
    std.debug.assert(index < input.controllers.len);
    const result = &input.controllers[index];
    return (result);
}

// TODO: rendering will become a three-tiered abstraction
pub const GameOffscreenBuffer = struct {
    memory: ?*void = null,
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
    mouse_buttons: [5]GameButtonState,
    mouse_x: i32 = 0,
    mouse_y: i32 = 0,
    mouse_z: i32 = 0,
    dt_for_frame: f32 = 0,
    controllers: [5]GameControllerInput,
};

pub const debugPlatformReadEntireFile = *const fn (
    *ThreadContext,
    [*:0]const u8,
) DebugReadFileResult;

const debugPlatformFreeFileMemory = *const fn (
    *ThreadContext,
    ?*anyopaque,
) void;

const debugPlatformWriteEntireFile = *const fn (
    *ThreadContext,
    [*:0]const u8,
    u32,
    ?*anyopaque,
) bool;

pub const GameMemory = struct {
    is_initialized: bool = false,
    permanent_storage_size: u32 = 0,
    // NOTE: Required to be cleared to zero at startup
    permanent_storage: [*]u8 = undefined,
    transient_storage_size: u32 = 0,
    transient_storage: [*]u8 = undefined,
    debugPlatformReadEntireFile: debugPlatformReadEntireFile,
    debugPlatformFreeFileMemory: debugPlatformFreeFileMemory,
    debugPlatformWriteEntireFile: debugPlatformWriteEntireFile,
};

pub const updateAndRender = *const fn (
    *ThreadContext,
    *GameMemory,
    *GameInput,
    *GameOffscreenBuffer,
) callconv(.C) void;

pub const getSoundSamples = *const fn (
    *ThreadContext,
    *GameMemory,
    *GameSoundBuffer,
) callconv(.C) void;
