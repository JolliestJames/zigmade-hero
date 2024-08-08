const std = @import("std");
const assert = std.debug.assert;

const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;

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
    assert(value <= 0xffffffff);
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
    assert(index < input.controllers.len);
    const result = &input.controllers[index];
    return (result);
}

pub const BITMAP_BYTES_PER_PIXEL = 4;

// TODO: rendering will become a three-tiered abstraction
pub const GameOffscreenBuffer = struct {
    // NOTE: Pixels are always 32-bits wide
    // Memory order BB GG RR XX
    memory: ?*void = null,
    width: i32,
    height: i32,
    pitch: i32,
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
    executable_reloaded: bool = false,
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

pub inline fn rdtsc() u64 {
    var low: u32 = undefined;
    var high: u32 = undefined;

    asm ("rdtsc"
        : [low] "={eax}" (low),
          [high] "={edx}" (high),
    );

    return (@as(u64, @intCast((high))) << 32) | @as(u64, @intCast(low));
}

pub const internal = struct {
    pub var debug_global_memory: *GameMemory = undefined;

    pub const DebugCycleTimerType = enum(u32) {
        update_and_render,
        render_group_to_output,
        draw_rectangle_slowly,
        test_pixel,
        fill_pixel,
    };

    pub const DebugCycleTimer = struct {
        cycle_count: u64 = 0,
        hit_count: u64 = 0,
    };

    pub inline fn beginTimedBlock(id: DebugCycleTimerType) void {
        if (debug_global_memory.counters) |*counters| {
            counters[@intFromEnum(id)].cycle_count = rdtsc();
        }
    }

    pub inline fn endTimedBlock(id: DebugCycleTimerType) void {
        if (debug_global_memory.counters) |*counters| {
            counters[@intFromEnum(id)].cycle_count += (rdtsc() - counters[@intFromEnum(id)].cycle_count);
            counters[@intFromEnum(id)].hit_count += 1;
        }
    }
};

pub const debug_cycle_timer_count = @typeInfo(internal.DebugCycleTimerType).Enum.fields.len;

pub const GameMemory = struct {
    is_initialized: bool = false,
    permanent_storage_size: u32 = 0,
    // NOTE: Required to be cleared to zero at startup
    permanent_storage: [*]void = undefined,
    transient_storage_size: u32 = 0,
    transient_storage: [*]void = undefined,
    debugPlatformReadEntireFile: debugPlatformReadEntireFile,
    debugPlatformFreeFileMemory: debugPlatformFreeFileMemory,
    debugPlatformWriteEntireFile: debugPlatformWriteEntireFile,
    counters: ?[debug_cycle_timer_count]internal.DebugCycleTimer =
        if (INTERNAL) [_]internal.DebugCycleTimer{.{}} ** debug_cycle_timer_count else null,
};

pub const beginTimedBlock = internal.beginTimedBlock;
pub const endTimedBlock = internal.endTimedBlock;

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
