//
// TODO:
// - Saved game locations
// - Getting a handle to our own exe
// - Asset loading path
// - Threading
// - Raw input
// - Sleep/timeBeginPeriod
// - ClipCursor() for multimonitor support
// - Fullscreen support
// - WM_SETCURSOR to control cursor visibilty
// - QueryCancelAutoplay
// - WM_ACTIVATEAPP for when we are not the active application
// - Blit speed improvements with BitBlt
// - Hardware acceleration (OpenGL/Direct3D/Both)
// - GetKeyboardLayout for French keyboards/intnl WASD support
//

const std = @import("std");
const platform = @import("zigmade_platform");

const DWORD = std.os.windows.DWORD;
const WINAPI = std.os.windows.WINAPI;
const DEBUG_WALL_CLOCK = @import("options").DEBUG_WALL_CLOCK;
const DEBUG_SYNC_DISPLAY = @import("options").DEBUG_SYNC_DISPLAY;
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;
const WIN32_STATE_FILE_NAME_COUNT = win32.MAX_PATH;
const WIN32_GENERIC_READ_WRITE = win32.FILE_ACCESS_FLAGS{
    .FILE_READ_DATA = 1,
    .FILE_READ_EA = 1,
    .FILE_READ_ATTRIBUTES = 1,
    .READ_CONTROL = 1,
    .FILE_WRITE_DATA = 1,
    .FILE_APPEND_DATA = 1,
    .FILE_WRITE_EA = 1,
    .FILE_WRITE_ATTRIBUTES = 1,
    .SYNCHRONIZE = 1,
};

const win32 = struct {
    usingnamespace @import("win32").foundation;
    usingnamespace @import("win32").graphics.gdi;
    usingnamespace @import("win32").media;
    usingnamespace @import("win32").media.audio;
    usingnamespace @import("win32").media.audio.direct_sound;
    usingnamespace @import("win32").storage.file_system;
    usingnamespace @import("win32").system;
    usingnamespace @import("win32").system.io;
    usingnamespace @import("win32").system.ioctl;
    usingnamespace @import("win32").system.com;
    usingnamespace @import("win32").system.diagnostics.debug;
    usingnamespace @import("win32").system.memory;
    usingnamespace @import("win32").system.performance;
    usingnamespace @import("win32").system.library_loader;
    usingnamespace @import("win32").system.threading;
    usingnamespace @import("win32").ui.input.keyboard_and_mouse;
    usingnamespace @import("win32").ui.input.xbox_controller;
    usingnamespace @import("win32").ui.windows_and_messaging;
    usingnamespace @import("win32").zig;
};

const BackBuffer = struct {
    memory: ?*void = undefined,
    info: win32.BITMAPINFO,
    width: i32,
    height: i32,
    pitch: i32,
    bytes_per_pixel: i32 = 4,
};

const WindowDimension = struct {
    width: i32,
    height: i32,
};

const Win32DebugTimeMarker = struct {
    output_play_cursor: DWORD,
    output_write_cursor: DWORD,
    output_location: DWORD,
    output_byte_count: DWORD,
    expected_flip_play_cursor: DWORD,
    flip_play_cursor: DWORD,
    flip_write_cursor: DWORD,
};

const Win32GameCode = struct {
    dll: ?win32.HINSTANCE = null,
    dll_last_write_time: win32.FILETIME = undefined,
    update_and_render: ?platform.update_and_render_type = null,
    get_sound_samples: ?platform.get_sound_samples_type = null,
    is_valid: bool = false,
};

const Win32SoundOutput = struct {
    samples_per_second: u32,
    running_sample_index: u32,
    bytes_per_sample: u32,
    secondary_buffer_size: DWORD,
    safety_bytes: DWORD,
    // TODO: Should running sample index be in bytes as well?
    // TODO: Math gets simpler if we add a "bytes per second" field?
};

const Win32ReplayBuffer = struct {
    file_handle: ?win32.HANDLE = undefined,
    memory_map: ?win32.HANDLE = undefined,
    file_name: [WIN32_STATE_FILE_NAME_COUNT:0]u8 =
        [_:0]u8{0} ** WIN32_STATE_FILE_NAME_COUNT,
    memory_block: ?*anyopaque = undefined,
};

const Win32State = struct {
    total_size: u64,
    game_memory_block: ?*anyopaque = undefined,
    replay_buffers: [4]Win32ReplayBuffer,
    recording_handle: ?win32.HANDLE = undefined,
    input_recording_index: usize,
    playback_handle: ?win32.HANDLE = undefined,
    input_playing_index: usize,
    exe_file_name: [WIN32_STATE_FILE_NAME_COUNT:0]u8 =
        [_:0]u8{0} ** WIN32_STATE_FILE_NAME_COUNT,
    one_past_last_exe_file_name_slash: usize,
};

const XInputGetState = struct {
    var call: *const fn (
        user_index: DWORD,
        state: ?*win32.XINPUT_STATE,
    ) callconv(WINAPI) isize = undefined;

    fn stub(_: DWORD, _: ?*win32.XINPUT_STATE) callconv(WINAPI) isize {
        return (@intFromEnum(win32.ERROR_DEVICE_NOT_CONNECTED));
    }
};

const XInputSetState = struct {
    var call: *const fn (
        user_index: DWORD,
        vibration: ?*win32.XINPUT_VIBRATION,
    ) callconv(WINAPI) isize = undefined;

    fn stub(_: DWORD, _: ?*win32.XINPUT_VIBRATION) callconv(WINAPI) isize {
        return (@intFromEnum(win32.ERROR_DEVICE_NOT_CONNECTED));
    }
};

// TODO: Use globals for now
var global_running: bool = false;
var global_pause: bool = false;
var global_back_buffer: BackBuffer = undefined;
var global_secondary_buffer: *win32.IDirectSoundBuffer = undefined;
var global_perf_count_frequency: i64 = undefined;

fn debug_platform_read_entire_file(
    thread: *platform.ThreadContext,
    file_name: [*:0]const u8,
) platform.DebugReadFileResult {
    var result: platform.DebugReadFileResult = undefined;

    const handle = win32.CreateFileA(
        file_name,
        win32.FILE_GENERIC_READ,
        win32.FILE_SHARE_READ,
        null,
        win32.OPEN_EXISTING,
        win32.FILE_FLAGS_AND_ATTRIBUTES{},
        null,
    );

    if (handle != win32.INVALID_HANDLE_VALUE) {
        var file_size: win32.LARGE_INTEGER = undefined;

        if (win32.GetFileSizeEx(handle, &file_size) > 0) {
            const file_size32 = platform.safe_truncate_u64(@as(u64, @intCast(file_size.QuadPart)));

            result.contents = win32.VirtualAlloc(
                null,
                file_size32,
                win32.VIRTUAL_ALLOCATION_TYPE{ .RESERVE = 1, .COMMIT = 1 },
                win32.PAGE_READWRITE,
            );

            if (result.contents) |contents| {
                var bytes_read: DWORD = undefined;

                if (win32.ReadFile(
                    handle,
                    contents,
                    file_size32,
                    &bytes_read,
                    null,
                ) > 0 and file_size32 == bytes_read) {
                    // NOTE: File read successfully
                    result.size = file_size32;
                } else {
                    // TODO: Logging
                    debug_platform_free_file_memory(thread, result.contents);
                    result.contents = null;
                }
            } else {
                // TODO: Logging
            }
        } else {
            // TODO: Logging
        }

        _ = win32.CloseHandle(handle);
    } else {
        // TODO: Logging
    }

    return result;
}

fn debug_platform_write_entire_file(
    thread: *platform.ThreadContext,
    file_name: [*:0]const u8,
    memory_size: u32,
    memory: ?*anyopaque,
) bool {
    _ = thread;
    var result = false;

    const handle = win32.CreateFileA(
        file_name,
        win32.FILE_GENERIC_WRITE,
        win32.FILE_SHARE_NONE,
        null,
        win32.CREATE_ALWAYS,
        win32.FILE_FLAGS_AND_ATTRIBUTES{},
        null,
    );

    if (handle != win32.INVALID_HANDLE_VALUE) {
        var bytes_written: DWORD = undefined;

        if (win32.WriteFile(
            handle,
            memory,
            memory_size,
            &bytes_written,
            null,
        ) > 0) {
            // NOTE: File written successfully
            result = (bytes_written == memory_size);
        } else {
            // TODO: Logging
        }

        _ = win32.CloseHandle(handle);
    } else {
        // TODO: Logging
    }

    return result;
}

fn debug_platform_free_file_memory(
    thread: *platform.ThreadContext,
    maybe_memory: ?*anyopaque,
) void {
    _ = thread;
    if (maybe_memory) |memory| {
        _ = win32.VirtualFree(memory, 0, win32.MEM_RELEASE);
    }
}

inline fn win32_get_last_write_time(
    file_name: [*:0]const u8,
) win32.FILETIME {
    var last_write_time = std.mem.zeroInit(win32.FILETIME, .{});

    var data: win32.WIN32_FILE_ATTRIBUTE_DATA = undefined;

    if (win32.GetFileAttributesExA(
        file_name,
        win32.GetFileExInfoStandard,
        &data,
    ) == win32.TRUE) {
        last_write_time = data.ftLastWriteTime;
    }

    return last_write_time;
}

fn win32_load_game_code(
    source_dll_name: [*:0]const u8,
    temp_dll_name: [*:0]const u8,
) Win32GameCode {
    var result = Win32GameCode{};

    result.dll_last_write_time = win32_get_last_write_time(source_dll_name);

    _ = win32.CopyFileA(source_dll_name, temp_dll_name, win32.FALSE);
    result.dll = win32.LoadLibraryA(temp_dll_name);

    if (result.dll != null) {
        result.update_and_render =
            @ptrCast(win32.GetProcAddress(result.dll, "update_and_render"));

        result.get_sound_samples =
            @ptrCast(win32.GetProcAddress(result.dll, "get_sound_samples"));

        if (result.update_and_render != null and result.get_sound_samples != null) {
            result.is_valid = true;
        }
    }

    if (!result.is_valid) {
        result.update_and_render = null;
        result.get_sound_samples = null;
    }

    return result;
}

fn win32_unload_game_code(game: *Win32GameCode) void {
    if (game.dll) |dll| {
        _ = win32.FreeLibrary(dll);
        game.dll = null;
    }

    game.is_valid = false;
    game.update_and_render = null;
    game.get_sound_samples = null;
}

fn win32_load_x_input() void {
    if (win32.LoadLibraryA(win32.XINPUT_DLL)) |x_input_library| {
        if (win32.GetProcAddress(
            x_input_library,
            "XInputGetState",
        )) |x_input_get_state| {
            XInputGetState.call = @as(
                @TypeOf(XInputGetState.call),
                @ptrCast(x_input_get_state),
            );
        } else {
            XInputGetState.call = XInputGetState.stub;
        }

        if (win32.GetProcAddress(
            x_input_library,
            "XInputSetState",
        )) |x_input_set_state| {
            XInputSetState.call = @as(
                @TypeOf(XInputSetState.call),
                @ptrCast(x_input_set_state),
            );
        } else {
            XInputSetState.call = XInputSetState.stub;
        }

        // TODO: diagnostic
    } else {
        // TODO: diagnostic
    }
}

var direct_sound_create: *const fn (
    guid_device: ?*const win32.Guid,
    pp_ds: ?*?*win32.IDirectSound,
    unknown_outer: ?*win32.IUnknown,
) callconv(WINAPI) win32.HRESULT = undefined;

fn win32_init_direct_sound(
    window: win32.HWND,
    samples_per_second: u32,
    buffer_size: u32,
) void {
    if (win32.LoadLibraryA("dsound.dll")) |direct_sound_library| {
        if (win32.GetProcAddress(
            direct_sound_library,
            "DirectSoundCreate",
        )) |direct_sound_create_address| {
            direct_sound_create = @as(
                @TypeOf(direct_sound_create),
                @ptrCast(direct_sound_create_address),
            );

            var maybe_direct_sound: ?*win32.IDirectSound = undefined;

            if (win32.SUCCEEDED(direct_sound_create(
                null,
                &maybe_direct_sound,
                null,
            ))) {
                var direct_sound = maybe_direct_sound.?;

                var wave_format = std.mem.zeroInit(win32.WAVEFORMATEX, .{});
                wave_format.wFormatTag = win32.WAVE_FORMAT_PCM;
                wave_format.nChannels = 2;
                wave_format.nSamplesPerSec = @intCast(samples_per_second);
                wave_format.wBitsPerSample = 16;
                wave_format.nBlockAlign = (wave_format.nChannels * wave_format.wBitsPerSample) / 8;
                wave_format.nAvgBytesPerSec = wave_format.nSamplesPerSec * wave_format.nBlockAlign;
                wave_format.cbSize = 0;

                if (win32.SUCCEEDED(direct_sound.vtable.SetCooperativeLevel(
                    direct_sound,
                    window,
                    win32.DSSCL_PRIORITY,
                ))) {
                    var buffer_description = std.mem.zeroInit(win32.DSBUFFERDESC, .{});
                    buffer_description.dwSize = @sizeOf(@TypeOf(buffer_description));
                    buffer_description.dwFlags = win32.DSBCAPS_PRIMARYBUFFER;
                    var maybe_primary_buffer: ?*win32.IDirectSoundBuffer = undefined;

                    if (win32.SUCCEEDED(direct_sound.vtable.CreateSoundBuffer(
                        direct_sound,
                        &buffer_description,
                        &maybe_primary_buffer,
                        null,
                    ))) {
                        var primary_buffer = maybe_primary_buffer.?;

                        if (win32.SUCCEEDED(primary_buffer.vtable.SetFormat(
                            primary_buffer,
                            &wave_format,
                        ))) {
                            std.debug.print("Primary buffer format set.\n", .{});
                        } else {
                            // TODO: diagnostic
                        }
                    } else {
                        // TODO: diagnostic
                    }
                } else {
                    // TODO: diagnostic
                }

                var buffer_description = std.mem.zeroInit(win32.DSBUFFERDESC, .{});
                buffer_description.dwSize = @sizeOf(win32.DSBUFFERDESC);
                buffer_description.dwFlags = win32.DSBCAPS_GETCURRENTPOSITION2;
                buffer_description.dwBufferBytes = @intCast(buffer_size);
                buffer_description.lpwfxFormat = &wave_format;
                var maybe_secondary_buffer: ?*win32.IDirectSoundBuffer = undefined;

                if (win32.SUCCEEDED(direct_sound.vtable.CreateSoundBuffer(
                    direct_sound,
                    &buffer_description,
                    &maybe_secondary_buffer,
                    null,
                ))) {
                    global_secondary_buffer = maybe_secondary_buffer.?;
                    std.debug.print("Secondary buffer format created.\n", .{});
                } else {
                    // TODO: diagnostic
                }
            } else {
                // TODO: diagnostic
            }
        }
    } else {
        // TODO: diagnostic
    }
}

fn win32_get_window_dimension(window: win32.HWND) WindowDimension {
    var result: WindowDimension = undefined;

    var client_rect: win32.RECT = undefined;
    _ = win32.GetClientRect(window, &client_rect);

    result.width = client_rect.right - client_rect.left;
    result.height = client_rect.bottom - client_rect.top;

    return (result);
}

fn win32_resize_dib_section(
    buffer: *BackBuffer,
    width: i32,
    height: i32,
) void {
    // TODO: Maybe don't free first, free after, then free first if that fails
    if (buffer.memory != undefined) {
        _ = win32.VirtualFree(buffer.memory, 0, win32.MEM_RELEASE);
    }

    buffer.width = width;
    buffer.height = height;
    buffer.bytes_per_pixel = 4;

    buffer.info.bmiHeader.biSize = @sizeOf(@TypeOf(buffer.info.bmiHeader));
    buffer.info.bmiHeader.biWidth = buffer.width;
    buffer.info.bmiHeader.biHeight = -buffer.height;
    buffer.info.bmiHeader.biPlanes = 1;
    buffer.info.bmiHeader.biBitCount = 32;
    buffer.info.bmiHeader.biCompression = win32.BI_RGB;

    const bitmap_memory_size = buffer.bytes_per_pixel * (buffer.width * buffer.height);
    buffer.memory = @ptrCast(win32.VirtualAlloc(
        null,
        @intCast(bitmap_memory_size),
        win32.VIRTUAL_ALLOCATION_TYPE{ .RESERVE = 1, .COMMIT = 1 },
        win32.PAGE_READWRITE,
    ));

    buffer.pitch = width * buffer.bytes_per_pixel;

    // TODO: clear bitmap to black
}

fn win32_display_buffer_in_window(
    buffer: *BackBuffer,
    device_context: ?win32.HDC,
    window_width: i32,
    window_height: i32,
) void {
    const offset_x = 10;
    const offset_y = 10;

    _ = win32.PatBlt(
        device_context,
        0,
        0,
        window_width,
        offset_y,
        win32.BLACKNESS,
    );
    _ = win32.PatBlt(
        device_context,
        0,
        offset_y + buffer.height,
        window_width,
        window_height,
        win32.BLACKNESS,
    );
    _ = win32.PatBlt(
        device_context,
        0,
        0,
        offset_x,
        window_height,
        win32.BLACKNESS,
    );
    _ = win32.PatBlt(
        device_context,
        offset_x + buffer.width,
        0,
        window_width,
        window_height,
        win32.BLACKNESS,
    );

    // NOTE: For prototyping purposes, we're going to always blit
    // 1:1 pixels to make sure we don't introduce artifacts with
    // stretching while we are learning to code the renderer
    _ = win32.StretchDIBits(
        device_context,
        offset_x,
        offset_y,
        buffer.width,
        buffer.height,
        0,
        0,
        buffer.width,
        buffer.height,
        buffer.memory,
        &buffer.info,
        win32.DIB_RGB_COLORS,
        win32.SRCCOPY,
    );
}

fn win32_main_window_callback(
    window: win32.HWND,
    message: u32,
    w_param: win32.WPARAM,
    l_param: win32.LPARAM,
) callconv(WINAPI) win32.LRESULT {
    var result: win32.LRESULT = 0;

    switch (message) {
        win32.WM_CLOSE => {
            // TODO: Handle with a message to the user?
            global_running = false;
        },
        win32.WM_ACTIVATEAPP => {
            if (false) {
                if (w_param == win32.TRUE) {
                    _ = win32.SetLayeredWindowAttributes(
                        window,
                        0,
                        255,
                        win32.LWA_ALPHA,
                    );
                } else {
                    _ = win32.SetLayeredWindowAttributes(
                        window,
                        0,
                        64,
                        win32.LWA_ALPHA,
                    );
                }
            }
        },
        win32.WM_DESTROY => {
            // TODO: Handle as an error--recreate window?
            global_running = false;
        },
        win32.WM_SYSKEYDOWN,
        win32.WM_SYSKEYUP,
        win32.WM_KEYDOWN,
        win32.WM_KEYUP,
        => {
            std.debug.print("Keyboard input came in through a non-dispatch message!", .{});
            std.debug.assert(false);
        },
        win32.WM_PAINT => {
            var paint = std.mem.zeroInit(win32.PAINTSTRUCT, .{});
            const device_context = win32.BeginPaint(window, &paint);
            defer _ = win32.EndPaint(window, &paint);
            const dimension = win32_get_window_dimension(window);
            win32_display_buffer_in_window(
                &global_back_buffer,
                device_context,
                dimension.width,
                dimension.height,
            );
        },
        else => {
            result = win32.DefWindowProcW(window, message, w_param, l_param);
        },
    }

    return (result);
}

fn win32_clear_buffer(sound_output: *Win32SoundOutput) void {
    var region_1: ?*anyopaque = undefined;
    var region_1_size: DWORD = undefined;
    var region_2: ?*anyopaque = undefined;
    var region_2_size: DWORD = undefined;

    if (win32.SUCCEEDED(global_secondary_buffer.vtable.Lock(
        global_secondary_buffer,
        0,
        sound_output.secondary_buffer_size,
        &region_1,
        &region_1_size,
        &region_2,
        &region_2_size,
        0,
    ))) {
        if (region_1) |region| {
            var dest_sample: [*]i8 = @alignCast(@ptrCast(region));

            for (0..region_1_size) |i| {
                dest_sample[i] = 0;
            }
        }

        if (region_2) |region| {
            var dest_sample: [*]i8 = @alignCast(@ptrCast(region));

            for (0..region_2_size) |i| {
                dest_sample[i] = 0;
            }
        }

        _ = global_secondary_buffer.vtable.Unlock(
            global_secondary_buffer,
            region_1,
            region_1_size,
            region_2,
            region_2_size,
        );
    }
}

fn win32_fill_sound_buffer(
    sound_output: *Win32SoundOutput,
    byte_to_lock: DWORD,
    bytes_to_write: DWORD,
    source_buffer: *platform.GameSoundBuffer,
) void {
    var region_1: ?*anyopaque = undefined;
    var region_1_size: DWORD = undefined;
    var region_2: ?*anyopaque = undefined;
    var region_2_size: DWORD = undefined;

    if (win32.SUCCEEDED(global_secondary_buffer.vtable.Lock(
        global_secondary_buffer,
        byte_to_lock,
        bytes_to_write,
        &region_1,
        &region_1_size,
        &region_2,
        &region_2_size,
        0,
    ))) {
        std.debug.assert(region_1_size + region_2_size == bytes_to_write);

        const source_sample = source_buffer.samples;
        const region_1_sample_count: DWORD = region_1_size /
            @as(DWORD, @intCast(sound_output.bytes_per_sample));
        const offset: DWORD = region_1_sample_count * 2;
        const region_2_sample_count: DWORD = region_2_size /
            @as(DWORD, @intCast(sound_output.bytes_per_sample));

        if (region_1) |region| {
            var dest_sample: [*]i16 = @alignCast(@ptrCast(region));

            for (0..region_1_sample_count) |i| {
                dest_sample[i * 2] = source_sample[i * 2];
                dest_sample[i * 2 + 1] = source_sample[i * 2 + 1];

                sound_output.running_sample_index += 1;
            }
        }

        if (region_2) |region| {
            var dest_sample: [*]i16 = @alignCast(@ptrCast(region));

            for (0..region_2_sample_count) |i| {
                dest_sample[i * 2] = source_sample[offset + i * 2];
                dest_sample[i * 2 + 1] = source_sample[offset + i * 2 + 1];

                sound_output.running_sample_index += 1;
            }
        }

        _ = global_secondary_buffer.vtable.Unlock(
            global_secondary_buffer,
            region_1,
            region_1_size,
            region_2,
            region_2_size,
        );
    }
}

fn win32_process_keyboard_message(
    new_state: *platform.GameButtonState,
    is_down: bool,
) void {
    if (new_state.ended_down != is_down) {
        new_state.ended_down = is_down;
        new_state.half_transition_count += 1;
    }
}

fn win32_process_x_input_digital_button(
    button_state: DWORD,
    old_state: *platform.GameButtonState,
    new_state: *platform.GameButtonState,
    button_bit: DWORD,
) void {
    new_state.ended_down = ((button_state & button_bit) == button_bit);
    new_state.half_transition_count =
        if (old_state.ended_down != new_state.ended_down) 1 else 0;
}

fn win32_process_x_input_stick_value(
    value: i16,
    deadzone_threshold: i16,
) f32 {
    var result: f32 = 0;

    if (value < -deadzone_threshold) {
        result = @as(f32, @floatFromInt(value + deadzone_threshold)) /
            (32768.0 - @as(f32, @floatFromInt(deadzone_threshold)));
    } else if (value > deadzone_threshold) {
        result = @as(f32, @floatFromInt(value - deadzone_threshold)) /
            (32767.0 - @as(f32, @floatFromInt(deadzone_threshold)));
    }

    return result;
}

fn win32_process_pending_messages(
    state: *Win32State,
    keyboard_controller: *platform.GameControllerInput,
) void {
    var message: win32.MSG = undefined;

    while (win32.PeekMessageW(
        &message,
        null,
        0,
        0,
        win32.PM_REMOVE,
    ) > 0) {
        switch (message.message) {
            win32.WM_QUIT => global_running = false,
            win32.WM_SYSKEYDOWN,
            win32.WM_SYSKEYUP,
            win32.WM_KEYDOWN,
            win32.WM_KEYUP,
            => {
                const vk_code: win32.VIRTUAL_KEY = @enumFromInt(message.wParam);
                const was_down = (message.lParam & (1 << 30)) != 0;
                const is_down = (message.lParam & (1 << 31)) == 0;

                if (was_down != is_down) {
                    switch (vk_code) {
                        win32.VK_W => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.move_up,
                            is_down,
                        ),

                        win32.VK_A => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.move_left,
                            is_down,
                        ),

                        win32.VK_S => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.move_down,
                            is_down,
                        ),
                        win32.VK_D => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.move_right,
                            is_down,
                        ),
                        win32.VK_Q => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.left_shoulder,
                            is_down,
                        ),
                        win32.VK_E => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.right_shoulder,
                            is_down,
                        ),
                        win32.VK_UP => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.action_up,
                            is_down,
                        ),
                        win32.VK_DOWN => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.action_down,
                            is_down,
                        ),
                        win32.VK_LEFT => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.action_left,
                            is_down,
                        ),
                        win32.VK_RIGHT => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.action_right,
                            is_down,
                        ),
                        win32.VK_ESCAPE => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.start,
                            is_down,
                        ),
                        win32.VK_SPACE => win32_process_keyboard_message(
                            &keyboard_controller.buttons.map.back,
                            is_down,
                        ),
                        win32.VK_P => {
                            if (INTERNAL and is_down) {
                                global_pause = !global_pause;
                            }
                        },
                        win32.VK_L => {
                            if (is_down) {
                                if (state.input_playing_index == 0) {
                                    if (state.input_recording_index == 0) {
                                        win32_begin_recording_input(state, 1);
                                    } else {
                                        win32_end_recording_input(state);
                                        win32_begin_input_play_back(state, 1);
                                    }
                                } else {
                                    win32_end_input_play_back(state);
                                }
                            }
                        },
                        else => {},
                    }
                }

                const alt_key_was_down: bool = (message.lParam & (1 << 29)) != 0;
                if (vk_code == win32.VK_F4 and alt_key_was_down) {
                    global_running = false;
                }
            },
            else => {
                _ = win32.TranslateMessage(&message);
                _ = win32.DispatchMessageW(&message);
            },
        }
    }
}

inline fn rdtsc() u64 {
    var low: u32 = undefined;
    var high: u32 = undefined;

    asm volatile ("rdtsc"
        : [low] "={eax}" (low),
          [high] "={edx}" (high),
    );

    return (@as(u64, @intCast((high))) << 32) | @as(u64, @intCast(low));
}

inline fn win32_get_wall_clock() win32.LARGE_INTEGER {
    var result: win32.LARGE_INTEGER = undefined;
    _ = win32.QueryPerformanceCounter(&result);
    return (result);
}

inline fn win32_get_seconds_elapsed(
    start: win32.LARGE_INTEGER,
    end: win32.LARGE_INTEGER,
) f32 {
    return (@as(f32, @floatFromInt(end.QuadPart - start.QuadPart)) /
        @as(f32, @floatFromInt(global_perf_count_frequency)));
}

// TODO: Move debug sync display code into platform API once we
// have a real renderer
inline fn win32_debug_sync_display(
    back_buffer: *BackBuffer,
    markers: []Win32DebugTimeMarker,
    current_marker_index: i32,
    sound_output: *Win32SoundOutput,
    target_seconds_per_frame: f32,
) void {
    _ = target_seconds_per_frame;
    // TODO: Draw where we're writing out sound

    const pad_x = 16;
    const pad_y = 16;

    const line_height = 64;

    const coefficient = @as(
        f32,
        @floatFromInt(back_buffer.width - 2 * pad_x),
    ) / @as(
        f32,
        @floatFromInt(sound_output.secondary_buffer_size),
    );

    for (markers, 0..) |marker, i| {
        std.debug.assert(marker.output_write_cursor < sound_output.secondary_buffer_size);
        std.debug.assert(marker.output_write_cursor < sound_output.secondary_buffer_size);
        std.debug.assert(marker.output_location < sound_output.secondary_buffer_size);
        std.debug.assert(marker.output_byte_count < sound_output.secondary_buffer_size);
        std.debug.assert(marker.flip_play_cursor < sound_output.secondary_buffer_size);
        std.debug.assert(marker.flip_play_cursor < sound_output.secondary_buffer_size);

        const play_color = 0xFFFFFFFF;
        const write_color = 0xFFFF0000;
        const expected_flip_color = 0xFFFFFF00;
        const play_window_color = 0xFFFF00FF;
        var top: i32 = pad_y;
        var bottom: i32 = pad_y + line_height;

        if (current_marker_index == i) {
            top += line_height + pad_y;
            bottom += line_height + pad_y;

            const first_top = top;
            win32_draw_sound_buffer_marker(back_buffer, sound_output, coefficient, pad_x, top, bottom, marker.output_play_cursor, play_color);
            win32_draw_sound_buffer_marker(back_buffer, sound_output, coefficient, pad_x, top, bottom, marker.output_write_cursor, write_color);

            top += line_height + pad_y;
            bottom += line_height + pad_y;

            win32_draw_sound_buffer_marker(back_buffer, sound_output, coefficient, pad_x, top, bottom, marker.output_location, play_color);
            win32_draw_sound_buffer_marker(back_buffer, sound_output, coefficient, pad_x, top, bottom, marker.output_location + marker.output_byte_count, write_color);

            top += line_height + pad_y;
            bottom += line_height + pad_y;

            win32_draw_sound_buffer_marker(back_buffer, sound_output, coefficient, pad_x, first_top, bottom, marker.expected_flip_play_cursor, expected_flip_color);
        }

        win32_draw_sound_buffer_marker(back_buffer, sound_output, coefficient, pad_x, top, bottom, marker.flip_play_cursor, play_color);
        win32_draw_sound_buffer_marker(back_buffer, sound_output, coefficient, pad_x, top, bottom, marker.flip_play_cursor + 480 * sound_output.bytes_per_sample, play_window_color);
        win32_draw_sound_buffer_marker(back_buffer, sound_output, coefficient, pad_x, top, bottom, marker.flip_write_cursor, write_color);
    }
}

inline fn win32_draw_sound_buffer_marker(
    back_buffer: *BackBuffer,
    sound_output: *Win32SoundOutput,
    coefficient: f32,
    pad_x: i32,
    top: i32,
    bottom: i32,
    value: DWORD,
    color: u32,
) void {
    _ = sound_output;
    const x: i32 = pad_x +
        @as(i32, @intFromFloat(coefficient *
        @as(f32, @floatFromInt(value))));

    win32_debug_draw_vertical(
        back_buffer,
        x,
        top,
        bottom,
        color,
    );
}

inline fn win32_debug_draw_vertical(
    back_buffer: *BackBuffer,
    x: i32,
    top: i32,
    bottom: i32,
    color: u32,
) void {
    const t = if (top <= 0) 0 else top;

    const b = if (bottom >= back_buffer.height)
        back_buffer.height
    else
        bottom;

    if (x >= 0 and x < back_buffer.width) {
        const offset = @as(usize, @intCast(x *
            back_buffer.bytes_per_pixel +
            t *
            back_buffer.pitch));

        var pixel: [*]u8 = @as(
            [*]u8,
            @alignCast(@ptrCast(back_buffer.memory)),
        ) + offset;

        for (@intCast(t)..@intCast(b)) |_| {
            @as([*]u32, @alignCast(@ptrCast(pixel)))[0] = color;
            pixel += @as(usize, @intCast(back_buffer.pitch));
        }
    }
}

fn concatenate(
    source_a: []const u8,
    source_b: []const u8,
    destination: [:0]u8,
) void {
    // TODO: Bounds check on destination
    var index: usize = 0;

    for (source_a, 0..) |char, i| {
        destination[i] = char;
        index += 1;
    }

    for (source_b, 0..) |char, i| {
        destination[index + i] = char;
    }
}

fn win32_get_input_file_location(
    state: *Win32State,
    input_stream: bool,
    slot_index: usize,
    destination: [:0]u8,
) void {
    var buf: [64]u8 = undefined;

    const temp = std.fmt.bufPrint(
        &buf,
        "loop_edit_{d}_{s}.zmi",
        .{ slot_index, if (input_stream) "input" else "state" },
    ) catch {
        @panic("Failed to get input file location!");
    };

    win32_build_exe_path_file_name(state, temp, destination);
}

fn win32_get_replay_buffer(state: *Win32State, index: u32) *Win32ReplayBuffer {
    std.debug.assert(index < state.replay_buffers.len);

    const result = &state.replay_buffers[index];

    return result;
}

inline fn copy_memory(
    destination: [*]u8,
    source: [*]u8,
    size: u64,
) void {
    @memcpy(
        destination[0..size],
        source[0..size],
    );
}

fn win32_begin_recording_input(
    state: *Win32State,
    input_recording_index: usize,
) void {
    const replay_buffer = win32_get_replay_buffer(
        state,
        @intCast(input_recording_index),
    );

    if (replay_buffer.memory_block) |block| {
        state.input_recording_index = input_recording_index;
        var file_name = [_:0]u8{0} ** WIN32_STATE_FILE_NAME_COUNT;
        win32_get_input_file_location(state, true, input_recording_index, @ptrCast(&file_name));

        state.recording_handle = win32.CreateFileA(
            &file_name,
            win32.FILE_GENERIC_WRITE,
            win32.FILE_SHARE_NONE,
            null,
            win32.CREATE_ALWAYS,
            win32.FILE_FLAGS_AND_ATTRIBUTES{},
            null,
        );

        copy_memory(@ptrCast(block), @ptrCast(state.game_memory_block), state.total_size);
    }
}

fn win32_end_recording_input(state: *Win32State) void {
    _ = win32.CloseHandle(state.recording_handle);
    state.input_recording_index = 0;
}

fn win32_begin_input_play_back(
    state: *Win32State,
    input_playing_index: usize,
) void {
    const replay_buffer = win32_get_replay_buffer(
        state,
        @intCast(input_playing_index),
    );

    if (replay_buffer.memory_block) |block| {
        state.input_playing_index = input_playing_index;
        var file_name = [_:0]u8{0} ** WIN32_STATE_FILE_NAME_COUNT;
        win32_get_input_file_location(state, true, input_playing_index, &file_name);

        state.playback_handle = win32.CreateFileA(
            &file_name,
            win32.FILE_GENERIC_READ,
            win32.FILE_SHARE_NONE,
            null,
            win32.OPEN_EXISTING,
            win32.FILE_FLAGS_AND_ATTRIBUTES{},
            null,
        );

        copy_memory(@ptrCast(state.game_memory_block), @ptrCast(block), state.total_size);
    }
}

fn win32_end_input_play_back(state: *Win32State) void {
    _ = win32.CloseHandle(state.playback_handle);
    state.input_playing_index = 0;
}

fn win32_record_input(
    state: *Win32State,
    game_input: *platform.GameInput,
) void {
    var bytes_written: DWORD = undefined;

    _ = win32.WriteFile(
        state.recording_handle,
        game_input,
        @sizeOf(platform.GameInput),
        &bytes_written,
        null,
    );
}

fn win32_play_back_input(
    state: *Win32State,
    game_input: *platform.GameInput,
) void {
    var bytes_read: DWORD = undefined;

    if (win32.ReadFile(
        state.playback_handle,
        game_input,
        @sizeOf(@TypeOf(game_input.*)),
        &bytes_read,
        null,
    ) != win32.FALSE) {
        if (bytes_read == 0) {
            // NOTE: We've hit the end of the stream, go back to the beginning
            const playing_index = state.input_playing_index;
            win32_end_input_play_back(state);
            win32_begin_input_play_back(state, playing_index);

            _ = win32.ReadFile(
                state.playback_handle,
                game_input,
                @sizeOf(@TypeOf(game_input.*)),
                &bytes_read,
                null,
            );
        }
    }
}

fn win32_get_exe_file_name(state: *Win32State) void {
    // NOTE: Never use MAX_PATH in code that is user-facing, because it
    // can be dangerous and lead to bad results.
    _ = win32.GetModuleFileNameA(
        null,
        &state.exe_file_name,
        @sizeOf(@TypeOf(state.exe_file_name)),
    );

    state.one_past_last_exe_file_name_slash = 0;

    for (state.exe_file_name, 0..) |char, index| {
        if (char == '\\') {
            state.one_past_last_exe_file_name_slash = index + 1;
        }
    }
}

fn win32_build_exe_path_file_name(
    state: *Win32State,
    file_name: []const u8,
    destination: [:0]u8,
) void {
    concatenate(
        state.exe_file_name[0..state.one_past_last_exe_file_name_slash],
        file_name,
        destination[0..win32.MAX_COUNTER_PATH :0],
    );
}

pub export fn wWinMain(
    instance: ?win32.HINSTANCE,
    _: ?win32.HINSTANCE,
    _: [*:0]u16,
    _: u32,
) callconv(WINAPI) c_int {
    var win32_state = std.mem.zeroInit(Win32State, .{});

    var perf_count_frequency_result: win32.LARGE_INTEGER = undefined;
    _ = win32.QueryPerformanceFrequency(&perf_count_frequency_result);
    global_perf_count_frequency = perf_count_frequency_result.QuadPart;

    win32_get_exe_file_name(&win32_state);

    var source_game_code_dll_path = [_:0]u8{0} ** WIN32_STATE_FILE_NAME_COUNT;
    win32_build_exe_path_file_name(
        &win32_state,
        "zigmade.dll",
        &source_game_code_dll_path,
    );

    var temp_game_code_dll_path = [_:0]u8{0} ** WIN32_STATE_FILE_NAME_COUNT;
    win32_build_exe_path_file_name(
        &win32_state,
        "zigmade_temp.dll",
        &temp_game_code_dll_path,
    );

    // NOTE: Set Windows scheduler granularity to 1ms
    // so that Sleep() can be more granular
    const desired_scheduler_ms = 1;
    const sleep_is_granular = (win32.timeBeginPeriod(desired_scheduler_ms) == win32.TIMERR_NOERROR);

    win32_load_x_input();

    var window_class = std.mem.zeroInit(win32.WNDCLASSW, .{});

    // TODO: would be nice to properly log when something goes wrong
    // with these error union return types
    win32_resize_dib_section(&global_back_buffer, 960, 540);

    window_class.style = win32.WNDCLASS_STYLES{ .HREDRAW = 1, .VREDRAW = 1 };
    window_class.lpfnWndProc = @ptrCast(&win32_main_window_callback);
    window_class.hInstance = instance;
    // window_class.hIcon = ;
    window_class.lpszClassName = win32.L("HandmadeHeroWindowClass");

    if (win32.RegisterClassW(&window_class) != 0) {
        var window_style = win32.WS_OVERLAPPEDWINDOW;
        window_style.VISIBLE = 1;

        if (win32.CreateWindowExW(
            win32.WINDOW_EX_STYLE{},
            //win32.WINDOW_EX_STYLE{ .TOPMOST = 1, .LAYERED = 1 },
            window_class.lpszClassName,
            win32.L("Handmade Hero"),
            window_style,
            1280,
            25,
            1280,
            720,
            //win32.CW_USEDEFAULT,
            //win32.CW_USEDEFAULT,
            //win32.CW_USEDEFAULT,
            //win32.CW_USEDEFAULT,
            null,
            null,
            instance,
            null,
        )) |window| {
            var sound_output = std.mem.zeroInit(Win32SoundOutput, .{});

            // TODO: How do we reliably query this on Windows?
            var monitor_refresh_hertz: i32 = 60;
            var win32_refresh_rate: i32 = undefined;

            if (win32.GetDC(window)) |refresh_dc| {
                defer _ = win32.ReleaseDC(window, refresh_dc);

                win32_refresh_rate = win32.GetDeviceCaps(refresh_dc, win32.VREFRESH);
            }

            if (win32_refresh_rate > 1) {
                monitor_refresh_hertz = win32_refresh_rate;
            }

            const game_update_hertz: f32 =
                @as(f32, @floatFromInt(monitor_refresh_hertz)) / 2.0;
            const target_seconds_per_frame: f32 = 1.0 / game_update_hertz;

            // TODO: Make this like sixty seconds?
            sound_output.samples_per_second = 48_000;
            sound_output.bytes_per_sample = @sizeOf(i16) * 2;
            sound_output.secondary_buffer_size =
                @as(DWORD, @intCast(sound_output.samples_per_second *
                sound_output.bytes_per_sample));

            // TODO: Actually compute the variance and see
            // what the lowest reasonable value is
            // NOTE: Because of a higher refresh rate/game_update_hertz,
            // more safety bytes are necessary to make sure we are always
            // writing enough sound output to the buffer
            const safety_bytes =
                (@as(f32, @floatFromInt(sound_output.samples_per_second)) *
                @as(f32, @floatFromInt(sound_output.bytes_per_sample)) /
                game_update_hertz) * 2.0;

            sound_output.safety_bytes = @as(DWORD, @intFromFloat(safety_bytes));

            win32_init_direct_sound(
                window,
                sound_output.samples_per_second,
                sound_output.secondary_buffer_size,
            );

            win32_clear_buffer(&sound_output);

            _ = global_secondary_buffer.vtable.Play(
                global_secondary_buffer,
                0,
                0,
                win32.DSBPLAY_LOOPING,
            );

            global_running = true;

            // TODO: pool with bitmap VirtualAlloc
            const samples = win32.VirtualAlloc(
                null,
                sound_output.secondary_buffer_size,
                win32.VIRTUAL_ALLOCATION_TYPE{ .RESERVE = 1, .COMMIT = 1 },
                win32.PAGE_READWRITE,
            );

            const base_address: ?*anyopaque =
                if (INTERNAL)
                @ptrFromInt(platform.Terabytes(2))
            else
                null;

            var game_memory = platform.GameMemory{
                .permanent_storage_size = platform.Megabytes(64),
                .transient_storage_size = platform.Gigabytes(1),
                .debug_platform_read_entire_file = debug_platform_read_entire_file,
                .debug_platform_write_entire_file = debug_platform_write_entire_file,
                .debug_platform_free_file_memory = debug_platform_free_file_memory,
            };

            // TODO: Handle various memory footprints using system metrics
            // TODO: Use LARGE_PAGES and call adjust token privileges when not on WinXP?
            win32_state.total_size = game_memory.permanent_storage_size +
                game_memory.transient_storage_size;
            win32_state.game_memory_block = @as([*]u8, @ptrCast(win32.VirtualAlloc(
                base_address,
                @intCast(win32_state.total_size),
                win32.VIRTUAL_ALLOCATION_TYPE{ .RESERVE = 1, .COMMIT = 1 },
                win32.PAGE_READWRITE,
            )));

            game_memory.permanent_storage = @ptrCast(win32_state.game_memory_block);
            game_memory.transient_storage = game_memory.permanent_storage +
                game_memory.permanent_storage_size;

            for (&win32_state.replay_buffers, 0..) |*buffer, index| {
                // TODO: Recording system still seems to take a brief moment
                // on record start--find out what Windows is doing and if
                // we can speed up/defer some of that processing.
                win32_get_input_file_location(&win32_state, false, index, &buffer.file_name);

                buffer.file_handle = win32.CreateFileA(
                    &buffer.file_name,
                    WIN32_GENERIC_READ_WRITE,
                    win32.FILE_SHARE_NONE,
                    null,
                    win32.CREATE_ALWAYS,
                    win32.FILE_FLAGS_AND_ATTRIBUTES{},
                    null,
                );

                const max_size = win32.LARGE_INTEGER{
                    .QuadPart = @intCast(win32_state.total_size),
                };

                buffer.memory_map = win32.CreateFileMappingA(
                    buffer.file_handle,
                    null,
                    win32.PAGE_READWRITE,
                    @intCast(max_size.u.HighPart),
                    max_size.u.LowPart,
                    null,
                );

                buffer.memory_block = win32.MapViewOfFile(
                    buffer.memory_map,
                    win32.FILE_MAP_ALL_ACCESS,
                    0,
                    0,
                    win32_state.total_size,
                );

                if (buffer.memory_block != undefined) {
                    // Memory acquired
                } else {
                    // TODO: Diagnostic
                }
            }

            if (samples != undefined and
                game_memory.permanent_storage != undefined and
                game_memory.transient_storage != undefined)
            {
                var inputs: [2]platform.GameInput = undefined;
                var new_input = &inputs[0];
                var old_input = &inputs[1];

                var last_counter = win32_get_wall_clock();
                var flip_wall_clock = win32_get_wall_clock();
                var debug_time_marker_index: usize = 0;

                var debug_time_markers: [15]Win32DebugTimeMarker =
                    std.mem.zeroes([15]Win32DebugTimeMarker);

                var audio_latency_bytes: DWORD = 0;
                var audio_latency_seconds: f32 = 0.0;
                var sound_is_valid = false;

                var game = win32_load_game_code(
                    &source_game_code_dll_path,
                    &temp_game_code_dll_path,
                );

                var last_cycle_count = rdtsc();

                while (global_running) {
                    new_input.dt_for_frame = target_seconds_per_frame;

                    var new_dll_write_time = win32_get_last_write_time(&source_game_code_dll_path);

                    if (win32.CompareFileTime(&new_dll_write_time, &game.dll_last_write_time) != 0) {
                        win32_unload_game_code(&game);
                        game = win32_load_game_code(
                            &source_game_code_dll_path,
                            &temp_game_code_dll_path,
                        );
                    }

                    // TODO: Zeroing "macro" with comptime
                    // TODO: We can't zero everything because the up/down state will be wrong
                    // NOTE: Index 0 belongs to keyboard
                    const old_keyboard_controller: *platform.GameControllerInput =
                        try platform.get_controller(old_input, 0);
                    var new_keyboard_controller: *platform.GameControllerInput =
                        try platform.get_controller(new_input, 0);
                    new_keyboard_controller.* =
                        std.mem.zeroInit(platform.GameControllerInput, .{});
                    new_keyboard_controller.is_connected = true;

                    for (0..new_keyboard_controller.buttons.array.len) |button_index| {
                        new_keyboard_controller.buttons.array[button_index].ended_down =
                            old_keyboard_controller.buttons.array[button_index].ended_down;
                    }

                    win32_process_pending_messages(&win32_state, new_keyboard_controller);

                    if (!global_pause) {
                        var mouse_pos: win32.POINT = undefined;
                        _ = win32.GetCursorPos(&mouse_pos);
                        _ = win32.ScreenToClient(window, &mouse_pos);
                        new_input.mouse_x = mouse_pos.x;
                        new_input.mouse_y = mouse_pos.y;
                        new_input.mouse_z = 0; // TODO: Support mousewheel?
                        win32_process_keyboard_message(
                            &new_input.mouse_buttons[0],
                            @as(u16, @bitCast(win32.GetKeyState(@intFromEnum(win32.VK_LBUTTON)))) &
                                (1 << 15) > 0,
                        );
                        win32_process_keyboard_message(
                            &new_input.mouse_buttons[1],
                            @as(u16, @bitCast(win32.GetKeyState(@intFromEnum(win32.VK_MBUTTON)))) &
                                (1 << 15) > 0,
                        );
                        win32_process_keyboard_message(
                            &new_input.mouse_buttons[2],
                            @as(u16, @bitCast(win32.GetKeyState(@intFromEnum(win32.VK_RBUTTON)))) &
                                (1 << 15) > 0,
                        );
                        win32_process_keyboard_message(
                            &new_input.mouse_buttons[3],
                            @as(u16, @bitCast(win32.GetKeyState(@intFromEnum(win32.VK_XBUTTON1)))) &
                                (1 << 15) > 0,
                        );
                        win32_process_keyboard_message(
                            &new_input.mouse_buttons[4],
                            @as(u16, @bitCast(win32.GetKeyState(@intFromEnum(win32.VK_XBUTTON2)))) &
                                (1 << 15) > 0,
                        );

                        // TODO: Need to not poll disconnected controllers
                        // to avoid xinput frame ratre hit on older libraries
                        // TODO: should we poll this more frequently?
                        var max_controller_count = win32.XUSER_MAX_COUNT;
                        if (max_controller_count > new_input.controllers.len - 1) {
                            max_controller_count = new_input.controllers.len - 1;
                        }

                        // NOTE: Indices 1..4 belong to gamepad
                        for (0..max_controller_count) |controller_index| {
                            const our_index = controller_index + 1;
                            var old_controller: *platform.GameControllerInput =
                                try platform.get_controller(old_input, our_index);
                            var new_controller: *platform.GameControllerInput =
                                try platform.get_controller(new_input, our_index);
                            var controller_state: win32.XINPUT_STATE = undefined;

                            if (XInputGetState.call(
                                @intCast(controller_index),
                                &controller_state,
                            ) == @intFromEnum(win32.ERROR_SUCCESS)) {
                                new_controller.is_connected = true;
                                new_controller.is_analog = old_controller.is_analog;

                                // NOTE: Controller is plugged in
                                // TODO: See if controller_state.dwPacketNumber increments too quickly
                                const pad: *win32.XINPUT_GAMEPAD = &controller_state.Gamepad;

                                // TODO: This is a square deadzone, check XInput to
                                // verify that the deadzone is round and show how to do
                                // round deadzone processing
                                new_controller.stick_average_x = win32_process_x_input_stick_value(
                                    pad.sThumbLX,
                                    win32.XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE,
                                );
                                new_controller.stick_average_y = win32_process_x_input_stick_value(
                                    pad.sThumbLY,
                                    win32.XINPUT_GAMEPAD_LEFT_THUMB_DEADZONE,
                                );

                                if ((new_controller.stick_average_x != 0.0) or
                                    (new_controller.stick_average_y != 0.0))
                                {
                                    new_controller.is_analog = true;
                                }

                                if ((pad.wButtons & win32.XINPUT_GAMEPAD_DPAD_UP) > 0) {
                                    new_controller.stick_average_y = 1.0;
                                    new_controller.is_analog = false;
                                }

                                if ((pad.wButtons & win32.XINPUT_GAMEPAD_DPAD_DOWN) > 0) {
                                    new_controller.stick_average_y = -1.0;
                                    new_controller.is_analog = false;
                                }

                                if ((pad.wButtons & win32.XINPUT_GAMEPAD_DPAD_LEFT) > 0) {
                                    new_controller.stick_average_x = -1.0;
                                    new_controller.is_analog = false;
                                }

                                if ((pad.wButtons & win32.XINPUT_GAMEPAD_DPAD_RIGHT) > 0) {
                                    new_controller.stick_average_x = 1.0;
                                    new_controller.is_analog = false;
                                }

                                const threshold: f32 = 0.5;
                                win32_process_x_input_digital_button(
                                    if (new_controller.stick_average_x < -threshold)
                                        1
                                    else
                                        0,
                                    &old_controller.buttons.map.move_left,
                                    &new_controller.buttons.map.move_left,
                                    1,
                                );
                                win32_process_x_input_digital_button(
                                    if (new_controller.stick_average_x > threshold)
                                        1
                                    else
                                        0,
                                    &old_controller.buttons.map.move_right,
                                    &new_controller.buttons.map.move_right,
                                    1,
                                );
                                win32_process_x_input_digital_button(
                                    if (new_controller.stick_average_y < -threshold)
                                        1
                                    else
                                        0,
                                    &old_controller.buttons.map.move_down,
                                    &new_controller.buttons.map.move_down,
                                    1,
                                );
                                win32_process_x_input_digital_button(
                                    if (new_controller.stick_average_y > threshold)
                                        1
                                    else
                                        0,
                                    &old_controller.buttons.map.move_up,
                                    &new_controller.buttons.map.move_up,
                                    1,
                                );

                                win32_process_x_input_digital_button(
                                    pad.wButtons,
                                    &old_controller.buttons.map.action_down,
                                    &new_controller.buttons.map.action_down,
                                    win32.XINPUT_GAMEPAD_A,
                                );
                                win32_process_x_input_digital_button(
                                    pad.wButtons,
                                    &old_controller.buttons.map.action_right,
                                    &new_controller.buttons.map.action_right,
                                    win32.XINPUT_GAMEPAD_B,
                                );
                                win32_process_x_input_digital_button(
                                    pad.wButtons,
                                    &old_controller.buttons.map.action_left,
                                    &new_controller.buttons.map.action_left,
                                    win32.XINPUT_GAMEPAD_X,
                                );
                                win32_process_x_input_digital_button(
                                    pad.wButtons,
                                    &old_controller.buttons.map.action_up,
                                    &new_controller.buttons.map.action_up,
                                    win32.XINPUT_GAMEPAD_Y,
                                );
                                win32_process_x_input_digital_button(
                                    pad.wButtons,
                                    &old_controller.buttons.map.left_shoulder,
                                    &new_controller.buttons.map.left_shoulder,
                                    win32.XINPUT_GAMEPAD_LEFT_SHOULDER,
                                );
                                win32_process_x_input_digital_button(
                                    pad.wButtons,
                                    &old_controller.buttons.map.right_shoulder,
                                    &new_controller.buttons.map.right_shoulder,
                                    win32.XINPUT_GAMEPAD_RIGHT_SHOULDER,
                                );
                                win32_process_x_input_digital_button(
                                    pad.wButtons,
                                    &old_controller.buttons.map.start,
                                    &new_controller.buttons.map.start,
                                    win32.XINPUT_GAMEPAD_START,
                                );
                                win32_process_x_input_digital_button(
                                    pad.wButtons,
                                    &old_controller.buttons.map.back,
                                    &new_controller.buttons.map.back,
                                    win32.XINPUT_GAMEPAD_BACK,
                                );

                                // var start = pad.wButtons & win32.XINPUT_GAMEPAD_START;
                                // var back = pad.wButtons & win32.XINPUT_GAMEPAD_BACK;
                            } else {
                                // NOTE: Controller is unavailable
                                new_controller.is_connected = false;
                            }
                        }

                        // var vibration: win32.XINPUT_VIBRATION = undefined;
                        // vibration.wLeftMotorSpeed = 60000;
                        // vibration.wRightMotorSpeed = 60000;
                        // _ = XInputSetState.call(0, &vibration);

                        var thread = std.mem.zeroInit(platform.ThreadContext, .{});

                        var offscreen_buffer: platform.GameOffscreenBuffer =
                            std.mem.zeroInit(platform.GameOffscreenBuffer, .{});
                        offscreen_buffer.memory = @alignCast(@ptrCast(global_back_buffer.memory));
                        offscreen_buffer.width = global_back_buffer.width;
                        offscreen_buffer.height = global_back_buffer.height;
                        offscreen_buffer.pitch = global_back_buffer.pitch;
                        offscreen_buffer.bytes_per_pixel = global_back_buffer.bytes_per_pixel;

                        if (win32_state.input_recording_index != 0) {
                            win32_record_input(&win32_state, new_input);
                        }

                        if (win32_state.input_playing_index != 0) {
                            win32_play_back_input(&win32_state, new_input);
                        }

                        if (game.update_and_render) |update_and_render| {
                            update_and_render(
                                &thread,
                                &game_memory,
                                new_input,
                                &offscreen_buffer,
                            );
                        }

                        const audio_wall_clock = win32_get_wall_clock();
                        const from_begin_to_audio_seconds = win32_get_seconds_elapsed(
                            flip_wall_clock,
                            audio_wall_clock,
                        );

                        var play_cursor: DWORD = 0;
                        var write_cursor: DWORD = 0;

                        if (win32.SUCCEEDED(global_secondary_buffer.vtable.GetCurrentPosition(
                            global_secondary_buffer,
                            &play_cursor,
                            &write_cursor,
                        ))) {
                            // NOTE: How sound output computation works
                            //
                            // Define a safety value that is the number of samples that the game
                            // loop may vary by (up to 2ms)
                            //
                            // When we wake up to write audio, look and see what the play cursor
                            // position is and forecast ahead where the play cursor ought to be
                            // on the next frame boundary.
                            //
                            // Then look to see if the write cursor is before this boundary by
                            // the safety value. If it is, fill target to frame boundary + 1 frame,
                            // and then one frame further, providing perfect audio sync for
                            // cards with low enough latency
                            //
                            // If write cursor is after the safety value, then assume
                            // audio cannot be perfectly synced, so write one frame's worth of
                            // audio plus safety value's amount of guard samples

                            if (!sound_is_valid) {
                                sound_output.running_sample_index =
                                    write_cursor /
                                    sound_output.bytes_per_sample;

                                sound_is_valid = true;
                            }

                            const byte_to_lock =
                                (sound_output.running_sample_index *
                                sound_output.bytes_per_sample) %
                                sound_output.secondary_buffer_size;

                            const expected_sound_bytes_per_frame = @as(
                                DWORD,
                                @intFromFloat(@as(
                                    f32,
                                    @floatFromInt(sound_output.samples_per_second *
                                        sound_output.bytes_per_sample),
                                ) /
                                    game_update_hertz),
                            );
                            var seconds_left_until_flip: f32 =
                                target_seconds_per_frame -
                                from_begin_to_audio_seconds;

                            // NOTE: Without this check, a crash occurs when unpausing
                            // the game due to trying to convert seconds_left_until_flip
                            // to a DWORD when its value is negative
                            if (seconds_left_until_flip < 0) {
                                seconds_left_until_flip = 0.0;
                            }

                            const expected_bytes_until_flip =
                                @as(DWORD, @intFromFloat((seconds_left_until_flip /
                                target_seconds_per_frame) *
                                @as(f32, @floatFromInt(expected_sound_bytes_per_frame))));

                            const expected_frame_boundary_byte = play_cursor + expected_bytes_until_flip;
                            var safe_write_cursor = write_cursor;

                            if (safe_write_cursor < play_cursor) {
                                safe_write_cursor += sound_output.secondary_buffer_size;
                            }

                            std.debug.assert(safe_write_cursor >= play_cursor);

                            safe_write_cursor += sound_output.safety_bytes;
                            const audio_card_is_low_latency = safe_write_cursor < expected_frame_boundary_byte;
                            var target_cursor: DWORD = 0;

                            if (audio_card_is_low_latency) {
                                target_cursor = expected_frame_boundary_byte + expected_sound_bytes_per_frame;
                            } else {
                                target_cursor = write_cursor +
                                    expected_sound_bytes_per_frame +
                                    sound_output.safety_bytes;
                            }

                            target_cursor = target_cursor % sound_output.secondary_buffer_size;

                            var bytes_to_write: DWORD = 0;

                            if (byte_to_lock > target_cursor) {
                                bytes_to_write = sound_output.secondary_buffer_size - byte_to_lock;
                                bytes_to_write += target_cursor;
                            } else {
                                bytes_to_write = target_cursor - byte_to_lock;
                            }

                            var sound_buffer = std.mem.zeroInit(platform.GameSoundBuffer, .{});
                            sound_buffer.samples_per_second = sound_output.samples_per_second;
                            sound_buffer.sample_count = bytes_to_write / sound_output.bytes_per_sample;
                            sound_buffer.samples = @alignCast(@ptrCast(samples.?));

                            if (game.get_sound_samples) |get_sound_samples| {
                                get_sound_samples(
                                    &thread,
                                    &game_memory,
                                    &sound_buffer,
                                );
                            }

                            if (INTERNAL) {
                                var marker = &debug_time_markers[debug_time_marker_index];
                                marker.output_play_cursor = play_cursor;
                                marker.output_write_cursor = write_cursor;
                                marker.output_location = byte_to_lock;
                                marker.output_byte_count = bytes_to_write;
                                marker.expected_flip_play_cursor = expected_frame_boundary_byte;

                                var unwrapped_write_cursor = write_cursor;

                                if (unwrapped_write_cursor < play_cursor) {
                                    unwrapped_write_cursor += sound_output.secondary_buffer_size;
                                }

                                audio_latency_bytes = unwrapped_write_cursor - play_cursor;

                                audio_latency_seconds =
                                    (@as(f32, @floatFromInt(audio_latency_bytes)) /
                                    @as(f32, @floatFromInt(sound_output.bytes_per_sample))) /
                                    @as(f32, @floatFromInt(sound_output.samples_per_second));

                                if (DEBUG_SYNC_DISPLAY) {
                                    std.debug.print(
                                        "BTL: {}, TC {}, BTW: {} - PC: {}, WC: {}, DELTA: {}, ({d:1.6})s\n",
                                        .{
                                            byte_to_lock,
                                            target_cursor,
                                            bytes_to_write,
                                            play_cursor,
                                            write_cursor,
                                            audio_latency_bytes,
                                            audio_latency_seconds,
                                        },
                                    );
                                }
                            }

                            win32_fill_sound_buffer(
                                &sound_output,
                                byte_to_lock,
                                bytes_to_write,
                                &sound_buffer,
                            );
                        } else {
                            sound_is_valid = false;
                        }

                        const work_counter = win32_get_wall_clock();
                        const work_seconds_elapsed = win32_get_seconds_elapsed(
                            last_counter,
                            work_counter,
                        );

                        var seconds_elapsed_for_frame = work_seconds_elapsed;

                        // TODO: Not tested yet, probably buggy
                        if (seconds_elapsed_for_frame < target_seconds_per_frame) {
                            if (sleep_is_granular) {
                                const sleep_ms = @as(DWORD, @intFromFloat(1000.0 *
                                    (target_seconds_per_frame - seconds_elapsed_for_frame)));

                                if (sleep_ms > 0) {
                                    win32.Sleep(sleep_ms);
                                }
                            }

                            const test_seconds_elapsed_for_frame = win32_get_seconds_elapsed(
                                last_counter,
                                win32_get_wall_clock(),
                            );

                            if (test_seconds_elapsed_for_frame < target_seconds_per_frame) {
                                // TODO: log missed sleep here
                            }

                            while (seconds_elapsed_for_frame < target_seconds_per_frame) {
                                seconds_elapsed_for_frame = win32_get_seconds_elapsed(
                                    last_counter,
                                    win32_get_wall_clock(),
                                );
                            }
                        } else {
                            // TODO: Missed framerate
                            // TODO: logging
                        }

                        const end_counter = win32_get_wall_clock();
                        const ms_per_frame = 1000.0 * win32_get_seconds_elapsed(
                            last_counter,
                            end_counter,
                        );
                        last_counter = end_counter;

                        const dimension = win32_get_window_dimension(window);

                        if (DEBUG_SYNC_DISPLAY) {
                            win32_debug_sync_display(
                                &global_back_buffer,
                                &debug_time_markers,
                                @intCast(if (debug_time_marker_index == 0)
                                    debug_time_markers.len - 1
                                else
                                    debug_time_marker_index - 1),
                                &sound_output,
                                target_seconds_per_frame,
                            );
                        }

                        if (win32.GetDC(window)) |device_context| {
                            defer _ = win32.ReleaseDC(window, device_context);

                            win32_display_buffer_in_window(
                                &global_back_buffer,
                                device_context,
                                dimension.width,
                                dimension.height,
                            );
                        }

                        flip_wall_clock = win32_get_wall_clock();

                        // NOTE: This is debug code
                        if (INTERNAL) {
                            if (win32.SUCCEEDED(global_secondary_buffer.vtable.GetCurrentPosition(
                                global_secondary_buffer,
                                &play_cursor,
                                &write_cursor,
                            ))) {
                                std.debug.assert(debug_time_marker_index < debug_time_markers.len);
                                var marker = &debug_time_markers[debug_time_marker_index];
                                marker.flip_play_cursor = play_cursor;
                                marker.flip_write_cursor = write_cursor;
                            }
                        }

                        const temp = new_input;
                        new_input = old_input;
                        old_input = temp;
                        // TODO: should we clear these here?

                        if (DEBUG_WALL_CLOCK) {
                            const end_cycle_count = rdtsc();
                            const cycles_elapsed = end_cycle_count - last_cycle_count;
                            last_cycle_count = end_cycle_count;

                            const fps: f32 = 0.0;
                            const mega_cycles_per_frame =
                                @as(f64, @floatFromInt(cycles_elapsed)) /
                                @as(f64, @floatFromInt(1000 * 1000));

                            // Trying to print floats with wsprintf does not appear to cause a problem
                            // Including sprintf perhaps not worth it since we can only see messages from
                            // std.debug.print() anyways
                            // Leaving this code here for posterity
                            var fps_buffer = [_]u8{0} ** 255;
                            var args = [_]f64{ ms_per_frame, fps, mega_cycles_per_frame };
                            _ = win32.wvsprintfA(
                                @ptrCast(&fps_buffer),
                                ",%fms/f, %ff/s, %fmc/f\n",
                                @ptrCast(&args),
                            );
                            win32.OutputDebugStringA(@ptrCast(&fps_buffer));

                            std.debug.print("{d:6.2}ms/f, {d:6.2}f/s, {d:6.2}mc/f\n", .{
                                ms_per_frame,
                                fps,
                                mega_cycles_per_frame,
                            });
                        }

                        if (INTERNAL) {
                            debug_time_marker_index += 1;
                            if (debug_time_marker_index == debug_time_markers.len) {
                                debug_time_marker_index = 0;
                            }
                        }
                    }
                } else {
                    // TODO: logging
                }
            } else {
                // TODO: logging
            }
        }
    } else {
        // TODO: logging
    }

    return (0);
}
