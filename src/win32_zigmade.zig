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
const zigmade = @import("zigmade/zigmade.zig");

const WINAPI = std.os.windows.WINAPI;

const win32 = struct {
    usingnamespace @import("win32").zig;
    usingnamespace @import("win32").foundation;
    usingnamespace @import("win32").system;
    usingnamespace @import("win32").system.com;
    usingnamespace @import("win32").system.memory;
    usingnamespace @import("win32").system.performance;
    usingnamespace @import("win32").system.library_loader;
    usingnamespace @import("win32").graphics.gdi;
    usingnamespace @import("win32").ui.input.keyboard_and_mouse;
    usingnamespace @import("win32").ui.input.xbox_controller;
    usingnamespace @import("win32").ui.windows_and_messaging;
    usingnamespace @import("win32").media.audio;
    usingnamespace @import("win32").media.audio.direct_sound;
    usingnamespace @import("win32").system.diagnostics.debug;
};

const BackBuffer = struct {
    info: win32.BITMAPINFO = undefined,
    memory: ?*anyopaque = undefined,
    width: i32 = undefined,
    height: i32 = undefined,
    pitch: i32 = undefined,
    bytes_per_pixel: i32 = 4,
};

// TODO: Use globals for now
var global_running: bool = false;
var global_back_buffer: BackBuffer = undefined;
var global_secondary_buffer: *win32.IDirectSoundBuffer = undefined;

const WindowDimension = struct {
    width: i32 = undefined,
    height: i32 = undefined,
};

const XInputGetState = struct {
    var call: *const fn (
        user_index: u32,
        state: ?*win32.XINPUT_STATE,
    ) callconv(WINAPI) isize = undefined;

    fn stub(_: u32, _: ?*win32.XINPUT_STATE) callconv(WINAPI) isize {
        return (@intFromEnum(win32.ERROR_DEVICE_NOT_CONNECTED));
    }
};

const XInputSetState = struct {
    var call: *const fn (
        user_index: u32,
        vibration: ?*win32.XINPUT_VIBRATION,
    ) callconv(WINAPI) isize = undefined;

    fn stub(_: u32, _: ?*win32.XINPUT_VIBRATION) callconv(WINAPI) isize {
        return (@intFromEnum(win32.ERROR_DEVICE_NOT_CONNECTED));
    }
};

var direct_sound_create: *const fn (
    guid_device: ?*const win32.Guid,
    pp_ds: ?*?*win32.IDirectSound,
    unknown_outer: ?*win32.IUnknown,
) callconv(WINAPI) win32.HRESULT = undefined;

fn win32_load_x_input() !void {
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

fn win32_init_direct_sound(
    window: win32.HWND,
    samples_per_second: i32,
    buffer_size: i32,
) !void {
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
                    buffer_description.dwSize = @sizeOf(win32.DSBUFFERDESC);
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
                buffer_description.dwFlags = 0;
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

fn win32_get_window_dimension(window: win32.HWND) !WindowDimension {
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
) !void {
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

    var bitmap_memory_size = buffer.bytes_per_pixel * (buffer.width * buffer.height);
    buffer.memory = win32.VirtualAlloc(
        null,
        @intCast(bitmap_memory_size),
        win32.VIRTUAL_ALLOCATION_TYPE{ .RESERVE = 1, .COMMIT = 1 },
        win32.PAGE_READWRITE,
    );

    buffer.pitch = width * buffer.bytes_per_pixel;

    // TODO: clear bitmap to black
}

fn win32_display_buffer_in_window(
    buffer: *BackBuffer,
    device_context: ?win32.HDC,
    window_width: i32,
    window_height: i32,
) !void {
    // TODO: Correct aspect ratio
    _ = win32.StretchDIBits(
        device_context,
        0,
        0,
        window_width,
        window_height,
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
            win32.OutputDebugStringA("WM_ACTIVATEAPP\n");
        },
        win32.WM_DESTROY => {
            // TODO: Handle as an error--recreate window?
            global_running = false;
        },
        win32.WM_SYSKEYDOWN, win32.WM_SYSKEYUP, win32.WM_KEYDOWN, win32.WM_KEYUP => {
            var vk_code: win32.VIRTUAL_KEY = @enumFromInt(w_param);
            var was_down = (l_param & (1 << 30)) != 0;
            var is_down = (l_param & (1 << 31)) == 0;

            if (was_down != is_down) {
                switch (vk_code) {
                    win32.VK_W => win32.OutputDebugStringA("W\n"),
                    win32.VK_A => win32.OutputDebugStringA("A\n"),
                    win32.VK_S => win32.OutputDebugStringA("S\n"),
                    win32.VK_D => win32.OutputDebugStringA("D\n"),
                    win32.VK_Q => win32.OutputDebugStringA("Q\n"),
                    win32.VK_E => win32.OutputDebugStringA("E\n"),
                    win32.VK_UP => win32.OutputDebugStringA("VK_UP\n"),
                    win32.VK_DOWN => win32.OutputDebugStringA("VK_DOWN\n"),
                    win32.VK_LEFT => win32.OutputDebugStringA("VK_LEFT\n"),
                    win32.VK_RIGHT => win32.OutputDebugStringA("VK_RIGHT\n"),
                    win32.VK_ESCAPE => {
                        win32.OutputDebugStringA("ESCAPE: ");
                        std.debug.print("ESCAPE: ", .{});

                        if (is_down) {
                            win32.OutputDebugStringA("is down");
                            std.debug.print("is down", .{});
                        }
                        if (was_down) {
                            win32.OutputDebugStringA("was down");
                            std.debug.print("was down", .{});
                        }

                        win32.OutputDebugStringA("\n");
                        std.debug.print("\n", .{});
                    },
                    win32.VK_SPACE => win32.OutputDebugStringA("VK_SPACE\n"),
                    else => {},
                }
            }

            var alt_key_was_down: bool = (l_param & (1 << 29)) != 0;
            if (vk_code == win32.VK_F4 and alt_key_was_down) {
                global_running = false;
            }
        },
        win32.WM_PAINT => {
            var paint = std.mem.zeroInit(win32.PAINTSTRUCT, .{});
            var device_context = win32.BeginPaint(window, &paint);
            var dimension = try win32_get_window_dimension(window);
            try win32_display_buffer_in_window(
                &global_back_buffer,
                device_context,
                dimension.width,
                dimension.height,
            );

            _ = win32.EndPaint(window, &paint);
        },
        else => {
            result = win32.DefWindowProcW(window, message, w_param, l_param);
        },
    }

    return (result);
}

const Win32SoundOutput = struct {
    samples_per_second: i32,
    tone_hertz: i32,
    tone_volume: i16,
    running_sample_index: u32,
    wave_period: i32,
    bytes_per_sample: i32,
    secondary_buffer_size: i32,
    t_sine: f32,
    latency_sample_count: i32,
};

fn win32_fill_sound_buffer(
    sound_output: *Win32SoundOutput,
    byte_to_lock: u32,
    bytes_to_write: u32,
) !void {
    var maybe_region_1: ?*anyopaque = undefined;
    var region_1_size: u32 = undefined;
    var maybe_region_2: ?*anyopaque = undefined;
    var region_2_size: u32 = undefined;

    if (win32.SUCCEEDED(global_secondary_buffer.vtable.Lock(
        global_secondary_buffer,
        byte_to_lock,
        bytes_to_write,
        &maybe_region_1,
        &region_1_size,
        &maybe_region_2,
        &region_2_size,
        0,
    ))) {
        if (maybe_region_1) |region_1| {
            var region_1_sample_count: u32 =
                region_1_size / @as(u32, @intCast(sound_output.bytes_per_sample));
            var sample_out = @as([*]i16, @alignCast(@ptrCast(region_1)));

            for (0..region_1_sample_count) |_| {
                var sine_value: f32 = std.math.sin(sound_output.t_sine);
                var sample_value: i16 = @as(
                    i16,
                    @intFromFloat(
                        sine_value * @as(
                            f32,
                            @floatFromInt(sound_output.tone_volume),
                        ),
                    ),
                );

                sample_out[0] = sample_value;
                sample_out += 1;
                sample_out[0] = sample_value;
                sample_out += 1;

                sound_output.t_sine +=
                    2.0 *
                    std.math.pi *
                    1.0 /
                    @as(f32, @floatFromInt(sound_output.wave_period));

                sound_output.running_sample_index += 1;
            }
        }

        if (maybe_region_2) |region_2| {
            var region_2_sample_count: u32 =
                region_2_size /
                @as(u32, @intCast(sound_output.bytes_per_sample));
            var sample_out = @as([*]i16, @alignCast(@ptrCast(region_2)));

            for (0..region_2_sample_count) |_| {
                var sine_value: f32 = std.math.sin(sound_output.t_sine);
                var sample_value: i16 = @as(
                    i16,
                    @intFromFloat(
                        sine_value * @as(
                            f32,
                            @floatFromInt(sound_output.tone_volume),
                        ),
                    ),
                );

                sample_out[0] = sample_value;
                sample_out += 1;
                sample_out[0] = sample_value;
                sample_out += 1;

                sound_output.t_sine +=
                    2.0 *
                    std.math.pi *
                    1.0 /
                    @as(f32, @floatFromInt(sound_output.wave_period));

                sound_output.running_sample_index += 1;
            }
        }

        _ = global_secondary_buffer.vtable.Unlock(
            global_secondary_buffer,
            maybe_region_1,
            region_1_size,
            maybe_region_2,
            region_2_size,
        );
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

pub export fn wWinMain(
    instance: ?win32.HINSTANCE,
    _: ?win32.HINSTANCE,
    _: [*:0]u16,
    _: u32,
) callconv(WINAPI) c_int {
    //var perf_count_frequency_result: win32.LARGE_INTEGER = undefined;
    //_ = win32.QueryPerformanceFrequency(&perf_count_frequency_result);
    //var perf_count_frequency = perf_count_frequency_result.QuadPart;

    try win32_load_x_input();

    var window_class = std.mem.zeroInit(win32.WNDCLASSW, .{});

    // TODO: would be nice to properly log when something goes wrong with these error union return types
    try win32_resize_dib_section(&global_back_buffer, 1280, 720);

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
            window_class.lpszClassName,
            win32.L("Handmade Hero"),
            window_style,
            win32.CW_USEDEFAULT,
            win32.CW_USEDEFAULT,
            win32.CW_USEDEFAULT,
            win32.CW_USEDEFAULT,
            null,
            null,
            instance,
            null,
        )) |window| {
            if (win32.GetDC(window)) |device_context| {
                var x_offset: i32 = 0;
                var y_offset: i32 = 0;

                var sound_output: Win32SoundOutput = undefined;
                sound_output.samples_per_second = 48_000;
                sound_output.tone_hertz = 256;
                sound_output.tone_volume = 3_000;
                sound_output.running_sample_index = 0;
                sound_output.wave_period = @divFloor(
                    sound_output.samples_per_second,
                    sound_output.tone_hertz,
                );
                sound_output.bytes_per_sample = @sizeOf(i16) * 2;
                sound_output.secondary_buffer_size =
                    sound_output.samples_per_second *
                    sound_output.bytes_per_sample;
                sound_output.latency_sample_count = @divFloor(
                    sound_output.samples_per_second,
                    15,
                );

                try win32_init_direct_sound(
                    window,
                    sound_output.samples_per_second,
                    sound_output.secondary_buffer_size,
                );

                try win32_fill_sound_buffer(
                    &sound_output,
                    0,
                    @as(
                        u32,
                        @intCast(sound_output.latency_sample_count *
                            sound_output.bytes_per_sample),
                    ),
                );

                _ = global_secondary_buffer.vtable.Play(
                    global_secondary_buffer,
                    0,
                    0,
                    win32.DSBPLAY_LOOPING,
                );

                global_running = true;

                var last_counter: win32.LARGE_INTEGER = undefined;
                _ = win32.QueryPerformanceCounter(&last_counter);

                var last_cycle_count = rdtsc();

                while (global_running) {
                    var message: win32.MSG = undefined;

                    while (win32.PeekMessageW(
                        &message,
                        null,
                        0,
                        0,
                        win32.PM_REMOVE,
                    ) > 0) {
                        if (message.message == win32.WM_QUIT) {
                            global_running = false;
                        }

                        _ = win32.TranslateMessage(&message);
                        _ = win32.DispatchMessageW(&message);
                    }

                    // TODO: should we poll this more frequently?
                    for (0..win32.XUSER_MAX_COUNT) |controller_index| {
                        var controller_state: win32.XINPUT_STATE = undefined;

                        if (XInputGetState.call(
                            @as(u32, @intCast(controller_index)),
                            &controller_state,
                        ) == @intFromEnum(win32.ERROR_SUCCESS)) {
                            // NOTE: Controller is plugged in
                            // TODO: See if controller_state.dwPacketNumber increments too quickly
                            var pad: *win32.XINPUT_GAMEPAD = &controller_state.Gamepad;

                            // var up = pad.wButtons & win32.XINPUT_GAMEPAD_DPAD_UP;
                            // var down = pad.wButtons & win32.XINPUT_GAMEPAD_DPAD_DOWN;
                            // var left = pad.wButtons & win32.XINPUT_GAMEPAD_DPAD_LEFT;
                            // var right = pad.wButtons & win32.XINPUT_GAMEPAD_DPAD_RIGHT;
                            // var start = pad.wButtons & win32.XINPUT_GAMEPAD_START;
                            // var back = pad.wButtons & win32.XINPUT_GAMEPAD_BACK;
                            // var left_shoulder = pad.wButtons & win32.XINPUT_GAMEPAD_LEFT_SHOULDER;
                            // var right_shoulder = pad.wButtons & win32.XINPUT_GAMEPAD_RIGHT_SHOULDER;
                            // var a_button = pad.wButtons & win32.XINPUT_GAMEPAD_A;
                            // var b_button = pad.wButtons & win32.XINPUT_GAMEPAD_B;
                            // var x_button = pad.wButtons & win32.XINPUT_GAMEPAD_X;
                            // var y_button = pad.wButtons & win32.XINPUT_GAMEPAD_Y;

                            var stick_x = pad.sThumbLX;
                            var stick_y = pad.sThumbLY;

                            // NOTE: we will do deadzone handling later using
                            x_offset += @divFloor(stick_x, 4096);
                            y_offset += @divFloor(stick_y, 4096);

                            sound_output.tone_hertz = 512 + @as(
                                i32,
                                @intFromFloat(256.0 *
                                    (@as(f32, @floatFromInt(stick_y)) /
                                    30_000.0)),
                            );

                            sound_output.wave_period = @divFloor(
                                sound_output.samples_per_second,
                                sound_output.tone_hertz,
                            );
                        } else {
                            // NOTE: Controller is unavailable
                        }
                    }

                    // var vibration: win32.XINPUT_VIBRATION = undefined;
                    // vibration.wLeftMotorSpeed = 60000;
                    // vibration.wRightMotorSpeed = 60000;
                    // _ = XInputSetState.call(0, &vibration);

                    var buffer: zigmade.GameOffscreenBuffer = std.mem.zeroInit(zigmade.GameOffscreenBuffer, .{});
                    buffer.memory = global_back_buffer.memory;
                    buffer.width = global_back_buffer.width;
                    buffer.height = global_back_buffer.height;
                    buffer.pitch = global_back_buffer.pitch;

                    try zigmade.game_update_and_render(&buffer, x_offset, y_offset);

                    // NOTE: DirectSound output test
                    var play_cursor: u32 = undefined;
                    var write_cursor: u32 = undefined;

                    if (win32.SUCCEEDED(global_secondary_buffer.vtable.GetCurrentPosition(
                        global_secondary_buffer,
                        &play_cursor,
                        &write_cursor,
                    ))) {
                        var byte_to_lock: u32 =
                            (sound_output.running_sample_index *
                            @as(u32, @intCast(sound_output.bytes_per_sample))) %
                            @as(u32, @intCast(sound_output.secondary_buffer_size));

                        var target_cursor: u32 = (play_cursor +
                            (@as(u32, @intCast(sound_output.latency_sample_count))) *
                            @as(u32, @intCast(sound_output.bytes_per_sample))) %
                            @as(u32, @intCast(sound_output.secondary_buffer_size));

                        var bytes_to_write: u32 = undefined;

                        if (byte_to_lock > target_cursor) {
                            bytes_to_write = @as(
                                u32,
                                @intCast(sound_output.secondary_buffer_size),
                            ) - byte_to_lock;
                            bytes_to_write += target_cursor;
                        } else {
                            bytes_to_write = target_cursor - byte_to_lock;
                        }

                        try win32_fill_sound_buffer(
                            &sound_output,
                            byte_to_lock,
                            bytes_to_write,
                        );
                    }

                    var dimension = try win32_get_window_dimension(window);

                    try win32_display_buffer_in_window(
                        &global_back_buffer,
                        device_context,
                        dimension.width,
                        dimension.height,
                    );

                    var end_cycle_count = rdtsc();

                    var end_counter: win32.LARGE_INTEGER = undefined;
                    _ = win32.QueryPerformanceCounter(&end_counter);

                    //var cycles_elapsed = end_cycle_count - last_cycle_count;
                    //var counter_elapsed = end_counter.QuadPart -
                    //    last_counter.QuadPart;
                    //var ms_per_frame = @as(f32, @floatFromInt(1000 * counter_elapsed)) /
                    //    @as(f32, @floatFromInt(perf_count_frequency));
                    //var fps = @as(f32, @floatFromInt(perf_count_frequency)) /
                    //    @as(f32, (@floatFromInt(counter_elapsed)));
                    //var mega_cycles_per_frame = @as(f32, @floatFromInt(cycles_elapsed)) /
                    //    @as(f32, @floatFromInt(1000 * 1000));

                    // Trying to print floats with wsprintf does not appear to cause a problem
                    // Including sprintf perhaps not worth it since we can only see messages from
                    // std.debug.print() anyways
                    // Leaving this code here for posterity
                    // var buffer = [_]u8{0} ** 255;
                    // var args = [_]f32{ ms_per_frame, fps, mega_cycles_per_frame };
                    // _ = win32.wvsprintfA(
                    //     @as([*:0]u8, @ptrCast(&buffer)),
                    //     "%fms/f, %ff/s, %fmc/f\n",
                    //     @as(*i8, @ptrCast(&args)),
                    // );
                    // win32.OutputDebugStringA(@as(
                    //     [*:0]u8,
                    //     @ptrCast(&buffer),
                    // ));

                    //std.debug.print("{d:6.2}ms/f, {d:6.2}f/s, {d:6.2}mc/f\n", .{
                    //    ms_per_frame,
                    //    fps,
                    //    mega_cycles_per_frame,
                    //});

                    last_counter = end_counter;
                    last_cycle_count = end_cycle_count;
                }
            } else {
                // TODO: logging
            }
        } else {
            // TODO: logging
        }
    } else {
        // TODO: logging
    }

    return (0);
}
