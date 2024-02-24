const std = @import("std");

const WINAPI = std.os.windows.WINAPI;

const win32 = struct {
    usingnamespace @import("win32").zig;
    usingnamespace @import("win32").foundation;
    usingnamespace @import("win32").system;
    usingnamespace @import("win32").system.com;
    usingnamespace @import("win32").system.memory;
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
var running: bool = false;
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
        if (win32.GetProcAddress(x_input_library, "XInputGetState")) |x_input_get_state| {
            XInputGetState.call = @as(@TypeOf(XInputGetState.call), @ptrCast(x_input_get_state));
        } else {
            XInputGetState.call = XInputGetState.stub;
        }

        if (win32.GetProcAddress(x_input_library, "XInputSetState")) |x_input_set_state| {
            XInputSetState.call = @as(@TypeOf(XInputSetState.call), @ptrCast(x_input_set_state));
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
        if (win32.GetProcAddress(direct_sound_library, "DirectSoundCreate")) |direct_sound_create_address| {
            direct_sound_create = @as(@TypeOf(direct_sound_create), @ptrCast(direct_sound_create_address));
            var maybe_direct_sound: ?*win32.IDirectSound = undefined;

            if (win32.SUCCEEDED(direct_sound_create(null, &maybe_direct_sound, null))) {
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
                            // NOTE: we finally have the format!
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
                    // Start playing?
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

fn render_weird_gradient(
    buffer: *BackBuffer,
    x_offset: u32,
    y_offset: u32,
) !void {
    var row: [*]u8 = @ptrCast(buffer.memory);

    for (0..@intCast(buffer.height)) |y| {
        var pixel: [*]u32 = @ptrCast(@alignCast(row));

        for (0..@intCast(buffer.width)) |x| {
            var blue: u32 = @as(u8, @truncate(x + x_offset));
            var green: u32 = @as(u8, @truncate(y + y_offset));

            pixel[0] = (green << 8) | blue;
            pixel += 1;
        }

        row += @as(usize, @intCast(buffer.pitch));
    }
}

fn win32_resize_dib_section(
    buffer: *BackBuffer,
    width: i32,
    height: i32,
) !void {
    // TODO: Maybe don't free first, free after, then free first if that fails
    if (buffer.*.memory != undefined) {
        _ = win32.VirtualFree(buffer.*.memory, 0, win32.MEM_RELEASE);
    }

    buffer.*.width = width;
    buffer.*.height = height;
    buffer.*.bytes_per_pixel = 4;

    buffer.*.info.bmiHeader.biSize = @sizeOf(@TypeOf(buffer.*.info.bmiHeader));
    buffer.*.info.bmiHeader.biWidth = buffer.*.width;
    buffer.*.info.bmiHeader.biHeight = -buffer.*.height;
    buffer.*.info.bmiHeader.biPlanes = 1;
    buffer.*.info.bmiHeader.biBitCount = 32;
    buffer.*.info.bmiHeader.biCompression = win32.BI_RGB;

    var bitmap_memory_size = buffer.*.bytes_per_pixel * (buffer.*.width * buffer.*.height);
    buffer.*.memory = win32.VirtualAlloc(
        null,
        @intCast(bitmap_memory_size),
        win32.VIRTUAL_ALLOCATION_TYPE{ .RESERVE = 1, .COMMIT = 1 },
        win32.PAGE_READWRITE,
    );

    buffer.*.pitch = width * buffer.*.bytes_per_pixel;

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
            running = false;
        },
        win32.WM_ACTIVATEAPP => {
            win32.OutputDebugStringA("WM_ACTIVATEAPP\n");
        },
        win32.WM_DESTROY => {
            // TODO: Handle as an error--recreate window?
            running = false;
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
                running = false;
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

pub export fn wWinMain(
    instance: ?win32.HINSTANCE,
    _: ?win32.HINSTANCE,
    _: [*:0]u16,
    _: u32,
) callconv(WINAPI) c_int {
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
                var x_offset: u32 = 0;
                var y_offset: u32 = 0;

                const samples_per_second = 48_000;
                const tone_hertz = 256;
                const tone_volume = 1_000;
                var running_sample_index: u32 = 0;
                var square_wave_period: u32 = samples_per_second / tone_hertz;
                var half_square_wave_period: u32 = square_wave_period / 2;
                const bytes_per_sample = @sizeOf(i16) * 2;
                const secondary_buffer_size = samples_per_second * bytes_per_sample;

                running = true;

                try win32_init_direct_sound(
                    window,
                    samples_per_second,
                    secondary_buffer_size,
                );

                var sound_is_playing = false;

                while (running) {
                    var message: win32.MSG = undefined;

                    while (win32.PeekMessageW(&message, null, 0, 0, win32.PM_REMOVE) > 0) {
                        if (message.message == win32.WM_QUIT) {
                            running = false;
                        }

                        _ = win32.TranslateMessage(&message);
                        _ = win32.DispatchMessageW(&message);
                    }

                    // TODO: should we poll this more frequently?
                    for (0..win32.XUSER_MAX_COUNT) |controller_index| {
                        var controller_state: win32.XINPUT_STATE = undefined;

                        if (XInputGetState.call(@as(u32, @intCast(controller_index)), &controller_state) == @intFromEnum(win32.ERROR_SUCCESS)) {
                            // NOTE: Controller is plugged in
                            // TODO: See if controller_state.dwPacketNumber increments too quickly
                            var pad: *win32.XINPUT_GAMEPAD = &controller_state.Gamepad;

                            // var up = pad.*.wButtons & win32.XINPUT_GAMEPAD_DPAD_UP;
                            // var down = pad.*.wButtons & win32.XINPUT_GAMEPAD_DPAD_DOWN;
                            // var left = pad.*.wButtons & win32.XINPUT_GAMEPAD_DPAD_LEFT;
                            // var right = pad.*.wButtons & win32.XINPUT_GAMEPAD_DPAD_RIGHT;
                            // var start = pad.*.wButtons & win32.XINPUT_GAMEPAD_START;
                            // var back = pad.*.wButtons & win32.XINPUT_GAMEPAD_BACK;
                            // var left_shoulder = pad.*.wButtons & win32.XINPUT_GAMEPAD_LEFT_SHOULDER;
                            // var right_shoulder = pad.*.wButtons & win32.XINPUT_GAMEPAD_RIGHT_SHOULDER;
                            var a_button = pad.*.wButtons & win32.XINPUT_GAMEPAD_A;
                            // var b_button = pad.*.wButtons & win32.XINPUT_GAMEPAD_B;
                            // var x_button = pad.*.wButtons & win32.XINPUT_GAMEPAD_X;
                            // var y_button = pad.*.wButtons & win32.XINPUT_GAMEPAD_Y;

                            // var stick_x = pad.*.sThumbLX;
                            // var stick_y = pad.*.sThumbLY;

                            if (a_button > 0) {
                                y_offset += 2;
                            }
                        } else {
                            // NOTE: Controller is unavailable
                        }
                    }

                    // var vibration: win32.XINPUT_VIBRATION = undefined;
                    // vibration.wLeftMotorSpeed = 60000;
                    // vibration.wRightMotorSpeed = 60000;
                    // _ = XInputSetState.call(0, &vibration);

                    try render_weird_gradient(&global_back_buffer, x_offset, y_offset);

                    // NOTE: DirectSound output test
                    var play_cursor: u32 = undefined;
                    var write_cursor: u32 = undefined;

                    if (win32.SUCCEEDED(global_secondary_buffer.vtable.GetCurrentPosition(
                        global_secondary_buffer,
                        &play_cursor,
                        &write_cursor,
                    ))) {
                        var byte_to_lock: u32 = running_sample_index * bytes_per_sample % secondary_buffer_size;
                        var bytes_to_write: u32 = undefined;

                        if (byte_to_lock == play_cursor) {
                            bytes_to_write = secondary_buffer_size;
                        } else if (byte_to_lock > play_cursor) {
                            bytes_to_write = secondary_buffer_size - byte_to_lock;
                            bytes_to_write += play_cursor;
                        } else {
                            bytes_to_write = play_cursor - byte_to_lock;
                        }

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
                                var region_1_sample_count: u32 = region_1_size / bytes_per_sample;
                                var sample_out = @as([*]i16, @alignCast(@ptrCast(region_1)));

                                for (0..region_1_sample_count) |_| {
                                    var sample_value: i16 = if (((running_sample_index / half_square_wave_period) % 2) > 0) tone_volume else -tone_volume;
                                    sample_out[0] = sample_value;
                                    sample_out += 1;
                                    sample_out[0] = sample_value;
                                    sample_out += 1;
                                    running_sample_index += 1;
                                }
                            }

                            if (maybe_region_2) |region_2| {
                                var region_2_sample_count: u32 = region_2_size / bytes_per_sample;
                                var sample_out = @as([*]i16, @alignCast(@ptrCast(region_2)));
                                for (0..region_2_sample_count) |_| {
                                    var sample_value: i16 = if (((running_sample_index / half_square_wave_period) % 2) > 0) tone_volume else -tone_volume;
                                    sample_out[0] = sample_value;
                                    sample_out += 1;
                                    sample_out[0] = sample_value;
                                    sample_out += 1;
                                    running_sample_index += 1;
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

                    if (!sound_is_playing) {
                        _ = global_secondary_buffer.vtable.Play(
                            global_secondary_buffer,
                            0,
                            0,
                            win32.DSBPLAY_LOOPING,
                        );
                        sound_is_playing = true;
                    }

                    var dimension = try win32_get_window_dimension(window);

                    try win32_display_buffer_in_window(
                        &global_back_buffer,
                        device_context,
                        dimension.width,
                        dimension.height,
                    );

                    x_offset += 1;
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
