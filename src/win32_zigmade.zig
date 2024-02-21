const std = @import("std");

const WINAPI = std.os.windows.WINAPI;
const DWORD = std.os.windows.DWORD;

const win32 = struct {
    usingnamespace @import("win32").zig;
    usingnamespace @import("win32").foundation;
    usingnamespace @import("win32").system;
    usingnamespace @import("win32").system.memory;
    usingnamespace @import("win32").system.library_loader;
    usingnamespace @import("win32").graphics.gdi;
    usingnamespace @import("win32").ui.input.keyboard_and_mouse;
    usingnamespace @import("win32").ui.input.xbox_controller;
    usingnamespace @import("win32").ui.windows_and_messaging;
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
        return (0);
    }
};

const XInputSetState = struct {
    var call: *const fn (
        user_index: u32,
        vibration: ?*win32.XINPUT_VIBRATION,
    ) callconv(WINAPI) isize = undefined;

    fn stub(_: u32, _: ?*win32.XINPUT_VIBRATION) callconv(WINAPI) isize {
        return (0);
    }
};

fn win32LoadXInput() !void {
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
    }
}

fn win32GetWindowDimension(window: win32.HWND) !WindowDimension {
    var result: WindowDimension = undefined;

    var client_rect: win32.RECT = undefined;
    _ = win32.GetClientRect(window, &client_rect);

    result.width = client_rect.right - client_rect.left;
    result.height = client_rect.bottom - client_rect.top;

    return (result);
}

fn renderWeirdGradient(
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

fn win32ResizeDIBSection(
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
        win32.MEM_COMMIT,
        win32.PAGE_READWRITE,
    );

    buffer.*.pitch = width * buffer.*.bytes_per_pixel;

    // TODO: clear bitmap to black
}

fn win32DisplayBufferInWindow(
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

fn win32MainWindowCallback(
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
            var was_down = ((l_param & 1 << 30) != 0);
            var is_down = ((l_param & 1 << 31) == 0);

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
        },
        win32.WM_PAINT => {
            var paint = std.mem.zeroInit(win32.PAINTSTRUCT, .{});
            var device_context = win32.BeginPaint(window, &paint);
            var dimension = try win32GetWindowDimension(window);
            try win32DisplayBufferInWindow(
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
    try win32LoadXInput();

    var window_class = std.mem.zeroInit(win32.WNDCLASSW, .{});

    // TODO: would be nice to properly log when something goes wrong with these error union return types
    try win32ResizeDIBSection(&global_back_buffer, 1280, 720);

    window_class.style = win32.WNDCLASS_STYLES{ .HREDRAW = 1, .VREDRAW = 1 };
    window_class.lpfnWndProc = @ptrCast(&win32MainWindowCallback);
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

                running = true;

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

                    try renderWeirdGradient(&global_back_buffer, x_offset, y_offset);

                    var dimension = try win32GetWindowDimension(window);

                    try win32DisplayBufferInWindow(
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
