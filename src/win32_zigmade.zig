const std = @import("std");

const WINAPI = std.os.windows.WINAPI;
const DWORD = std.os.windows.DWORD;

const win32 = struct {
    usingnamespace @import("win32").zig;
    usingnamespace @import("win32").foundation;
    usingnamespace @import("win32").system;
    usingnamespace @import("win32").system.memory;
    usingnamespace @import("win32").graphics.gdi;
    usingnamespace @import("win32").ui.windows_and_messaging;
    usingnamespace @import("win32").system.diagnostics.debug;
};

// TODO: Use a global State struct for now
const State = struct {
    var running: bool = undefined;
};

const Bitmap = struct {
    var info: win32.BITMAPINFO = undefined;
    var memory: ?*anyopaque = undefined;
    var width: i32 = undefined;
    var height: i32 = undefined;
};

const bytes_per_pixel = 4;

fn renderWeirdGradient(
    x_offset: u32,
    y_offset: u32,
) !void {
    var pitch: usize = @intCast(Bitmap.width * bytes_per_pixel);
    var row: [*]u8 = @ptrCast(Bitmap.memory);

    for (0..@intCast(Bitmap.height)) |y| {
        var pixel: [*]u32 = @ptrCast(@alignCast(row));

        for (0..@intCast(Bitmap.width)) |x| {
            var blue: u32 = @as(u8, @truncate(x + x_offset));
            var green: u32 = @as(u8, @truncate(y + y_offset));

            pixel[0] = (green << 8) | blue;
            pixel += 1;
        }

        row += pitch;
    }
}

fn win32ResizeDIBSection(width: i32, height: i32) !void {
    // TODO: Maybe don't free first, free after, then free first if that fails
    if (Bitmap.memory != undefined) {
        _ = win32.VirtualFree(Bitmap.memory, 0, win32.MEM_RELEASE);
    }

    Bitmap.width = width;
    Bitmap.height = height;

    Bitmap.info.bmiHeader.biSize = @sizeOf(@TypeOf(Bitmap.info.bmiHeader));
    Bitmap.info.bmiHeader.biWidth = Bitmap.width;
    Bitmap.info.bmiHeader.biHeight = -Bitmap.height;
    Bitmap.info.bmiHeader.biPlanes = 1;
    Bitmap.info.bmiHeader.biBitCount = 32;
    Bitmap.info.bmiHeader.biCompression = win32.BI_RGB;

    var bitmap_memory_size = bytes_per_pixel * (Bitmap.width * Bitmap.height);
    Bitmap.memory = win32.VirtualAlloc(
        null,
        @intCast(bitmap_memory_size),
        win32.MEM_COMMIT,
        win32.PAGE_READWRITE,
    );

    // TODO: clear bitmap to black
}

fn win32UpdateWindow(
    device_context: ?win32.HDC,
    client_rect: *win32.RECT,
    // _: win32.HWND,
    // x: i32,
    // y: i32,
    // width: i32,
    // height: i32,
) !void {
    var window_width = client_rect.*.right - client_rect.*.left;
    var window_height = client_rect.*.bottom - client_rect.*.top;

    _ = win32.StretchDIBits(
        device_context,
        0,
        0,
        Bitmap.width,
        Bitmap.height,
        0,
        0,
        window_width,
        window_height,
        Bitmap.memory,
        &Bitmap.info,
        win32.DIB_RGB_COLORS,
        win32.SRCCOPY,
    );
}

fn win32MainWindowCallback(
    window: win32.HWND,
    message: u32,
    wParam: win32.WPARAM,
    lParam: win32.LPARAM,
) callconv(WINAPI) win32.LRESULT {
    var result: win32.LRESULT = 0;

    switch (message) {
        win32.WM_SIZE => {
            var client_rect = std.mem.zeroInit(win32.RECT, .{});
            _ = win32.GetClientRect(window, &client_rect);

            var width = client_rect.right - client_rect.left;
            var height = client_rect.bottom - client_rect.top;

            try win32ResizeDIBSection(width, height);
            win32.OutputDebugStringA("WM_SIZE\n");
        },
        win32.WM_CLOSE => {
            // TODO: Handle with a message to the user?
            State.running = false;
        },
        win32.WM_ACTIVATEAPP => {
            win32.OutputDebugStringA("WM_ACTIVATEAPP\n");
        },
        win32.WM_DESTROY => {
            // TODO: Handle as an error--recreate window?
            State.running = false;
        },
        win32.WM_PAINT => {
            var paint = std.mem.zeroInit(win32.PAINTSTRUCT, .{});
            var device_context = win32.BeginPaint(window, &paint);
            // var x = paint.rcPaint.left;
            // var y = paint.rcPaint.top;
            // var height = paint.rcPaint.bottom - paint.rcPaint.top;
            // var width = paint.rcPaint.right - paint.rcPaint.left;

            var client_rect = std.mem.zeroInit(win32.RECT, .{});
            _ = win32.GetClientRect(window, &client_rect);

            try win32UpdateWindow(device_context, &client_rect);

            _ = win32.EndPaint(window, &paint);
        },
        else => {
            result = win32.DefWindowProcW(window, message, wParam, lParam);
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
    var window_class = std.mem.zeroInit(win32.WNDCLASSW, .{});

    // TODO: does HREDRAW/OWNDC/VREDRAW still matter
    window_class.style = win32.WNDCLASS_STYLES{ .OWNDC = 1, .HREDRAW = 1, .VREDRAW = 1 };
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
            State.running = true;

            var x_offset: u32 = 0;
            var y_offset: u32 = 0;

            while (State.running) {
                var message = std.mem.zeroInit(win32.MSG, .{});

                while (win32.PeekMessageW(
                    &message,
                    null,
                    0,
                    0,
                    win32.PEEK_MESSAGE_REMOVE_TYPE{ .REMOVE = 1 },
                ) > 0) {
                    if (message.message == win32.WM_QUIT) {
                        State.running = false;
                    }

                    _ = win32.TranslateMessage(&message);
                    _ = win32.DispatchMessageW(&message);
                }

                try renderWeirdGradient(x_offset, y_offset);

                var device_context = win32.GetDC(window);
                var client_rect = std.mem.zeroInit(win32.RECT, .{});
                _ = win32.GetClientRect(window, &client_rect);
                // var window_width = client_rect.right - client_rect.left;
                // var window_height = client_rect.bottom - client_rect.top;

                try win32UpdateWindow(
                    device_context,
                    &client_rect,
                    // 0,
                    // 0,
                    // window_width,
                    // window_height
                );

                _ = win32.ReleaseDC(window, device_context);
                x_offset += 1;
                // y_offset += 1;
            }
        } else {
            // TODO: logging
        }
    } else {
        // TODO: logging
    }

    return (0);
}
