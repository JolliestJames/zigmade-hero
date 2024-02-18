const std = @import("std");

const WINAPI = std.os.windows.WINAPI;
const DWORD = std.os.windows.DWORD;

const win32 = struct {
    usingnamespace @import("win32").zig;
    usingnamespace @import("win32").foundation;
    usingnamespace @import("win32").system;
    usingnamespace @import("win32").graphics.gdi;
    usingnamespace @import("win32").ui.windows_and_messaging;
    usingnamespace @import("win32").system.diagnostics.debug;
};

// TODO: Use a global State struct for now
const State = struct {
    var running: bool = undefined;
    var bitmap_info: win32.BITMAPINFO = undefined;
    var bitmap_memory: ?*anyopaque = undefined;
    var bitmap_handle: ?win32.HBITMAP = undefined;
    var device_context: win32.HDC = undefined;
};

fn win32ResizeDIBSection(width: i32, height: i32) !void {
    // TODO: Maybe don't free first, free after, then free first if that fails
    if (State.bitmap_handle != undefined) {
        _ = win32.DeleteObject(State.bitmap_handle);
    }

    if (State.device_context == undefined) {
        // TODO: should we recreate DC under certain special circumstances?
        State.device_context = win32.CreateCompatibleDC(null);
    }

    State.bitmap_info.bmiHeader.biSize = @sizeOf(@TypeOf(State.bitmap_info.bmiHeader));
    State.bitmap_info.bmiHeader.biWidth = width;
    State.bitmap_info.bmiHeader.biHeight = height;
    State.bitmap_info.bmiHeader.biPlanes = 1;
    State.bitmap_info.bmiHeader.biBitCount = 32;
    State.bitmap_info.bmiHeader.biCompression = win32.BI_RGB;

    State.bitmap_handle = win32.CreateDIBSection(
        State.device_context,
        &State.bitmap_info,
        win32.DIB_RGB_COLORS,
        &State.bitmap_memory,
        null,
        0,
    );

    _ = win32.ReleaseDC(null, State.device_context);
}

fn win32UpdateWindow(
    device_context: ?win32.HDC,
    _: win32.HWND,
    x: i32,
    y: i32,
    width: i32,
    height: i32,
) !void {
    _ = win32.StretchDIBits(
        device_context,
        x,
        y,
        width,
        height,
        x,
        y,
        width,
        height,
        State.bitmap_memory,
        &State.bitmap_info,
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
            var x = paint.rcPaint.left;
            var y = paint.rcPaint.top;
            var height = paint.rcPaint.bottom - paint.rcPaint.top;
            var width = paint.rcPaint.right - paint.rcPaint.left;

            try win32UpdateWindow(device_context, window, x, y, width, height);

            _ = win32.EndPaint(window, &paint);
        },
        else => {
            // win32.OutputDebugStringA("default\n");
            result = win32.DefWindowProc(window, message, wParam, lParam);
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
    window_class.style = @enumFromInt(@intFromEnum(win32.CS_OWNDC) | @intFromEnum(win32.CS_HREDRAW) | @intFromEnum(win32.CS_VREDRAW));
    window_class.lpfnWndProc = @ptrCast(&win32MainWindowCallback);
    window_class.hInstance = instance;
    // window_class.hIcon = ;
    window_class.lpszClassName = win32.L("HandmadeHeroWindowClass");

    if (win32.RegisterClassW(&window_class) != 0) {
        if (win32.CreateWindowExW(
            @as(win32.WINDOW_EX_STYLE, @enumFromInt(0)),
            window_class.lpszClassName,
            win32.L("Handmade Hero"),
            @enumFromInt(@intFromEnum(win32.WS_OVERLAPPEDWINDOW) | @intFromEnum(win32.WS_VISIBLE)),
            win32.CW_USEDEFAULT,
            win32.CW_USEDEFAULT,
            win32.CW_USEDEFAULT,
            win32.CW_USEDEFAULT,
            null,
            null,
            instance,
            null,
        )) |_| {
            State.running = true;

            var message = std.mem.zeroInit(win32.MSG, .{});

            while (State.running) {
                var result = win32.GetMessageW(&message, null, 0, 0);

                if (result > 0) {
                    _ = win32.TranslateMessage(&message);
                    _ = win32.DispatchMessageW(&message);
                } else {
                    break;
                }
            }
        } else {
            // TODO: logging
        }
    } else {
        // TODO: logging
    }

    return (0);
}
