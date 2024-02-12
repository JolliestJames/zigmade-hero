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

fn MainWindowCallback(
    window: win32.HWND,
    message: u32,
    wParam: win32.WPARAM,
    lParam: win32.LPARAM,
) callconv(WINAPI) win32.LRESULT {
    var result: win32.LRESULT = 0;

    switch (message) {
        win32.WM_SIZE => {
            win32.OutputDebugStringA("WM_SIZE\n");
        },
        win32.WM_DESTROY => {
            win32.OutputDebugStringA("WM_SIZE\n");
        },
        win32.WM_CLOSE => {
            win32.OutputDebugStringA("WM_CLOSE\n");
        },
        win32.WM_ACTIVATEAPP => {
            win32.OutputDebugStringA("WM_ACTIVATEAPP\n");
        },
        win32.WM_PAINT => {
            var paint = std.mem.zeroInit(win32.PAINTSTRUCT, .{});

            var device_context = win32.BeginPaint(window, &paint);
            var x = paint.rcPaint.left;
            var y = paint.rcPaint.top;
            var height = paint.rcPaint.bottom - paint.rcPaint.top;
            var width = paint.rcPaint.right - paint.rcPaint.left;

            const state = struct {
                var operation: win32.ROP_CODE = win32.WHITENESS;
            };
            _ = win32.PatBlt(device_context, x, y, width, height, state.operation);

            if (state.operation == win32.WHITENESS) {
                state.operation = win32.BLACKNESS;
            } else {
                state.operation = win32.WHITENESS;
            }

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
    window_class.lpfnWndProc = @ptrCast(&MainWindowCallback);
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
            var message = std.mem.zeroInit(win32.MSG, .{});

            while (true) {
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
