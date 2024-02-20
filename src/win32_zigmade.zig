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

fn win32GetWindowDimension(window: win32.HWND) !WindowDimension {
    var result: WindowDimension = undefined;

    var client_rect: win32.RECT = undefined;
    _ = win32.GetClientRect(window, &client_rect);

    result.width = client_rect.right - client_rect.left;
    result.height = client_rect.bottom - client_rect.top;

    return (result);
}

fn renderWeirdGradient(
    buffer: BackBuffer,
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
    device_context: ?win32.HDC,
    window_width: i32,
    window_height: i32,
    buffer: BackBuffer,
    // _: win32.HWND,
    // x: i32,
    // y: i32,
    // width: i32,
    // height: i32,
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
    wParam: win32.WPARAM,
    lParam: win32.LPARAM,
) callconv(WINAPI) win32.LRESULT {
    var result: win32.LRESULT = 0;

    switch (message) {
        win32.WM_SIZE => {},
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
        win32.WM_PAINT => {
            var paint = std.mem.zeroInit(win32.PAINTSTRUCT, .{});
            var device_context = win32.BeginPaint(window, &paint);
            // var x = paint.rcPaint.left;
            // var y = paint.rcPaint.top;
            var dimension = try win32GetWindowDimension(window);

            try win32DisplayBufferInWindow(device_context, dimension.width, dimension.height, global_back_buffer);

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
            running = true;

            var x_offset: u32 = 0;
            var y_offset: u32 = 0;

            while (running) {
                var message: win32.MSG = undefined;

                while (win32.PeekMessageW(&message, null, 0, 0, win32.PM_REMOVE) > 0) {
                    if (message.message == win32.WM_QUIT) {
                        running = false;
                    }

                    _ = win32.TranslateMessage(&message);
                    _ = win32.DispatchMessageW(&message);
                }

                try renderWeirdGradient(global_back_buffer, x_offset, y_offset);

                if (win32.GetDC(window)) |device_context| {
                    var dimension = try win32GetWindowDimension(window);

                    try win32DisplayBufferInWindow(
                        device_context,
                        dimension.width,
                        dimension.height,
                        global_back_buffer,
                        // 0,
                        // 0,
                        // window_width,
                        // window_height
                    );

                    _ = win32.ReleaseDC(window, device_context);
                    x_offset += 1;
                    y_offset += 2;
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
