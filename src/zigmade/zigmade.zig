// TODO: Services that the platform layer provides to the game

// NOTE: Services that the game provides to the platform layer
// maybe expand in the future - sound on separate thread

pub const GameOffscreenBuffer = struct {
    memory: ?*anyopaque = undefined,
    width: i32 = undefined,
    height: i32 = undefined,
    pitch: i32 = undefined,
};

fn render_weird_gradient(
    buffer: *GameOffscreenBuffer,
    blue_offset: i32,
    green_offset: i32,
) !void {
    var row: [*]u8 = @ptrCast(buffer.memory);

    for (0..@intCast(buffer.height)) |y| {
        var pixel: [*]u32 = @ptrCast(@alignCast(row));

        for (0..@intCast(buffer.width)) |x| {
            var blue: u32 = @as(
                u8,
                @truncate(x + @as(
                    u32,
                    @bitCast(blue_offset),
                )),
            );
            var green: u32 = @as(
                u8,
                @truncate(y + @as(
                    u32,
                    @bitCast(green_offset),
                )),
            );

            pixel[0] = (green << 8) | blue;
            pixel += 1;
        }

        row += @as(usize, @intCast(buffer.pitch));
    }
}

// GAME NEEDS FOUR THINGS
// - timing
// - controller/keyboard input
// - bitmap buffer
// - sound buffer
pub fn game_update_and_render(
    buffer: *GameOffscreenBuffer,
    blue_offset: i32,
    green_offset: i32,
) !void {
    try render_weird_gradient(
        buffer,
        blue_offset,
        green_offset,
    );
}
