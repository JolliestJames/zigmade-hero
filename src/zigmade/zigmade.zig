const std = @import("std");
const assert = std.debug.assert;
const platform = @import("zigmade_platform");
const tiling = @import("zigmade_tile.zig");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;

const GameState = struct {
    player_p: tiling.TileMapPosition,
    world: ?*World = null,
    world_arena: MemoryArena,
};

const World = struct {
    tile_map: *tiling.TileMap,
};

pub const MemoryArena = struct {
    size: usize,
    base: [*]u8,
    used: usize,
};

fn game_output_sound(
    sound_buffer: *platform.GameSoundBuffer,
    game_state: *GameState,
) !void {
    _ = game_state;
    const tone_volume: i16 = 3_000;
    _ = tone_volume;
    const wave_period = @divTrunc(
        @as(i32, @intCast(sound_buffer.samples_per_second)),
        400,
    );
    _ = wave_period;
    var sample_out = sound_buffer.samples;

    for (0..@intCast(sound_buffer.sample_count)) |i| {
        //var sine_value = @sin(game_state.t_sine);
        //var sample_value = @as(i16, @intFromFloat(
        //    sine_value * @as(
        //        f16,
        //        @floatFromInt(tone_volume),
        //    ),
        //));

        const sample_value: i16 = 0;
        sample_out[2 * i] = sample_value;
        sample_out[2 * i + 1] = sample_value;

        //game_state.t_sine +=
        //    2.0 *
        //    std.math.pi *
        //    1.0 /
        //    @as(f32, @floatFromInt(wave_period));

        //if (game_state.t_sine > 2.0 * std.math.pi) {
        //    game_state.t_sine -= 2.0 * std.math.pi;
        //}
    }
}

fn render_weird_gradient(
    buffer: *platform.GameOffscreenBuffer,
    blue_offset: i32,
    green_offset: i32,
) !void {
    var row: [*]u8 = @alignCast(@ptrCast(buffer.memory));

    for (0..@intCast(buffer.height)) |y| {
        var pixel: [*]u32 = @ptrCast(@alignCast(row));

        for (0..@intCast(buffer.width)) |x| {
            const blue: u32 = @as(u8, @truncate(x + @as(
                u32,
                @bitCast(blue_offset),
            )));
            const green: u32 = @as(u8, @truncate(y + @as(
                u32,
                @bitCast(green_offset),
            )));

            pixel[x] = (green << 16) | blue;
        }

        row += @as(usize, @intCast(buffer.pitch));
    }
}

fn draw_rectangle(
    buffer: *platform.GameOffscreenBuffer,
    f_min_x: f32,
    f_min_y: f32,
    f_max_x: f32,
    f_max_y: f32,
    r: f32,
    g: f32,
    b: f32,
) !void {
    var min_x: i32 = @intFromFloat(@round(f_min_x));
    var min_y: i32 = @intFromFloat(@round(f_min_y));
    var max_x: i32 = @intFromFloat(@round(f_max_x));
    var max_y: i32 = @intFromFloat(@round(f_max_y));

    if (min_x < 0) min_x = 0;
    if (min_y < 0) min_y = 0;
    if (max_x > buffer.width) max_x = buffer.width;
    if (max_y > buffer.height) max_y = buffer.height;
    if (min_x > max_x) max_x = min_x;
    if (min_y > max_y) max_y = min_y;

    const color: u32 =
        (@as(u32, (@intFromFloat(@round(r * 255.0)))) << 16) |
        (@as(u32, (@intFromFloat(@round(g * 255.0)))) << 8) |
        (@as(u32, (@intFromFloat(@round(b * 255.0)))) << 0);

    var row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
        (@as(usize, @intCast(min_x)) *
        @as(usize, @intCast(buffer.bytes_per_pixel))) +
        @as(u32, @bitCast(min_y *% buffer.pitch));

    for (@intCast(min_y)..@intCast(max_y)) |_| {
        var pixel: [*]u32 = @alignCast(@ptrCast(row));

        for (@intCast(min_x)..@intCast(max_x)) |_| {
            pixel[0] = color;
            pixel += 1;
        }

        row += @as(usize, @intCast(buffer.pitch));
    }
}

fn initialize_arena(
    arena: *MemoryArena,
    size: usize,
    base: [*]u8,
) void {
    arena.size = size;
    arena.base = base;
    arena.used = 0;
}

fn push_size(
    arena: *MemoryArena,
    comptime T: type,
) *T {
    assert((arena.used + @sizeOf(T)) <= arena.size);
    const result = arena.base + arena.used;
    arena.used += @sizeOf(T);
    return @as(*T, @alignCast(@ptrCast(result)));
}

fn push_array(
    arena: *MemoryArena,
    count: usize,
    comptime T: type,
) [*]T {
    assert((arena.used + count * @sizeOf(T)) <= arena.size);
    const result = arena.base + arena.used;
    arena.used += count * @sizeOf(T);
    return @as([*]T, @alignCast(@ptrCast(result)));
}

// GAME NEEDS FOUR THINGS
// - timing
// - controller/keyboard input
// - bitmap buffer
// - sound buffer
pub export fn update_and_render(
    thread: *platform.ThreadContext,
    memory: *platform.GameMemory,
    input: *platform.GameInput,
    buffer: *platform.GameOffscreenBuffer,
) void {
    _ = thread;
    assert(@sizeOf(@TypeOf(input.controllers[0].buttons.map)) ==
        @sizeOf(platform.GameButtonState) * input.controllers[0].buttons.array.len);
    assert(@sizeOf(GameState) <= memory.permanent_storage_size);

    const player_height = 1.4;
    const player_width = 0.75 * player_height;

    var game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    if (!memory.is_initialized) {
        // TODO: This may be more appropriate to do in the platform layer
        game_state.player_p.abs_tile_x = 1;
        game_state.player_p.abs_tile_y = 3;
        game_state.player_p.rel_tile_x = 5.0;
        game_state.player_p.rel_tile_y = 5.0;

        // TODO: Can we just use Zig's own arena allocator?
        initialize_arena(
            &game_state.world_arena,
            memory.permanent_storage_size - @sizeOf(GameState),
            memory.permanent_storage + @sizeOf(GameState),
        );

        game_state.world = push_size(&game_state.world_arena, World);
        var world = game_state.world.?;
        world.tile_map = push_size(&game_state.world_arena, tiling.TileMap);

        var tile_map = world.tile_map;

        tile_map.chunk_shift = 4;
        tile_map.chunk_mask = (@as(u32, @intCast(1)) <<
            @as(u5, @intCast(tile_map.chunk_shift))) - 1;
        tile_map.chunk_dim = (@as(u32, @intCast(1)) <<
            @as(u5, @intCast(tile_map.chunk_shift)));
        tile_map.tile_chunk_count_x = 128;
        tile_map.tile_chunk_count_y = 128;

        tile_map.tile_chunks = push_array(
            &game_state.world_arena,
            tile_map.tile_chunk_count_x * tile_map.tile_chunk_count_y,
            tiling.TileChunk,
        );

        for (0..tile_map.tile_chunk_count_y) |y| {
            for (0..tile_map.tile_chunk_count_x) |x| {
                tile_map.tile_chunks.?[y * tile_map.tile_chunk_count_x + x].tiles =
                    push_array(
                    &game_state.world_arena,
                    tile_map.chunk_dim * tile_map.chunk_dim,
                    usize,
                );
            }
        }

        // TODO: Begin using tile side in meters
        tile_map.tile_side_in_meters = 1.4;
        tile_map.tile_side_in_pixels = 60;
        tile_map.meters_to_pixels =
            @as(f32, @floatFromInt(tile_map.tile_side_in_pixels)) /
            tile_map.tile_side_in_meters;

        const lower_left_x: f32 = @floatFromInt(@divFloor(-tile_map.tile_side_in_pixels, 2));
        _ = lower_left_x;
        const lower_left_y: f32 = @floatFromInt(buffer.height);
        _ = lower_left_y;

        const tiles_per_width = 17;
        const tiles_per_height = 9;

        for (0..32) |screen_y| {
            for (0..32) |screen_x| {
                for (0..tiles_per_height) |tile_y| {
                    for (0..tiles_per_width) |tile_x| {
                        const abs_tile_x = screen_x * tiles_per_width + tile_x;
                        const abs_tile_y = screen_y * tiles_per_height + tile_y;

                        tiling.set_tile_value(
                            &game_state.world_arena,
                            tile_map,
                            abs_tile_x,
                            abs_tile_y,
                            if ((tile_x == tile_y) and (tile_y % 2 == 0))
                                1
                            else
                                0,
                        );
                    }
                }
            }
        }

        memory.is_initialized = true;
    }

    const world = game_state.world.?;
    const tile_map = world.tile_map;

    for (0..input.controllers.len) |controller_index| {
        const controller: *platform.GameControllerInput =
            try platform.get_controller(input, controller_index);

        if (controller.is_analog) {
            // NOTE: Use analog movement tuning
        } else {
            // NOTE: Use digital movement tuning
            var d_player_x: f32 = 0.0; //pixels/s
            var d_player_y: f32 = 0.0; //pixels/s

            // TODO: Investigate timing tomorrow and verify that it
            // is working properly because it appears things are moving
            // half as fast as expected
            if (controller.buttons.map.move_up.ended_down) {
                d_player_y = 1.0;
            }

            if (controller.buttons.map.move_down.ended_down) {
                d_player_y = -1.0;
            }

            if (controller.buttons.map.move_left.ended_down) {
                d_player_x = -1.0;
            }

            if (controller.buttons.map.move_right.ended_down) {
                d_player_x = 1.0;
            }

            var player_speed: f32 = 2.0;

            if (controller.buttons.map.action_up.ended_down) {
                player_speed = 10.0;
            }

            d_player_x *= player_speed;
            d_player_y *= player_speed;

            // TODO: Diagonal will be faster. Fix once we have vectors!
            var new_player_p = game_state.player_p;
            new_player_p.rel_tile_x += input.dt_for_frame * d_player_x;
            new_player_p.rel_tile_y += input.dt_for_frame * d_player_y;
            new_player_p = tiling.recanonicalize_position(tile_map, new_player_p);
            //// TODO: delta function to auto-recanonicalize

            var player_left = new_player_p;
            player_left.rel_tile_x -= 0.5 * player_width;
            player_left = tiling.recanonicalize_position(tile_map, player_left);

            var player_right = new_player_p;
            player_right.rel_tile_x += 0.5 * player_width;
            player_right = tiling.recanonicalize_position(tile_map, player_right);

            if (tiling.is_world_point_empty(tile_map, new_player_p) and
                tiling.is_world_point_empty(tile_map, player_right) and
                tiling.is_world_point_empty(tile_map, player_left))
            {
                game_state.player_p = new_player_p;
            }
        }
    }

    try draw_rectangle(
        buffer,
        0.0,
        0.0,
        @floatFromInt(buffer.width),
        @floatFromInt(buffer.height),
        1.0,
        0.0,
        0.1,
    );

    const screen_center_x = 0.5 * @as(f32, @floatFromInt(buffer.width));
    const screen_center_y = 0.5 * @as(f32, @floatFromInt(buffer.height));

    var rel_row: i32 = -10;

    while (rel_row < 10) : (rel_row += 1) {
        var rel_column: i32 = -20;

        while (rel_column < 20) : (rel_column += 1) {
            const column = @as(usize, @bitCast(
                @as(isize, @bitCast(game_state.player_p.abs_tile_x)) +
                    rel_column,
            ));

            const row = @as(usize, @bitCast(
                @as(isize, @bitCast(game_state.player_p.abs_tile_y)) +
                    rel_row,
            ));

            const tile_id = tiling.get_tile_value(
                tile_map,
                @truncate(column),
                @truncate(row),
            );

            var color: f32 = 0.5;

            if (tile_id == 1) color = 1.0;

            if ((column == game_state.player_p.abs_tile_x) and
                (row == game_state.player_p.abs_tile_y))
            {
                color = 0.0;
            }

            const cen_x = screen_center_x -
                tile_map.meters_to_pixels * game_state.player_p.rel_tile_x +
                @as(f32, @floatFromInt(rel_column * tile_map.tile_side_in_pixels));

            const cen_y = screen_center_y +
                tile_map.meters_to_pixels * game_state.player_p.rel_tile_y -
                @as(f32, @floatFromInt(rel_row * tile_map.tile_side_in_pixels));

            const min_x = cen_x - 0.5 * @as(f32, @floatFromInt(tile_map.tile_side_in_pixels));
            const min_y = cen_y - 0.5 * @as(f32, @floatFromInt(tile_map.tile_side_in_pixels));
            const max_x = cen_x + 0.5 * @as(f32, @floatFromInt(tile_map.tile_side_in_pixels));
            const max_y = cen_y + 0.5 * @as(f32, @floatFromInt(tile_map.tile_side_in_pixels));

            try draw_rectangle(
                buffer,
                min_x,
                min_y,
                max_x,
                max_y,
                color,
                color,
                color,
            );
        }
    }

    const player_r = 1.0;
    const player_g = 1.0;
    const player_b = 0.0;
    const player_left = screen_center_x - 0.5 * tile_map.meters_to_pixels * player_width;
    const player_top = screen_center_y - tile_map.meters_to_pixels * player_height;

    try draw_rectangle(
        buffer,
        player_left,
        player_top,
        player_left + tile_map.meters_to_pixels * player_width,
        player_top + tile_map.meters_to_pixels * player_height,
        player_r,
        player_g,
        player_b,
    );
}

// NOTE: At the moment, this must be a very fast function
// It cannot be greater than ~1ms
// TODO: Reduce the pressure on this function's performance
// by measuring it or asking about it, etc.
pub export fn get_sound_samples(
    thread: *platform.ThreadContext,
    memory: *platform.GameMemory,
    sound_buffer: *platform.GameSoundBuffer,
) void {
    _ = thread;
    const game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    try game_output_sound(
        sound_buffer,
        game_state,
    );
}
