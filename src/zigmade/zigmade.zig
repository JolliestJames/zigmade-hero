const std = @import("std");
const assert = std.debug.assert;
const platform = @import("zigmade_platform");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;
const TILE_MAP_COUNT_X = 17;
const TILE_MAP_COUNT_Y = 9;

const GameState = struct {
    player_p: CanonicalPosition,
    //player_x: f32,
    //player_y: f32,
    // TODO: Should player state be canonical now?
    player_tile_map_x: usize,
    player_tile_map_y: usize,
};

// TODO: This can become world position or something similar
const CanonicalPosition = struct {
    // TODO: Take tile map x and y and tile x and y
    // then pack them into single 32-bit values for x and y
    // where there is some low bits for the tile index
    // and the high bits are for the tile page
    // (we can eliminate the need for floor)
    tile_map_x: usize,
    tile_map_y: usize,
    tile_x: i32,
    tile_y: i32,
    _tile_x: u32,
    _tile_y: u32,
    // TODO: Y should go up
    // TODO: Should these be from the center of a tile?
    tile_rel_x: f32,
    tile_rel_y: f32,
};

const TileMap = struct {
    tiles: ?[*]u32 = undefined,
};

const World = struct {
    tile_side_in_meters: f32,
    tile_side_in_pixels: i32,
    meters_to_pixels: f32,
    // TODO: Beginner's sparseness
    tile_maps: ?[*]TileMap = undefined,
    tile_map_count_x: usize,
    tile_map_count_y: usize,
    count_x: usize,
    count_y: usize,
    upper_left_x: f32,
    upper_left_y: f32,
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
    var min_x: i32 = @intFromFloat(f_min_x);
    var min_y: i32 = @intFromFloat(f_min_y);
    var max_x: i32 = @intFromFloat(f_max_x);
    var max_y: i32 = @intFromFloat(f_max_y);

    if (min_x < 0) min_x = 0;
    if (min_y < 0) min_y = 0;
    if (max_x > buffer.width) max_x = buffer.width;
    if (max_y > buffer.height) max_y = buffer.height;
    if (min_x > max_x) max_x = min_x;
    if (min_y > max_y) max_y = min_y;

    const color: u32 = (@as(u32, (@intFromFloat(r * 255.0))) << 16) |
        (@as(u32, (@intFromFloat(g * 255.0))) << 8) |
        (@as(u32, (@intFromFloat(b * 255.0))) << 0);

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

inline fn get_tile_map(
    world: *World,
    tile_map_x: usize,
    tile_map_y: usize,
) ?*TileMap {
    var tile_map: ?*TileMap = undefined;

    if (tile_map_x >= 0 and
        tile_map_x < world.tile_map_count_x and
        tile_map_y >= 0 and
        tile_map_y < world.tile_map_count_y)
    {
        const tile_map_index =
            tile_map_y *
            world.tile_map_count_x +
            tile_map_x;

        if (world.tile_maps) |tile_maps| {
            tile_map = &tile_maps[tile_map_index];
        }
    }

    return tile_map;
}

inline fn get_tile_value_unchecked(
    world: *World,
    tile_map: ?*TileMap,
    tile_x: usize,
    tile_y: usize,
) u32 {
    assert(tile_map != null);
    assert(tile_x >= 0 and
        tile_x < world.count_x and
        tile_y >= 0 and
        tile_y < world.count_y);

    const tile_index = tile_y * world.count_x + tile_x;
    const tiles = tile_map.?.tiles.?;
    const tile_map_value = tiles[tile_index];
    return tile_map_value;
}

inline fn is_tile_map_point_empty(
    world: *World,
    tile_map: ?*TileMap,
    test_tile_x: usize,
    test_tile_y: usize,
) bool {
    var empty = false;

    if (tile_map) |tiles| {
        if (test_tile_x >= 0 and
            test_tile_x < world.count_x and
            test_tile_y >= 0 and
            test_tile_y < world.count_y)
        {
            const tile_map_value = get_tile_value_unchecked(
                world,
                tiles,
                test_tile_x,
                test_tile_y,
            );

            empty = tile_map_value == 0;
        }
    }

    return empty;
}

inline fn recanonicalize_coordinate(
    world: *World,
    tile_count: usize,
    tile_map: *usize,
    tile: *i32,
    tile_rel: *f32,
) void {
    // TODO: Don't use the divide/multiply method for recanonicalizing
    // because this can round back onto the previous tile
    // TODO: Add bounds checking to prevent wrapping
    //const f_tile_side_in_meters = @as(f32, @floatFromInt(world.tile_side_in_meters));

    const offset = @as(i32, @intFromFloat(@divFloor(tile_rel.*, world.tile_side_in_meters)));
    tile.* += offset;
    tile_rel.* -= @as(f32, @floatFromInt(offset)) * world.tile_side_in_meters;

    assert(tile_rel.* >= 0.0);
    // TODO: Fix floating point math so this can be <
    // NOTE: With <, this assert only seems to trip with Casey's code
    assert(tile_rel.* <= world.tile_side_in_meters);

    if (tile.* < 0) {
        tile.* = @as(i32, @intCast(tile_count)) + tile.*;
        tile_map.* -= 1;
    }

    if (tile.* >= tile_count) {
        tile.* = tile.* - @as(i32, @intCast(tile_count));
        tile_map.* += 1;
    }
}

inline fn recanonicalize_position(
    world: *World,
    pos: CanonicalPosition,
) CanonicalPosition {
    var result = pos;

    recanonicalize_coordinate(
        world,
        world.count_x,
        &result.tile_map_x,
        &result.tile_x,
        &result.tile_rel_x,
    );

    recanonicalize_coordinate(
        world,
        world.count_y,
        &result.tile_map_y,
        &result.tile_y,
        &result.tile_rel_y,
    );

    return result;
}

inline fn is_world_point_empty(
    world: *World,
    can_pos: CanonicalPosition,
) bool {
    var empty = false;

    const tile_map = get_tile_map(world, can_pos.tile_map_x, can_pos.tile_map_y);

    empty = is_tile_map_point_empty(
        world,
        tile_map,
        @intCast(can_pos.tile_x),
        @intCast(can_pos.tile_y),
    );

    return empty;
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

    var tiles_00 = [TILE_MAP_COUNT_Y][TILE_MAP_COUNT_X]u32{
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
        [_]u32{ 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1 },
        [_]u32{ 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 },
        [_]u32{ 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1 },
        [_]u32{ 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1 },
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1 },
    };

    var tiles_01 = [TILE_MAP_COUNT_Y][TILE_MAP_COUNT_X]u32{
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    };

    var tiles_10 = [TILE_MAP_COUNT_Y][TILE_MAP_COUNT_X]u32{
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1 },
    };

    var tiles_11 = [TILE_MAP_COUNT_Y][TILE_MAP_COUNT_X]u32{
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 },
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 },
    };

    var tile_maps = std.mem.zeroes([2][2]TileMap);

    tile_maps[0][0] = TileMap{
        .tiles = @ptrCast(&tiles_00),
    };

    tile_maps[0][1].tiles = @ptrCast(&tiles_10);
    tile_maps[1][0].tiles = @ptrCast(&tiles_01);
    tile_maps[1][1].tiles = @ptrCast(&tiles_11);

    var world = std.mem.zeroInit(World, .{});
    world.tile_maps = @ptrCast(&tile_maps);
    world.tile_map_count_x = 2;
    world.tile_map_count_y = 2;
    world.count_x = TILE_MAP_COUNT_X;
    world.count_y = TILE_MAP_COUNT_Y;
    // TODO: Begin using tile side in meters
    world.tile_side_in_meters = 1.4;
    world.tile_side_in_pixels = 60;
    world.meters_to_pixels =
        @as(f32, @floatFromInt(world.tile_side_in_pixels)) /
        world.tile_side_in_meters;
    world.upper_left_x = @floatFromInt(@divFloor(-world.tile_side_in_pixels, 2));
    world.upper_left_y = 0.0;

    //const player_width = 0.75 * @as(f32, @floatFromInt(world.tile_side_in_pixels));
    //const player_height = world.tile_side_in_pixels;
    const player_height = 1.4;
    const player_width = 0.75 * player_height;

    const game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    if (!memory.is_initialized) {
        // TODO: This may be more appropriate to do in the platform layer
        game_state.player_p.tile_map_x = 0;
        game_state.player_p.tile_map_y = 0;
        game_state.player_p.tile_x = 3;
        game_state.player_p.tile_y = 3;
        game_state.player_p.tile_rel_x = 5.0;
        game_state.player_p.tile_rel_y = 5.0;

        memory.is_initialized = true;
    }

    const tile_map = get_tile_map(
        &world,
        game_state.player_p.tile_map_x,
        game_state.player_p.tile_map_y,
    );

    assert(tile_map != null);

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
                d_player_y = -1.0;
            }

            if (controller.buttons.map.move_down.ended_down) {
                d_player_y = 1.0;
            }

            if (controller.buttons.map.move_left.ended_down) {
                d_player_x = -1.0;
            }

            if (controller.buttons.map.move_right.ended_down) {
                d_player_x = 1.0;
            }

            d_player_x *= 2.0;
            d_player_y *= 2.0;

            // TODO: Diagonal will be faster. Fix once we have vectors!
            var new_player_p = game_state.player_p;
            new_player_p.tile_rel_x += input.dt_for_frame * d_player_x;
            new_player_p.tile_rel_y += input.dt_for_frame * d_player_y;
            new_player_p = recanonicalize_position(&world, new_player_p);
            // TODO: delta function to auto-recanonicalize

            var player_left = new_player_p;
            player_left.tile_rel_x -= 0.5 * player_width;
            player_left = recanonicalize_position(&world, player_left);

            var player_right = new_player_p;
            player_right.tile_rel_x += 0.5 * player_width;
            player_right = recanonicalize_position(&world, player_right);

            if (is_world_point_empty(&world, new_player_p) and
                is_world_point_empty(&world, player_right) and
                is_world_point_empty(&world, player_left))
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

    for (0..world.count_y) |row| {
        for (0..world.count_x) |column| {
            const tile_id = get_tile_value_unchecked(&world, tile_map, column, row);
            var color: f32 = 0.5;

            if (tile_id == 1) {
                color = 1.0;
            }

            if ((column == game_state.player_p.tile_x) and
                (row == game_state.player_p.tile_y))
            {
                color = 0.0;
            }

            const min_x =
                world.upper_left_x +
                @as(f32, @floatFromInt(column *
                @as(usize, @intCast(world.tile_side_in_pixels))));

            const min_y =
                world.upper_left_y +
                @as(f32, @floatFromInt(row *
                @as(usize, @intCast(world.tile_side_in_pixels))));

            const max_x = min_x + @as(f32, @floatFromInt(world.tile_side_in_pixels));
            const max_y = min_y + @as(f32, @floatFromInt(world.tile_side_in_pixels));

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
    const player_left = world.upper_left_x +
        @as(f32, @floatFromInt(world.tile_side_in_pixels * game_state.player_p.tile_x)) +
        world.meters_to_pixels * game_state.player_p.tile_rel_x -
        0.5 * world.meters_to_pixels * player_width;
    const player_top = world.upper_left_y +
        @as(f32, @floatFromInt(world.tile_side_in_pixels * game_state.player_p.tile_y)) +
        world.meters_to_pixels * game_state.player_p.tile_rel_y -
        world.meters_to_pixels * player_height;

    try draw_rectangle(
        buffer,
        player_left,
        player_top,
        player_left + world.meters_to_pixels * player_width,
        player_top + world.meters_to_pixels * player_height,
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
