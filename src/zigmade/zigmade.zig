const std = @import("std");
const assert = std.debug.assert;
const platform = @import("zigmade_platform");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;
const TILE_MAP_COUNT_X = 17;
const TILE_MAP_COUNT_Y = 9;

const GameState = struct {
    player_x: f32,
    player_y: f32,
    // TODO: Should player state be canonical now?
    player_tile_map_x: usize,
    player_tile_map_y: usize,
};

const CanonicalPosition = struct {
    // TODO: Take tile map x and y and tile x and y
    // then pack them into single 32-bit values for x and y
    // where there is some low bits for the tile index
    // and the high bits are for the tile page
    tile_map_x: usize,
    tile_map_y: usize,
    tile_x: i32,
    tile_y: i32,
    // Convert these to math-friendly, resolution independent
    // representations of world units relative to a tile
    tile_rel_x: f32,
    tile_rel_y: f32,
};

// TODO: Is this necessary?
const RawPosition = struct {
    tile_map_x: usize,
    tile_map_y: usize,
    // NOTE: Tilemap-relative x and y
    x: f32,
    y: f32,
};

const TileMap = struct {
    tiles: ?[*]u32 = undefined,
};

const World = struct {
    tile_side_in_meters: f32,
    tile_side_in_pixels: i32,
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

inline fn get_canonical_position(
    world: *World,
    pos: RawPosition,
) CanonicalPosition {
    var result = std.mem.zeroInit(CanonicalPosition, .{});

    result.tile_map_x = pos.tile_map_x;
    result.tile_map_y = pos.tile_map_y;

    const x = pos.x - world.upper_left_x;
    const y = pos.y - world.upper_left_y;
    const f_tile_side_in_pixels = @as(f32, @floatFromInt(world.tile_side_in_pixels));

    result.tile_x = @as(i32, @intFromFloat(@divFloor(x, f_tile_side_in_pixels)));
    result.tile_y = @as(i32, @intFromFloat(@divFloor(y, f_tile_side_in_pixels)));
    result.tile_rel_x = x - @as(f32, @floatFromInt(result.tile_x * world.tile_side_in_pixels));
    result.tile_rel_y = y - @as(f32, @floatFromInt(result.tile_y * world.tile_side_in_pixels));

    assert(result.tile_rel_x >= 0);
    assert(result.tile_rel_y >= 0);
    assert(result.tile_rel_x < @as(f32, @floatFromInt(world.tile_side_in_pixels)));
    assert(result.tile_rel_y < @as(f32, @floatFromInt(world.tile_side_in_pixels)));

    if (result.tile_x < 0) {
        result.tile_x = @as(i32, @intCast(world.count_x)) + result.tile_x;
        result.tile_map_x -= 1;
    }

    if (result.tile_y < 0) {
        result.tile_y = @as(i32, @intCast(world.count_y)) + result.tile_y;
        result.tile_map_y -= 1;
    }

    if (result.tile_x >= world.count_x) {
        result.tile_x = result.tile_x - @as(i32, @intCast(world.count_x));
        result.tile_map_x += 1;
    }

    if (result.tile_y >= world.count_y) {
        result.tile_y = result.tile_y - @as(i32, @intCast(world.count_y));
        result.tile_map_y += 1;
    }

    return result;
}

inline fn is_world_point_empty(
    world: *World,
    test_pos: RawPosition,
) bool {
    var empty = false;

    const can_pos = get_canonical_position(world, test_pos);
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
    world.upper_left_x = @floatFromInt(@divFloor(-world.tile_side_in_pixels, 2));
    world.upper_left_y = 0.0;

    const player_width = 0.75 * @as(f32, @floatFromInt(world.tile_side_in_pixels));
    const player_height = world.tile_side_in_pixels;

    const game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    if (!memory.is_initialized) {
        // TODO: This may be more appropriate to do in the platform layer
        game_state.player_x = 185.0;
        game_state.player_y = 150.0;

        memory.is_initialized = true;
    }

    const tile_map = get_tile_map(
        &world,
        game_state.player_tile_map_x,
        game_state.player_tile_map_y,
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

            d_player_x *= 64.0;
            d_player_y *= 64.0;

            // TODO: Diagonal will be faster. Fix once we have vectors!
            const new_player_x = game_state.player_x + input.dt_for_frame * d_player_x;
            const new_player_y = game_state.player_y + input.dt_for_frame * d_player_y;

            const player_pos = RawPosition{
                .tile_map_x = game_state.player_tile_map_x,
                .tile_map_y = game_state.player_tile_map_y,
                .x = new_player_x,
                .y = new_player_y,
            };
            var player_left = player_pos;
            player_left.x -= 0.5 * player_width;
            var player_right = player_pos;
            player_right.x += 0.5 * player_width;

            if (is_world_point_empty(&world, player_pos) and
                is_world_point_empty(&world, player_right) and
                is_world_point_empty(&world, player_left))
            {
                const can_pos = get_canonical_position(&world, player_pos);
                game_state.player_tile_map_x = can_pos.tile_map_x;
                game_state.player_tile_map_y = can_pos.tile_map_y;

                game_state.player_x = world.upper_left_x +
                    @as(f32, @floatFromInt(world.tile_side_in_pixels * can_pos.tile_x)) +
                    can_pos.tile_rel_x;
                game_state.player_y = world.upper_left_y +
                    @as(f32, @floatFromInt(world.tile_side_in_pixels * can_pos.tile_y)) +
                    can_pos.tile_rel_y;
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
    const player_left = game_state.player_x - 0.5 * player_width;
    const player_top = game_state.player_y - @as(f32, @floatFromInt(player_height));

    try draw_rectangle(
        buffer,
        player_left,
        player_top,
        player_left + player_width,
        player_top + @as(f32, @floatFromInt(player_height)),
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
