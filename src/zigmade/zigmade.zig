const std = @import("std");
const assert = std.debug.assert;
const platform = @import("zigmade_platform");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;

const GameState = struct {
    player_x: f32,
    player_y: f32,
};

const TILE_MAP_COUNT_X = 17;
const TILE_MAP_COUNT_Y = 9;

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

const TileMap = struct {
    count_x: usize,
    count_y: usize,
    upper_left_x: f32,
    upper_left_y: f32,
    tile_width: f32,
    tile_height: f32,
    tiles: ?[*]u32 = undefined,
};

const World = struct {
    // TODO: Beginner's sparseness
    tile_maps: ?[*]TileMap = undefined,
    tile_map_count_x: i32,
    tile_map_count_y: i32,
};

inline fn get_tile_map(
    world: *World,
    tile_map_x: usize,
    tile_map_y: usize,
) *TileMap {
    const tile_map: ?*TileMap = null;

    if (tile_map_x >= 0 and
        tile_map_x < world.tile_map_count_x and
        tile_map_y >= 0 and
        tile_map_y < world.tile_map_count_y)
    {
        const tile_map_index =
            tile_map_y *
            world.tile_map_count_x +
            tile_map_x;

        tile_map = &world.tile_maps.?[tile_map_index];
    }
    return tile_map;
}

inline fn get_tile_value_unchecked(
    tile_map: *TileMap,
    tile_x: usize,
    tile_y: usize,
) u32 {
    const tile_index = tile_y * tile_map.count_x + tile_x;
    const tile_map_value = tile_map.tiles.?[tile_index];
    return tile_map_value;
}

inline fn is_tile_map_point_empty(
    tile_map: *TileMap,
    test_x: f32,
    test_y: f32,
) bool {
    var empty = false;

    const player_tile_x: usize = @truncate(
        @as(usize, @intFromFloat((test_x -
            tile_map.upper_left_x) /
            tile_map.tile_width)),
    );
    const player_tile_y: usize = @truncate(
        @as(usize, @intFromFloat((test_y -
            tile_map.upper_left_y) /
            tile_map.tile_height)),
    );

    if (player_tile_x >= 0 and
        player_tile_x < tile_map.count_x and
        player_tile_y >= 0 and
        player_tile_y < tile_map.count_y)
    {
        const tile_map_value = get_tile_value_unchecked(
            tile_map,
            player_tile_x,
            player_tile_y,
        );

        empty = tile_map_value == 0;
    }

    return empty;
}

inline fn is_world_map_point_empty(
    world: *World,
    tile_map_x: usize,
    tile_map_y: usize,
    test_x: f32,
    test_y: f32,
) bool {
    var empty = false;

    const tile_map = get_tile_map(world, tile_map_x, tile_map_y);

    if (tile_map != null) {
        const player_tile_x: usize = @truncate(
            @as(usize, @intFromFloat((test_x -
                tile_map.upper_left_x) /
                tile_map.tile_width)),
        );
        const player_tile_y: usize = @truncate(
            @as(usize, @intFromFloat((test_y -
                tile_map.upper_left_y) /
                tile_map.tile_height)),
        );

        if (player_tile_x >= 0 and
            player_tile_x < tile_map.count_x and
            player_tile_y >= 0 and
            player_tile_y < tile_map.count_y)
        {
            const tile_map_value = get_tile_value_unchecked(
                tile_map,
                player_tile_x,
                player_tile_y,
            );

            empty = tile_map_value == 0;
        }
    }

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
        [_]u32{ 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1 },
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
        .count_x = TILE_MAP_COUNT_X,
        .count_y = TILE_MAP_COUNT_Y,
        .upper_left_x = -30.0,
        .upper_left_y = 0.0,
        .tile_width = 60.0,
        .tile_height = 60.0,
        .tiles = @ptrCast(&tiles_00),
    };

    tile_maps[0][1] = tile_maps[0][0];
    tile_maps[0][1].tiles = @ptrCast(&tiles_01);
    tile_maps[1][0] = tile_maps[0][0];
    tile_maps[1][0].tiles = @ptrCast(&tiles_10);
    tile_maps[1][1] = tile_maps[0][0];
    tile_maps[1][1].tiles = @ptrCast(&tiles_11);

    var tile_map = tile_maps[0][0];

    const world = World{
        .tile_maps = @ptrCast(&tile_maps),
        .tile_map_count_x = 2,
        .tile_map_count_y = 2,
    };
    _ = world;

    const player_width = 0.75 * tile_map.tile_width;
    const player_height = tile_map.tile_height;

    const game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    if (!memory.is_initialized) {
        // TODO: This may be more appropriate to do in the platform layer
        game_state.player_x = 150.0;
        game_state.player_y = 150.0;

        memory.is_initialized = true;
    }

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

            if (is_tile_map_point_empty(&tile_map, new_player_x - 0.5 * player_width, new_player_y) and
                is_tile_map_point_empty(&tile_map, new_player_x + 0.5 * player_width, new_player_y) and
                is_tile_map_point_empty(&tile_map, new_player_x, new_player_y))
            {
                game_state.player_x = new_player_x;
                game_state.player_y = new_player_y;
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

    for (0..tile_map.count_y) |row| {
        for (0..tile_map.count_x) |column| {
            const tile_id = get_tile_value_unchecked(&tile_map, column, row);
            var color: f32 = 0.5;

            if (tile_id == 1) {
                color = 1.0;
            }

            const min_x =
                tile_map.upper_left_x +
                @as(f32, @floatFromInt(column)) *
                tile_map.tile_width;

            const min_y =
                tile_map.upper_left_y +
                @as(f32, @floatFromInt(row)) *
                tile_map.tile_height;

            const max_x = min_x + tile_map.tile_width;
            const max_y = min_y + tile_map.tile_height;

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
    const player_top = game_state.player_y - player_height;

    try draw_rectangle(
        buffer,
        player_left,
        player_top,
        player_left + player_width,
        player_top + player_height,
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
