const std = @import("std");
const assert = std.debug.assert;
const platform = @import("zigmade_platform");
const tiling = @import("zigmade_tile.zig");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;

const GameState = struct {
    player_p: tiling.TileMapPosition,
    pixel_pointer: [*]u32,
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

var rand = std.rand.DefaultPrng.init(4096);

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
    f_min_x: f64,
    f_min_y: f64,
    f_max_x: f64,
    f_max_y: f64,
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

const BitmapHeader = packed struct {
    file_type: u16,
    file_size: u32,
    reserved_1: u16,
    reserved_2: u16,
    bitmap_offset: u32,
    size: u32,
    width: i32,
    height: i32,
    planes: u16,
    bits_per_pixel: u16,
};

fn debug_load_bmp(
    thread: *platform.ThreadContext,
    read_entire_file: platform.debug_platform_read_entire_file,
    file_name: [*:0]const u8,
) [*]u32 {
    var result: [*]u32 = undefined;
    const read_result = read_entire_file(thread, file_name);

    if (read_result.size != 0) {
        const header: *BitmapHeader = @alignCast(@ptrCast(read_result.contents));
        const pixels =
            @as([*]u32, @alignCast(@ptrCast(read_result.contents))) +
            header.bitmap_offset;

        result = pixels;
    }

    return result;
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
    size: usize,
) [*]u8 {
    assert((arena.used + size) <= arena.size);
    const result = arena.base + arena.used;
    arena.used += size;
    return result;
}

fn push_struct(
    arena: *MemoryArena,
    comptime T: type,
) *T {
    const result = push_size(arena, @sizeOf(T));
    return @as(*T, @alignCast(@ptrCast(result)));
}

pub fn push_array(
    arena: *MemoryArena,
    count: usize,
    comptime T: type,
) [*]T {
    const result = push_size(arena, count * @sizeOf(T));
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
        game_state.pixel_pointer = debug_load_bmp(
            thread,
            memory.debug_platform_read_entire_file,
            "assets/test_background.bmp",
        );

        game_state.player_p.abs_tile_x = 1;
        game_state.player_p.abs_tile_y = 3;
        game_state.player_p.offset_x = 5.0;
        game_state.player_p.offset_y = 5.0;

        // TODO: Can we just use Zig's own arena allocator?
        initialize_arena(
            &game_state.world_arena,
            memory.permanent_storage_size - @sizeOf(GameState),
            memory.permanent_storage + @sizeOf(GameState),
        );

        game_state.world = push_struct(&game_state.world_arena, World);
        var world = game_state.world.?;
        world.tile_map = push_struct(&game_state.world_arena, tiling.TileMap);

        var tile_map = world.tile_map;

        tile_map.chunk_shift = 4;
        tile_map.chunk_mask = (@as(u32, @intCast(1)) <<
            @as(u5, @intCast(tile_map.chunk_shift))) - 1;
        tile_map.chunk_dim = (@as(u32, @intCast(1)) <<
            @as(u5, @intCast(tile_map.chunk_shift)));
        tile_map.tile_chunk_count_x = 128;
        tile_map.tile_chunk_count_y = 128;
        tile_map.tile_chunk_count_z = 2;

        tile_map.tile_chunks = push_array(
            &game_state.world_arena,
            tile_map.tile_chunk_count_x *
                tile_map.tile_chunk_count_y *
                tile_map.tile_chunk_count_z,
            tiling.TileChunk,
        );

        tile_map.tile_side_in_meters = 1.4;

        const tiles_per_width = 17;
        const tiles_per_height = 9;

        var screen_x: usize = 0;
        var screen_y: usize = 0;
        var abs_tile_z: usize = 0;

        // TODO: Replace with real world generation
        var door_left = false;
        var door_right = false;
        var door_top = false;
        var door_bottom = false;
        var door_up = false;
        var door_down = false;

        for (0..100) |_| {
            var random_choice: usize = undefined;

            if (door_up or door_down) {
                random_choice = rand.random().int(usize) % 2;
            } else {
                random_choice = rand.random().int(usize) % 3;
            }

            var created_z_door = false;

            if (random_choice == 2) {
                created_z_door = true;

                if (abs_tile_z == 0) {
                    door_up = true;
                } else {
                    door_down = true;
                }
            } else if (random_choice == 1) {
                door_right = true;
            } else {
                door_top = true;
            }

            for (0..tiles_per_height) |tile_y| {
                for (0..tiles_per_width) |tile_x| {
                    const abs_tile_x = screen_x * tiles_per_width + tile_x;
                    const abs_tile_y = screen_y * tiles_per_height + tile_y;

                    var tile_value: usize = 1;

                    if (tile_x == 0 and (!door_left or (tile_y != tiles_per_height / 2))) {
                        tile_value = 2;
                    }

                    if (tile_x == (tiles_per_width - 1) and
                        (!door_right or (tile_y != tiles_per_height / 2)))
                    {
                        tile_value = 2;
                    }

                    if (tile_y == 0 and (!door_bottom or (tile_x != tiles_per_width / 2))) {
                        tile_value = 2;
                    }

                    if (tile_y == (tiles_per_height - 1) and
                        (!door_top or tile_x != (tiles_per_width / 2)))
                    {
                        tile_value = 2;
                    }

                    if (tile_x == 10 and tile_y == 6) {
                        if (door_up) {
                            tile_value = 3;
                        }

                        if (door_down) {
                            tile_value = 4;
                        }
                    }

                    tiling.set_tile_value(
                        &game_state.world_arena,
                        tile_map,
                        abs_tile_x,
                        abs_tile_y,
                        abs_tile_z,
                        tile_value,
                    );
                }
            }

            door_left = door_right;
            door_bottom = door_top;

            if (created_z_door) {
                door_down = !door_down;
                door_up = !door_up;
            } else {
                door_up = false;
                door_down = false;
            }

            door_right = false;
            door_top = false;

            if (random_choice == 2) {
                if (abs_tile_z == 0)
                    abs_tile_z = 1
                else
                    abs_tile_z = 0;
            } else if (random_choice == 1) {
                screen_x += 1;
            } else {
                screen_y += 1;
            }
        }

        memory.is_initialized = true;
    }

    const world = game_state.world.?;
    const tile_map = world.tile_map;

    const tile_side_in_pixels: i32 = 60;
    const meters_to_pixels: f64 =
        @as(f64, @floatFromInt(tile_side_in_pixels)) /
        tile_map.tile_side_in_meters;

    const lower_left_x: f32 = @floatFromInt(@divFloor(-tile_side_in_pixels, 2));
    _ = lower_left_x;
    const lower_left_y: f32 = @floatFromInt(buffer.height);
    _ = lower_left_y;

    for (0..input.controllers.len) |controller_index| {
        const controller: *platform.GameControllerInput =
            try platform.get_controller(input, controller_index);

        if (controller.is_analog) {
            // NOTE: Use analog movement tuning
        } else {
            // NOTE: Use digital movement tuning
            var d_player_x: f64 = 0.0; //pixels/s
            var d_player_y: f64 = 0.0; //pixels/s

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
            new_player_p.offset_x += input.dt_for_frame * d_player_x;
            new_player_p.offset_y += input.dt_for_frame * d_player_y;
            new_player_p = tiling.recanonicalize_position(tile_map, new_player_p);
            //// TODO: delta function to auto-recanonicalize

            var player_left = new_player_p;
            player_left.offset_x -= 0.5 * player_width;
            player_left = tiling.recanonicalize_position(tile_map, player_left);

            var player_right = new_player_p;
            player_right.offset_x += 0.5 * player_width;
            player_right = tiling.recanonicalize_position(tile_map, player_right);

            if (tiling.is_tile_map_point_empty(tile_map, new_player_p) and
                tiling.is_tile_map_point_empty(tile_map, player_right) and
                tiling.is_tile_map_point_empty(tile_map, player_left))
            {
                if (!tiling.on_same_tile(&game_state.player_p, &new_player_p)) {
                    const new_tile_value = tiling.get_tile_value_from_pos(
                        tile_map,
                        new_player_p,
                    );

                    if (new_tile_value == 3) {
                        new_player_p.abs_tile_z += 1;
                    } else if (new_tile_value == 4) {
                        new_player_p.abs_tile_z -= 1;
                    }
                }

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
                column,
                row,
                game_state.player_p.abs_tile_z,
            );

            if (tile_id > 0) {
                var color: f32 = 0.5;

                if (tile_id == 2) color = 1.0;

                if (tile_id > 2) color = 0.25;

                if ((column == game_state.player_p.abs_tile_x) and
                    (row == game_state.player_p.abs_tile_y))
                {
                    color = 0.0;
                }

                const cen_x = screen_center_x -
                    meters_to_pixels * game_state.player_p.offset_x +
                    @as(f32, @floatFromInt(rel_column * tile_side_in_pixels));

                const cen_y = screen_center_y +
                    meters_to_pixels * game_state.player_p.offset_y -
                    @as(f32, @floatFromInt(rel_row * tile_side_in_pixels));

                const min_x = cen_x - 0.5 * @as(f32, @floatFromInt(tile_side_in_pixels));
                const min_y = cen_y - 0.5 * @as(f32, @floatFromInt(tile_side_in_pixels));
                const max_x = cen_x + 0.5 * @as(f32, @floatFromInt(tile_side_in_pixels));
                const max_y = cen_y + 0.5 * @as(f32, @floatFromInt(tile_side_in_pixels));

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
    }

    const player_r = 1.0;
    const player_g = 1.0;
    const player_b = 0.0;
    const player_left = screen_center_x - 0.5 * meters_to_pixels * player_width;
    const player_top = screen_center_y - meters_to_pixels * player_height;

    try draw_rectangle(
        buffer,
        player_left,
        player_top,
        player_left + meters_to_pixels * player_width,
        player_top + meters_to_pixels * player_height,
        player_r,
        player_g,
        player_b,
    );

    if (false) {
        var source = game_state.pixel_pointer;
        var dest: [*]u32 = @alignCast(@ptrCast(buffer.memory));
        for (0..@intCast(buffer.height)) |y| {
            _ = y;
            for (0..@intCast(buffer.width)) |x| {
                _ = x;
                dest[0] = source[0];
                dest += 1;
                source += 1;
            }
        }
    }
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
