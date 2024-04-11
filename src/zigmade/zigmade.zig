const std = @import("std");
const assert = std.debug.assert;
const platform = @import("zigmade_platform");
const tile = @import("zigmade_tile.zig");
const math = @import("zigmade_math.zig");
const Vec2 = math.Vec2;
const intrinsics = @import("zigmade_intrinsics.zig");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;

const HeroBitmaps = struct {
    head: Bitmap,
    cape: Bitmap,
    torso: Bitmap,
    align_x: i32,
    align_y: i32,
};

const GameState = struct {
    world: ?*World = null,
    world_arena: MemoryArena,
    camera_p: tile.TileMapPosition,
    player_p: tile.TileMapPosition,
    d_player_p: Vec2,
    backdrop: Bitmap,
    hero_direction: usize = 0,
    hero_bitmaps: [4]HeroBitmaps,
};

const World = struct {
    tile_map: *tile.TileMap,
};

pub const MemoryArena = struct {
    size: usize,
    base: [*]u8,
    used: usize,
};

const Bitmap = struct {
    width: usize,
    height: usize,
    content: extern union {
        bytes: [*]u8,
        pixels: [*]u32,
    },
};

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
    compression: u32,
    size_of_bitmap: u32,
    horz_resolution: i32,
    vert_resolution: i32,
    colors_used: u32,
    colors_important: u32,
    red_mask: u32,
    green_mask: u32,
    blue_mask: u32,
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
    min: Vec2,
    max: Vec2,
    r: f32,
    g: f32,
    b: f32,
) void {
    var min_x: i64 = @intFromFloat(@round(min.x));
    var min_y: i64 = @intFromFloat(@round(min.y));
    var max_x: i64 = @intFromFloat(@round(max.x));
    var max_y: i64 = @intFromFloat(@round(max.y));

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
        @as(usize, @bitCast(min_y *% buffer.pitch));

    for (@intCast(min_y)..@intCast(max_y)) |_| {
        var pixel: [*]u32 = @alignCast(@ptrCast(row));

        for (@intCast(min_x)..@intCast(max_x)) |_| {
            pixel[0] = color;
            pixel += 1;
        }

        row += @as(usize, @intCast(buffer.pitch));
    }
}

fn draw_bitmap(
    buffer: *platform.GameOffscreenBuffer,
    bitmap: *Bitmap,
    unaligned_real_x: f64,
    unaligned_real_y: f64,
    align_x: i64,
    align_y: i64,
) void {
    const real_x = unaligned_real_x - @as(f64, @floatFromInt(align_x));
    const real_y = unaligned_real_y - @as(f64, @floatFromInt(align_y));
    var min_x: i64 = @intFromFloat(@round(real_x));
    var min_y: i64 = @intFromFloat(@round(real_y));
    var max_x: i64 = @intFromFloat(@round(real_x + @as(f64, @floatFromInt(bitmap.width))));
    var max_y: i64 = @intFromFloat(@round(real_y + @as(f64, @floatFromInt(bitmap.height))));

    var source_offset_x: i64 = 0;
    if (min_x < 0) {
        source_offset_x = -min_x;
        min_x = 0;
    }

    var source_offset_y: i64 = 0;
    if (min_y < 0) {
        source_offset_y = -min_y;
        min_y = 0;
    }

    if (max_x > buffer.width) max_x = @intCast(buffer.width);
    if (max_y > buffer.height) max_y = @intCast(buffer.height);
    if (min_x > max_x) max_x = min_x;
    if (min_y > max_y) max_y = min_y;

    var source_row = bitmap.content.pixels +
        bitmap.width * (bitmap.height - 1) -
        (bitmap.width * @as(usize, @intCast(source_offset_y))) +
        @as(usize, @intCast(source_offset_x));

    var dest_row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
        (@as(usize, @intCast(min_x)) *
        @as(usize, @intCast(buffer.bytes_per_pixel))) +
        @as(usize, @bitCast(min_y *% buffer.pitch));

    for (@intCast(min_y)..@intCast(max_y)) |_| {
        var dest: [*]u32 = @alignCast(@ptrCast(dest_row));
        var source = source_row;

        for (@intCast(min_x)..@intCast(max_x)) |_| {
            const a = @as(f32, @floatFromInt(((source[0] >> 24) & 0xFF))) / 255.0;
            const sr: f32 = @floatFromInt((source[0] >> 16) & 0xFF);
            const sg: f32 = @floatFromInt((source[0] >> 8) & 0xFF);
            const sb: f32 = @floatFromInt((source[0] >> 0) & 0xFF);

            const dr: f32 = @floatFromInt((dest[0] >> 16) & 0xFF);
            const dg: f32 = @floatFromInt((dest[0] >> 8) & 0xFF);
            const db: f32 = @floatFromInt((dest[0] >> 0) & 0xFF);

            // TODO: Someday, we need to talk about premultiplied alpha!
            // which this is not
            const r = (1.0 - a) * dr + a * sr;
            const g = (1.0 - a) * dg + a * sg;
            const b = (1.0 - a) * db + a * sb;

            dest[0] = (@as(u32, @intFromFloat(r + 0.5)) << 16) |
                (@as(u32, @intFromFloat(g + 0.5)) << 8) |
                (@as(u32, @intFromFloat(b + 0.5)) << 0);

            dest += 1;
            source += 1;
        }

        dest_row += @as(usize, @intCast(buffer.pitch));
        source_row -= bitmap.width;
    }
}

fn debug_load_bmp(
    thread: *platform.ThreadContext,
    read_entire_file: platform.debug_platform_read_entire_file,
    file_name: [*:0]const u8,
) Bitmap {
    var result: Bitmap = undefined;

    const read_result = read_entire_file(thread, file_name);

    if (read_result.size != 0) {
        const header: *BitmapHeader = @alignCast(@ptrCast(read_result.contents));
        const bytes: [*]u8 =
            @as([*]u8, @ptrCast(read_result.contents)) +
            header.bitmap_offset;

        result.content.bytes = bytes;
        result.width = @intCast(header.width);
        result.height = @intCast(header.height);

        assert(header.compression == 3);

        // NOTE: If using this generically, remember that BMP
        // files can go in either direction and height will be
        // negative for top-down
        // Also, there can be compression, etc. this is not complete
        // BMP loading code
        //
        // NOTE: Byte order memory is determined by the header itself,
        // so we have to read out the masks and convert pixels ourselves

        var source_dest = result.content.pixels;

        const red_mask = header.red_mask;
        const green_mask = header.green_mask;
        const blue_mask = header.blue_mask;
        const alpha_mask = ~(red_mask | green_mask | blue_mask);

        const red_shift = intrinsics.find_least_sig_set_bit(red_mask);
        const green_shift = intrinsics.find_least_sig_set_bit(green_mask);
        const blue_shift = intrinsics.find_least_sig_set_bit(blue_mask);
        const alpha_shift = intrinsics.find_least_sig_set_bit(alpha_mask);

        assert(red_shift.found);
        assert(green_shift.found);
        assert(blue_shift.found);
        assert(alpha_shift.found);

        for (0..@intCast(header.height)) |_| {
            for (0..@intCast(header.width)) |_| {
                const coefficient = source_dest[0];
                source_dest[0] =
                    (((coefficient >> @as(u5, @intCast(alpha_shift.index))) & 0xFF) << 24) |
                    (((coefficient >> @as(u5, @intCast(red_shift.index))) & 0xFF) << 16) |
                    (((coefficient >> @as(u5, @intCast(green_shift.index))) & 0xFF) << 8) |
                    (((coefficient >> @as(u5, @intCast(blue_shift.index))) & 0xFF) << 0);
                source_dest += 1;
            }
        }
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
        game_state.backdrop = debug_load_bmp(
            thread,
            memory.debug_platform_read_entire_file,
            "data/test/test_background.bmp",
        );

        var bitmaps = &game_state.hero_bitmaps;
        bitmaps[0].head = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_right_head.bmp");
        bitmaps[0].cape = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_right_cape.bmp");
        bitmaps[0].torso = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_right_torso.bmp");
        bitmaps[0].align_x = 72;
        bitmaps[0].align_y = 182;

        bitmaps[1].head = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_back_head.bmp");
        bitmaps[1].cape = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_back_cape.bmp");
        bitmaps[1].torso = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_back_torso.bmp");
        bitmaps[1].align_x = 72;
        bitmaps[1].align_y = 182;

        bitmaps[2].head = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_left_head.bmp");
        bitmaps[2].cape = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_left_cape.bmp");
        bitmaps[2].torso = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_left_torso.bmp");
        bitmaps[2].align_x = 72;
        bitmaps[2].align_y = 182;

        bitmaps[3].head = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_front_head.bmp");
        bitmaps[3].cape = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_front_cape.bmp");
        bitmaps[3].torso = debug_load_bmp(thread, memory.debug_platform_read_entire_file, "data/test/test_hero_front_torso.bmp");
        bitmaps[3].align_x = 72;
        bitmaps[3].align_y = 182;

        game_state.camera_p.abs_tile_x = 17 / 2;
        game_state.camera_p.abs_tile_y = 9 / 2;

        game_state.player_p.abs_tile_x = 1;
        game_state.player_p.abs_tile_y = 3;
        game_state.player_p.offset.x = 5.0;
        game_state.player_p.offset.y = 5.0;

        // TODO: Can we just use Zig's own arena allocator?
        initialize_arena(
            &game_state.world_arena,
            memory.permanent_storage_size - @sizeOf(GameState),
            memory.permanent_storage + @sizeOf(GameState),
        );

        game_state.world = push_struct(&game_state.world_arena, World);
        var world = game_state.world.?;
        world.tile_map = push_struct(&game_state.world_arena, tile.TileMap);

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
            tile.TileChunk,
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

                    tile.set_tile_value(
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
            var dd_player = Vec2{};

            // TODO: Investigate timing tomorrow and verify that it
            // is working properly because it appears things are moving
            // half as fast as expected
            if (controller.buttons.map.move_up.ended_down) {
                game_state.hero_direction = 1;
                dd_player.y = 1.0;
            }

            if (controller.buttons.map.move_down.ended_down) {
                game_state.hero_direction = 3;
                dd_player.y = -1.0;
            }

            if (controller.buttons.map.move_left.ended_down) {
                game_state.hero_direction = 2;
                dd_player.x = -1.0;
            }

            if (controller.buttons.map.move_right.ended_down) {
                game_state.hero_direction = 0;
                dd_player.x = 1.0;
            }

            if (dd_player.x != 0 and dd_player.y != 0) {
                dd_player = math.scale(dd_player, 0.707106781187);
            }

            var player_speed: f32 = 10.0; // m/s^2

            if (controller.buttons.map.action_up.ended_down) {
                player_speed = 50.0; // m/s^2
            }

            // player's acceleration
            dd_player = math.scale(dd_player, player_speed);

            // TODO: ODE here
            dd_player = math.add(
                dd_player,
                math.scale(game_state.d_player_p, -1.5),
            );

            var new_player_p = game_state.player_p;
            // calculate new player position from current velocity
            new_player_p.offset = math.add(
                math.add(
                    math.scale(dd_player, 0.5 * math.square(input.dt_for_frame)),
                    math.scale(game_state.d_player_p, input.dt_for_frame),
                ),
                new_player_p.offset,
            );
            // calculate new player velocity
            game_state.d_player_p = math.add(
                math.scale(dd_player, input.dt_for_frame),
                game_state.d_player_p,
            );

            new_player_p = tile.recanonicalize_position(tile_map, new_player_p);
            //// TODO: delta function to auto-recanonicalize

            var player_left = new_player_p;
            player_left.offset.x -= 0.5 * player_width;
            player_left = tile.recanonicalize_position(tile_map, player_left);

            var player_right = new_player_p;
            player_right.offset.x += 0.5 * player_width;
            player_right = tile.recanonicalize_position(tile_map, player_right);

            if (tile.is_tile_map_point_empty(tile_map, new_player_p) and
                tile.is_tile_map_point_empty(tile_map, player_right) and
                tile.is_tile_map_point_empty(tile_map, player_left))
            {
                if (!tile.on_same_tile(&game_state.player_p, &new_player_p)) {
                    const new_tile_value = tile.get_tile_value_from_pos(
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

            game_state.camera_p.abs_tile_z = game_state.player_p.abs_tile_z;

            const diff = tile.subtract(tile_map, game_state.player_p, game_state.camera_p);

            if (diff.dxy.x > (9.0 * tile_map.tile_side_in_meters)) {
                game_state.camera_p.abs_tile_x += 17;
            }

            if (diff.dxy.x < -(9.0 * tile_map.tile_side_in_meters)) {
                game_state.camera_p.abs_tile_x -= 17;
            }

            if (diff.dxy.y > (5.0 * tile_map.tile_side_in_meters)) {
                game_state.camera_p.abs_tile_y += 9;
            }

            if (diff.dxy.y < -(5.0 * tile_map.tile_side_in_meters)) {
                game_state.camera_p.abs_tile_y -= 9;
            }
        }
    }

    draw_bitmap(buffer, &game_state.backdrop, 0.0, 0.0, 0.0, 0.0);

    const screen_center_x = 0.5 * @as(f32, @floatFromInt(buffer.width));
    const screen_center_y = 0.5 * @as(f32, @floatFromInt(buffer.height));

    var rel_row: i32 = -10;

    while (rel_row < 10) : (rel_row += 1) {
        var rel_column: i32 = -20;

        while (rel_column < 20) : (rel_column += 1) {
            const column = @as(usize, @bitCast(
                @as(isize, @bitCast(game_state.camera_p.abs_tile_x)) +
                    rel_column,
            ));

            const row = @as(usize, @bitCast(
                @as(isize, @bitCast(game_state.camera_p.abs_tile_y)) +
                    rel_row,
            ));

            const tile_id = tile.get_tile_value(
                tile_map,
                column,
                row,
                game_state.camera_p.abs_tile_z,
            );

            if (tile_id > 1) {
                var color: f32 = 0.5;

                if (tile_id == 2) color = 1.0;

                if (tile_id > 2) color = 0.25;

                if ((column == game_state.camera_p.abs_tile_x) and
                    (row == game_state.camera_p.abs_tile_y))
                {
                    color = 0.0;
                }

                const tile_side = Vec2{
                    .x = 0.5 * @as(f32, @floatFromInt(tile_side_in_pixels)),
                    .y = 0.5 * @as(f32, @floatFromInt(tile_side_in_pixels)),
                };

                const cen = Vec2{
                    .x = screen_center_x -
                        meters_to_pixels * game_state.camera_p.offset.x +
                        @as(f32, @floatFromInt(rel_column * tile_side_in_pixels)),
                    .y = screen_center_y +
                        meters_to_pixels * game_state.camera_p.offset.y -
                        @as(f32, @floatFromInt(rel_row * tile_side_in_pixels)),
                };

                const min = math.sub(cen, tile_side);
                const max = math.add(cen, tile_side);

                draw_rectangle(
                    buffer,
                    min,
                    max,
                    color,
                    color,
                    color,
                );
            }
        }
    }

    const diff = tile.subtract(tile_map, game_state.player_p, game_state.camera_p);

    const player_r = 1.0;
    const player_g = 1.0;
    const player_b = 0.0;
    const player_ground_point_x = screen_center_x + meters_to_pixels * diff.dxy.x;
    const player_ground_point_y = screen_center_y - meters_to_pixels * diff.dxy.y;
    const player_origin = Vec2{
        .x = player_ground_point_x - 0.5 * meters_to_pixels * player_width,
        .y = player_ground_point_y - meters_to_pixels * player_height,
    };
    const player_dimensions = math.scale(
        Vec2{ .x = player_width, .y = player_height },
        meters_to_pixels,
    );

    draw_rectangle(
        buffer,
        player_origin,
        math.add(player_origin, player_dimensions),
        player_r,
        player_g,
        player_b,
    );

    var hero_bitmaps = game_state.hero_bitmaps[game_state.hero_direction];

    draw_bitmap(
        buffer,
        &hero_bitmaps.torso,
        player_ground_point_x,
        player_ground_point_y,
        hero_bitmaps.align_x,
        hero_bitmaps.align_y,
    );

    draw_bitmap(
        buffer,
        &hero_bitmaps.cape,
        player_ground_point_x,
        player_ground_point_y,
        hero_bitmaps.align_x,
        hero_bitmaps.align_y,
    );

    draw_bitmap(
        buffer,
        &hero_bitmaps.head,
        player_ground_point_x,
        player_ground_point_y,
        hero_bitmaps.align_x,
        hero_bitmaps.align_y,
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
