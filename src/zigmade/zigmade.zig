const std = @import("std");
const assert = std.debug.assert;
const rotl = std.math.rotl;
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

const Entity = struct {
    exists: bool,
    pos: tile.TileMapPosition,
    d_pos: Vec2,
    facing_direction: usize = 0,
    width: f64,
    height: f64,
};

const GameState = struct {
    world: ?*World = null,
    world_arena: MemoryArena,

    // TODO: Should we allow split-screen?
    camera_entity_index: usize,
    camera_p: tile.TileMapPosition,

    entity_count: usize,
    entities: [256]Entity,
    player_controller_index: [5]usize,
    backdrop: Bitmap,
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

        const red_scan = intrinsics.find_least_sig_set_bit(red_mask);
        const green_scan = intrinsics.find_least_sig_set_bit(green_mask);
        const blue_scan = intrinsics.find_least_sig_set_bit(blue_mask);
        const alpha_scan = intrinsics.find_least_sig_set_bit(alpha_mask);

        const red_shift = 16 - @as(i32, @intCast(red_scan.index));
        const green_shift = 8 - @as(i32, @intCast(green_scan.index));
        const blue_shift = 0 - @as(i32, @intCast(blue_scan.index));
        const alpha_shift = 24 - @as(i32, @intCast(alpha_scan.index));

        assert(red_scan.found);
        assert(green_scan.found);
        assert(blue_scan.found);
        assert(alpha_scan.found);

        for (0..@intCast(header.height)) |_| {
            for (0..@intCast(header.width)) |_| {
                const coefficient = source_dest[0];
                source_dest[0] =
                    (rotl(@TypeOf(coefficient), coefficient & red_mask, red_shift)) |
                    (rotl(@TypeOf(coefficient), coefficient & green_mask, green_shift)) |
                    (rotl(@TypeOf(coefficient), coefficient & blue_mask, blue_shift)) |
                    (rotl(@TypeOf(coefficient), coefficient & alpha_mask, alpha_shift));
                source_dest += 1;
            }
        }
    }

    return result;
}

fn closest_point_in_rect(
    min: Vec2,
    max: Vec2,
    pos: tile.TileMapPosition,
) tile.TileMapPosition {
    _ = min;
    _ = max;
    _ = pos;
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

fn test_wall(
    wall: f64,
    rel_x: f64,
    rel_y: f64,
    player_delta_x: f64,
    player_delta_y: f64,
    t_min: *f64,
    min_y: f64,
    max_y: f64,
) void {
    const t_epsilon = 0.0001;

    if (player_delta_x != 0) {
        const t_result = (wall - rel_x) / player_delta_x;
        const y = rel_y + t_result * player_delta_y;

        if (t_result >= 0.0 and t_min.* > t_result) {
            if (y >= min_y and y <= max_y) {
                t_min.* = @max(0.0, t_result - t_epsilon);
            }
        }
    }
}

fn move_player(
    game_state: *GameState,
    entity: *Entity,
    dt: f64,
    dd_pos: Vec2,
) void {
    const tile_map = game_state.world.?.tile_map;

    var acceleration = dd_pos;

    const acc_length = math.length_squared(acceleration);

    if (acc_length > 1.0) {
        acceleration = math.scale(acceleration, 1.0 / @sqrt(acc_length));
    }

    const player_speed: f32 = 50.0; // m/s^2
    acceleration = math.scale(acceleration, player_speed);

    // TODO: ODE here
    acceleration = math.add(
        acceleration,
        math.scale(entity.d_pos, -8.0),
    );

    var old_player_p = entity.pos;

    const player_delta = math.add(
        math.scale(acceleration, 0.5 * math.square(dt)),
        math.scale(entity.d_pos, dt),
    );

    entity.d_pos = math.add(
        math.scale(acceleration, dt),
        entity.d_pos,
    );

    const new_player_p = tile.offset_pos(tile_map, old_player_p, player_delta);

    if (false) {
        // TODO: delta function to auto-recanonicalize

        var player_left = new_player_p;
        player_left.offset_.x -= 0.5 * entity.width;
        player_left = tile.recanonicalize_position(tile_map, player_left);

        var player_right = new_player_p;
        player_right.offset_.x += 0.5 * entity.width;
        player_right = tile.recanonicalize_position(tile_map, player_right);

        var collided = false;
        var col_p: tile.TileMapPosition = undefined;

        if (!tile.is_tile_map_point_empty(tile_map, new_player_p)) {
            col_p = new_player_p;
            collided = true;
        }

        if (!tile.is_tile_map_point_empty(tile_map, player_right)) {
            col_p = player_right;
            collided = true;
        }

        if (!tile.is_tile_map_point_empty(tile_map, player_left)) {
            col_p = player_left;
            collided = true;
        }

        if (collided) {
            var r = Vec2{};

            if (col_p.abs_tile_x < entity.pos.abs_tile_x) {
                r = Vec2{ .x = 1 };
            }

            if (col_p.abs_tile_x > entity.pos.abs_tile_x) {
                r = Vec2{ .x = -1 };
            }

            if (col_p.abs_tile_y < entity.pos.abs_tile_y) {
                r = Vec2{ .y = 1 };
            }

            if (col_p.abs_tile_y > entity.pos.abs_tile_y) {
                r = Vec2{ .y = -1 };
            }

            entity.d_pos = math.sub(
                entity.d_pos,
                math.scale(
                    r,
                    1 * math.inner(entity.d_pos, r),
                ),
            );
        } else {
            entity.pos = new_player_p;
        }
    } else {
        //  const min_tile_x: usize = @min(old_player_p.abs_tile_x, new_player_p.abs_tile_x);
        //  const min_tile_y: usize = @min(old_player_p.abs_tile_y, new_player_p.abs_tile_y);
        //  const one_past_max_tile_x: usize = @max(old_player_p.abs_tile_x, new_player_p.abs_tile_x) + 1;
        //  const one_past_max_tile_y: usize = @max(old_player_p.abs_tile_y, new_player_p.abs_tile_y) + 1;

        const start_tile_x = old_player_p.abs_tile_x;
        const start_tile_y = old_player_p.abs_tile_y;
        const end_tile_x = new_player_p.abs_tile_x;
        const end_tile_y = new_player_p.abs_tile_y;
        const delta_x = intrinsics.sign_of(@bitCast(end_tile_x -% start_tile_x));
        const delta_y = intrinsics.sign_of(@bitCast(end_tile_y -% start_tile_y));

        const abs_tile_z = entity.pos.abs_tile_z;
        var t_min: f64 = 1.0;

        var abs_tile_y: i64 = @intCast(start_tile_y);
        while (true) : (abs_tile_y +%= delta_y) {
            var abs_tile_x: i64 = @intCast(start_tile_x);
            while (true) : (abs_tile_x +%= delta_x) {
                var test_tile_p = tile.centered_tile_point(
                    @intCast(abs_tile_x),
                    @intCast(abs_tile_y),
                    abs_tile_z,
                );

                const tile_value = tile.get_tile_value_from_pos(tile_map, test_tile_p);

                if (!tile.is_tile_value_empty(tile_value)) {
                    const min_corner = math.scale(Vec2{
                        .x = tile_map.tile_side_in_meters,
                        .y = tile_map.tile_side_in_meters,
                    }, -0.5);

                    const max_corner = math.scale(Vec2{
                        .x = tile_map.tile_side_in_meters,
                        .y = tile_map.tile_side_in_meters,
                    }, 0.5);

                    const rel_old_player_p = tile.subtract(tile_map, &old_player_p, &test_tile_p);
                    const rel = rel_old_player_p.dxy;

                    test_wall(min_corner.x, rel.x, rel.y, player_delta.x, player_delta.y, &t_min, min_corner.y, max_corner.y);
                    test_wall(max_corner.x, rel.x, rel.y, player_delta.x, player_delta.y, &t_min, min_corner.y, max_corner.y);
                    test_wall(min_corner.y, rel.y, rel.x, player_delta.y, player_delta.x, &t_min, min_corner.x, max_corner.x);
                    test_wall(max_corner.y, rel.y, rel.x, player_delta.y, player_delta.x, &t_min, min_corner.x, max_corner.x);
                }

                if (abs_tile_x == end_tile_x) break;
            }

            if (abs_tile_y == end_tile_y) break;
        }

        entity.pos = tile.offset_pos(
            tile_map,
            old_player_p,
            math.scale(player_delta, t_min),
        );
    }

    //
    // NOTE: Update camera/player Z based on last movement
    //
    if (!tile.on_same_tile(&old_player_p, &entity.pos)) {
        const new_tile_value = tile.get_tile_value_from_pos(
            tile_map,
            entity.pos,
        );

        if (new_tile_value == 3) {
            entity.pos.abs_tile_z += 1;
        } else if (new_tile_value == 4) {
            entity.pos.abs_tile_z -= 1;
        }
    }

    if (entity.d_pos.x == 0.0 and entity.d_pos.y == 0.0) {
        // Leave facing_direction alone
    } else if (@abs(entity.d_pos.x) > @abs(entity.d_pos.y)) {
        if (entity.d_pos.x > 0) {
            entity.facing_direction = 0;
        } else {
            entity.facing_direction = 2;
        }
    } else if (@abs(entity.d_pos.x) < @abs(entity.d_pos.y)) {
        if (entity.d_pos.y > 0) {
            entity.facing_direction = 1;
        } else {
            entity.facing_direction = 3;
        }
    }
}

inline fn get_entity(game_state: *GameState, index: usize) ?*Entity {
    var entity: ?*Entity = null;

    if (index > 0 and index < game_state.entities.len) {
        entity = &game_state.entities[index];
    }

    return entity;
}

fn add_entity(game_state: *GameState) usize {
    const entity_index = game_state.entity_count;
    game_state.entity_count += 1;

    assert(game_state.entity_count < game_state.entities.len);
    const entity = &game_state.entities[entity_index];
    entity.* = std.mem.zeroInit(Entity, .{});

    return entity_index;
}

fn initialize_player(game_state: *GameState, entity_index: usize) void {
    const entity = get_entity(game_state, entity_index);

    if (entity) |e| {
        e.exists = true;
        e.pos.abs_tile_x = 1;
        e.pos.abs_tile_y = 3;
        e.pos.offset_.x = 0.0;
        e.pos.offset_.y = 0.0;
        e.height = 1.4;
        e.width = 0.75 * e.height;
    }

    if (get_entity(game_state, game_state.camera_entity_index) == null) {
        game_state.camera_entity_index = entity_index;
    }
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

    var game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    if (!memory.is_initialized) {
        // NOTE: Reserve entity slot 0 as the null entity
        _ = add_entity(game_state);

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

        // TODO: Waiting for full sparseness
        //var screen_x: usize = std.math.maxInt(i32) / 2;
        //var screen_y: usize = std.math.maxInt(i32) / 2;
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

    //
    // NOTE: Movement
    //

    for (0..input.controllers.len) |controller_index| {
        const controller: *platform.GameControllerInput =
            try platform.get_controller(input, controller_index);

        const controlling_entity = get_entity(
            game_state,
            game_state.player_controller_index[controller_index],
        );

        if (controlling_entity) |entity| {
            var dd_pos = Vec2{};

            if (controller.is_analog) {
                // NOTE: Use analog movement tuning
                dd_pos = Vec2{
                    .x = controller.stick_average_x,
                    .y = controller.stick_average_y,
                };
            } else {
                // NOTE: Use digital movement tuning

                if (controller.buttons.map.move_up.ended_down) {
                    dd_pos.y = 1.0;
                }

                if (controller.buttons.map.move_down.ended_down) {
                    dd_pos.y = -1.0;
                }

                if (controller.buttons.map.move_left.ended_down) {
                    dd_pos.x = -1.0;
                }

                if (controller.buttons.map.move_right.ended_down) {
                    dd_pos.x = 1.0;
                }
            }

            move_player(game_state, entity, input.dt_for_frame, dd_pos);
        } else {
            if (controller.buttons.map.start.ended_down) {
                const entity_index = add_entity(game_state);
                initialize_player(game_state, entity_index);
                game_state.player_controller_index[controller_index] = entity_index;
            }
        }
    }

    const camera_following_entity = get_entity(game_state, game_state.camera_entity_index);

    if (camera_following_entity) |entity| {
        game_state.camera_p.abs_tile_z = entity.pos.abs_tile_z;

        const diff = tile.subtract(tile_map, &entity.pos, &game_state.camera_p);

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

    //
    // NOTE: Render
    //
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
                        meters_to_pixels * game_state.camera_p.offset_.x +
                        @as(f32, @floatFromInt(rel_column * tile_side_in_pixels)),
                    .y = screen_center_y +
                        meters_to_pixels * game_state.camera_p.offset_.y -
                        @as(f32, @floatFromInt(rel_row * tile_side_in_pixels)),
                };

                const min = math.sub(cen, math.scale(tile_side, 0.9));
                const max = math.add(cen, math.scale(tile_side, 0.9));

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

    const entities = game_state.entities;
    for (0..game_state.entity_count) |entity_index| {
        var entity = entities[entity_index];

        // TODO: Culling of entities based on z/camera view
        if (entity.exists) {
            const diff = tile.subtract(tile_map, &entity.pos, &game_state.camera_p);

            const player_r = 1.0;
            const player_g = 1.0;
            const player_b = 0.0;
            const player_ground_point_x = screen_center_x + meters_to_pixels * diff.dxy.x;
            const player_ground_point_y = screen_center_y - meters_to_pixels * diff.dxy.y;
            const player_origin = Vec2{
                .x = player_ground_point_x - 0.5 * meters_to_pixels * entity.width,
                .y = player_ground_point_y - meters_to_pixels * entity.height,
            };
            const entity_dimensions = math.scale(
                Vec2{ .x = entity.width, .y = entity.height },
                meters_to_pixels,
            );

            draw_rectangle(
                buffer,
                player_origin,
                math.add(player_origin, entity_dimensions),
                player_r,
                player_g,
                player_b,
            );

            var hero_bitmaps = game_state.hero_bitmaps[entity.facing_direction];

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
