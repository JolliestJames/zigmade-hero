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

const HighEntity = struct {
    pos: Vec2 = Vec2{}, // NOTE: Relative to the camera!
    d_pos: Vec2 = Vec2{},
    abs_tile_z: u32 = 0,
    facing_direction: usize = 0,
    z: f32 = 0,
    dz: f32 = 0,
    low_entity_index: usize = 0,
};

const LowEntity = struct {
    type: EntityType = .none,
    pos: tile.TileMapPosition = tile.TileMapPosition{},
    width: f32 = 0,
    height: f32 = 0,
    // NOTE: This is for "stairs"
    collides: bool = false,
    d_abs_tile_z: i32 = 0,
    high_entity_index: usize = 0,
};

const EntityType = enum {
    none,
    hero,
    wall,
};

const Entity = struct {
    low_index: usize = 0,
    low: ?*LowEntity = null,
    high: ?*HighEntity = null,
};

const GameState = struct {
    world: ?*World = null,
    world_arena: MemoryArena,

    // TODO: Should we allow split-screen?
    camera_entity_index: usize,
    camera_p: tile.TileMapPosition,

    low_entity_count: usize = 0,
    low_entities: [4096]LowEntity,
    high_entity_count: usize = 0,
    high_entities_: [256]HighEntity,
    player_controller_index: [5]usize,
    backdrop: Bitmap,
    shadow: Bitmap,
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

fn gameOutputSound(
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

fn drawRectangle(
    buffer: *platform.GameOffscreenBuffer,
    min: Vec2,
    max: Vec2,
    r: f32,
    g: f32,
    b: f32,
) void {
    var min_x: i32 = @intFromFloat(@round(min.x));
    var min_y: i32 = @intFromFloat(@round(min.y));
    var max_x: i32 = @intFromFloat(@round(max.x));
    var max_y: i32 = @intFromFloat(@round(max.y));

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
        (@as(u32, @intCast(min_x)) *
        @as(u32, @intCast(buffer.bytes_per_pixel))) +
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

fn drawBitmap(
    buffer: *platform.GameOffscreenBuffer,
    bitmap: *Bitmap,
    unaligned_real_x: f32,
    unaligned_real_y: f32,
    align_x: i32,
    align_y: i32,
    c_alpha: f32,
) void {
    const real_x = unaligned_real_x - @as(f32, @floatFromInt(align_x));
    const real_y = unaligned_real_y - @as(f32, @floatFromInt(align_y));
    var min_x: i32 = @intFromFloat(@round(real_x));
    var min_y: i32 = @intFromFloat(@round(real_y));
    var max_x: i32 = min_x + @as(i32, @intCast(bitmap.width));
    var max_y: i32 = min_y + @as(i32, @intCast(bitmap.height));
    //var max_x: i32 = @intFromFloat(@round(real_x + @as(f32, @floatFromInt(bitmap.width))));
    //var max_y: i32 = @intFromFloat(@round(real_y + @as(f32, @floatFromInt(bitmap.height))));

    var source_offset_x: i32 = 0;
    if (min_x < 0) {
        source_offset_x = -min_x;
        min_x = 0;
    }

    var source_offset_y: i32 = 0;
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
        (@as(u32, @intCast(min_x)) *
        @as(u32, @intCast(buffer.bytes_per_pixel))) +
        @as(u32, @bitCast(min_y *% buffer.pitch));

    for (@intCast(min_y)..@intCast(max_y)) |_| {
        var dest: [*]u32 = @alignCast(@ptrCast(dest_row));
        var source = source_row;

        for (@intCast(min_x)..@intCast(max_x)) |_| {
            var a = @as(f32, @floatFromInt(((source[0] >> 24) & 0xFF))) / 255.0;
            a *= @floatCast(c_alpha);

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

fn debugLoadBmp(
    thread: *platform.ThreadContext,
    readEntireFile: platform.debugPlatformReadEntireFile,
    file_name: [*:0]const u8,
) Bitmap {
    var result: Bitmap = undefined;

    const read_result = readEntireFile(thread, file_name);

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

        const red_scan = intrinsics.findLeastSigSetBit(red_mask);
        const green_scan = intrinsics.findLeastSigSetBit(green_mask);
        const blue_scan = intrinsics.findLeastSigSetBit(blue_mask);
        const alpha_scan = intrinsics.findLeastSigSetBit(alpha_mask);

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

fn initializeArena(
    arena: *MemoryArena,
    size: usize,
    base: [*]u8,
) void {
    arena.size = size;
    arena.base = base;
    arena.used = 0;
}

fn pushSize(
    arena: *MemoryArena,
    size: usize,
) [*]u8 {
    assert((arena.used + size) <= arena.size);
    const result = arena.base + arena.used;
    arena.used += size;
    return result;
}

pub fn pushStruct(
    arena: *MemoryArena,
    comptime T: type,
) *T {
    const result = pushSize(arena, @sizeOf(T));
    return @as(*T, @alignCast(@ptrCast(result)));
}

pub fn pushArray(
    arena: *MemoryArena,
    count: usize,
    comptime T: type,
) [*]T {
    const result = pushSize(arena, count * @sizeOf(T));
    return @as([*]T, @alignCast(@ptrCast(result)));
}

fn testWall(
    wall: f32,
    rel_x: f32,
    rel_y: f32,
    player_delta_x: f32,
    player_delta_y: f32,
    t_min: *f32,
    min_y: f32,
    max_y: f32,
) bool {
    var hit = false;
    const t_epsilon = 0.001;

    if (player_delta_x != 0.0) {
        const t_result = (wall - rel_x) / player_delta_x;
        const y = rel_y + t_result * player_delta_y;

        if (t_result >= 0.0 and t_min.* > t_result) {
            if (y >= min_y and y <= max_y) {
                t_min.* = @max(0.0, t_result - t_epsilon);
                hit = true;
            }
        }
    }

    return hit;
}

fn movePlayer(
    game_state: *GameState,
    entity: Entity,
    dt: f32,
    dd_pos: Vec2,
) void {
    const high = entity.high.?;
    const low = entity.low.?;

    const tile_map = game_state.world.?.tile_map;

    var acceleration = dd_pos;

    const acc_length = math.lengthSquared(acceleration);

    if (acc_length > 1.0) {
        acceleration = math.scale(acceleration, 1.0 / @sqrt(acc_length));
    }

    const player_speed: f32 = 50.0; // m/s^2
    acceleration = math.scale(acceleration, player_speed);

    // TODO: ODE here
    acceleration = math.add(
        acceleration,
        math.scale(high.d_pos, -8.0),
    );

    var player_delta = math.add(
        math.scale(acceleration, 0.5 * math.square(dt)),
        math.scale(high.d_pos, dt),
    );

    high.d_pos = math.add(
        math.scale(acceleration, dt),
        high.d_pos,
    );

    for (0..4) |_| {
        var t_min: f32 = 1.0;
        var wall_normal = Vec2{};
        var hit_high_index: usize = 0;
        const desired_position = math.add(high.pos, player_delta);

        for (1..game_state.high_entity_count) |high_index| {
            if (high_index != low.high_entity_index) {
                //var test_entity: Entity = undefined;
                const test_high = &game_state.high_entities_[high_index];
                const test_low_index = test_high.low_entity_index;
                const test_low = &game_state.low_entities[test_low_index];

                if (test_low.collides) {
                    const diameter_w = test_low.width + low.width;
                    const diameter_h = test_low.height + low.height;
                    const min_corner = math.scale(Vec2{ .x = diameter_w, .y = diameter_h }, -0.5);
                    const max_corner = math.scale(Vec2{ .x = diameter_w, .y = diameter_h }, 0.5);
                    const rel = math.sub(high.pos, test_high.pos);

                    if (testWall(min_corner.x, rel.x, rel.y, player_delta.x, player_delta.y, &t_min, min_corner.y, max_corner.y)) {
                        wall_normal = Vec2{ .x = -1, .y = 0 };
                        hit_high_index = high_index;
                    }

                    if (testWall(max_corner.x, rel.x, rel.y, player_delta.x, player_delta.y, &t_min, min_corner.y, max_corner.y)) {
                        wall_normal = Vec2{ .x = 1, .y = 0 };
                        hit_high_index = high_index;
                    }

                    if (testWall(min_corner.y, rel.y, rel.x, player_delta.y, player_delta.x, &t_min, min_corner.x, max_corner.x)) {
                        wall_normal = Vec2{ .x = 0, .y = -1 };
                        hit_high_index = high_index;
                    }

                    if (testWall(max_corner.y, rel.y, rel.x, player_delta.y, player_delta.x, &t_min, min_corner.x, max_corner.x)) {
                        wall_normal = Vec2{ .x = 0, .y = 1 };
                        hit_high_index = high_index;
                    }
                }
            }
        }

        high.pos = math.add(
            high.pos,
            math.scale(player_delta, t_min),
        );

        if (hit_high_index > 0) {
            high.d_pos = math.sub(
                high.d_pos,
                math.scale(
                    wall_normal,
                    1 * math.inner(high.d_pos, wall_normal),
                ),
            );

            player_delta = math.sub(desired_position, high.pos);

            player_delta = math.sub(
                player_delta,
                math.scale(
                    wall_normal,
                    1 * math.inner(player_delta, wall_normal),
                ),
            );

            const hit_high = &game_state.high_entities_[hit_high_index];
            const hit_low = &game_state.low_entities[hit_high.low_entity_index];
            high.abs_tile_z += @bitCast(hit_low.d_abs_tile_z);
        } else {
            break;
        }
    }

    if (high.d_pos.x == 0.0 and high.d_pos.y == 0.0) {
        // Leave facing_direction alone
    } else if (@abs(high.d_pos.x) > @abs(high.d_pos.y)) {
        if (high.d_pos.x > 0) {
            high.facing_direction = 0;
        } else {
            high.facing_direction = 2;
        }
    } else if (@abs(high.d_pos.x) < @abs(high.d_pos.y)) {
        if (high.d_pos.y > 0) {
            high.facing_direction = 1;
        } else {
            high.facing_direction = 3;
        }
    }

    low.pos = tile.mapIntoTileSpace(tile_map, game_state.camera_p, high.pos);
}

inline fn getLowEntity(
    game_state: *GameState,
    index: usize,
) *LowEntity {
    var result: *LowEntity = undefined;

    if (index > 0 and index < game_state.low_entities.len) {
        result = &game_state.low_entities[index];
    }

    return result;
}

inline fn getHighEntity(
    game_state: *GameState,
    low_index: usize,
) Entity {
    var result = Entity{};

    // TODO: Should we allow passing a zero index and return a null pointer?
    if (low_index > 0 and low_index < game_state.low_entity_count) {
        result.low_index = low_index;
        result.low = &game_state.low_entities[low_index];
        result.high = makeEntityHighFrequency(game_state, low_index);
    }

    return result;
}

inline fn makeEntityHighFrequency(
    game_state: *GameState,
    low_index: usize,
) *HighEntity {
    var high: *HighEntity = undefined;
    const tile_map = game_state.world.?.tile_map;
    var low = &game_state.low_entities[low_index];

    if (low.high_entity_index > 0) {
        high = &game_state.high_entities_[low.high_entity_index];
    } else {
        if (game_state.high_entity_count < game_state.high_entities_.len) {
            const high_index = game_state.high_entity_count;
            game_state.high_entity_count += 1;
            high = &game_state.high_entities_[high_index];

            // NOTE: Map entity into camera space
            const diff = tile.subtract(tile_map, &low.pos, &game_state.camera_p);
            high.pos = diff.dxy;
            high.d_pos = Vec2{};
            high.abs_tile_z = @intCast(low.pos.abs_tile_z);
            high.facing_direction = 0;
            high.low_entity_index = low_index;

            low.high_entity_index = high_index;
        } else {
            std.debug.print("Invalid code path\n", .{});
            assert(false);
        }
    }

    return high;
}

inline fn makeEntityLowFrequency(
    game_state: *GameState,
    low_index: usize,
) void {
    const low = &game_state.low_entities[low_index];
    const high_index = low.high_entity_index;

    if (high_index > 0) {
        const last_high_index = game_state.high_entity_count - 1;

        if (high_index != last_high_index) {
            const last = &game_state.high_entities_[last_high_index];
            const deleted = &game_state.high_entities_[high_index];

            deleted.* = last.*;
            game_state.low_entities[last.low_entity_index].high_entity_index = high_index;
        }

        game_state.high_entity_count -= 1;
        low.high_entity_index = 0;
    }
}

inline fn offsetAndCheckFrequencyByArea(
    game_state: *GameState,
    offset: Vec2,
    camera_bounds: math.Rectangle2,
) void {
    var entity_index: usize = 1;

    while (entity_index < game_state.high_entity_count) {
        var high = &game_state.high_entities_[entity_index];
        high.pos = math.add(high.pos, offset);

        if (math.isInRectangle(camera_bounds, high.pos)) {
            entity_index += 1;
        } else {
            makeEntityLowFrequency(game_state, high.low_entity_index);
        }
    }
}

fn addLowEntity(game_state: *GameState, entity_type: EntityType) usize {
    assert(game_state.low_entity_count < game_state.low_entities.len);

    const entity_index = game_state.low_entity_count;
    game_state.low_entity_count += 1;

    game_state.low_entities[entity_index] = std.mem.zeroInit(LowEntity, .{});
    game_state.low_entities[entity_index].type = entity_type;

    return entity_index;
}

fn addWall(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) usize {
    const entity_index = addLowEntity(game_state, .wall);
    const entity = getLowEntity(game_state, entity_index);

    entity.pos.abs_tile_x = abs_tile_x;
    entity.pos.abs_tile_y = abs_tile_y;
    entity.pos.abs_tile_z = abs_tile_z;
    entity.height = game_state.world.?.tile_map.tile_side_in_meters;
    entity.width = entity.height;
    entity.collides = true;

    return entity_index;
}

fn addPlayer(game_state: *GameState) usize {
    const entity_index = addLowEntity(game_state, .hero);
    const entity = getLowEntity(game_state, entity_index);

    entity.pos = game_state.camera_p;
    entity.height = 0.5;
    entity.width = 1.0;
    entity.collides = true;

    if (game_state.camera_entity_index == 0) {
        game_state.camera_entity_index = entity_index;
    }

    return entity_index;
}

fn setCamera(
    game_state: *GameState,
    new_camera_p: *tile.TileMapPosition,
) void {
    const tile_map = game_state.world.?.tile_map;
    const d_camera_p = tile.subtract(tile_map, new_camera_p, &game_state.camera_p);
    game_state.camera_p = new_camera_p.*;

    // TODO: Dim is chosen randomly
    const tile_span_x = 17 * 3;
    const tile_span_y = 9 * 3;
    const camera_bounds = math.rectCenterDim(
        Vec2{},
        math.scale(
            Vec2{ .x = tile_span_x, .y = tile_span_y },
            tile_map.tile_side_in_meters,
        ),
    );

    const entity_offset_for_frame = math.negate(d_camera_p.dxy);
    offsetAndCheckFrequencyByArea(game_state, entity_offset_for_frame, camera_bounds);

    const min_tile_x = new_camera_p.abs_tile_x -% tile_span_x / 2;
    const max_tile_x = new_camera_p.abs_tile_x +% tile_span_x / 2;
    const min_tile_y = new_camera_p.abs_tile_y -% tile_span_y / 2;
    const max_tile_y = new_camera_p.abs_tile_y +% tile_span_y / 2;

    for (1..game_state.low_entity_count) |entity_index| {
        const low = &game_state.low_entities[entity_index];

        if (low.high_entity_index == 0) {
            if (low.pos.abs_tile_z == new_camera_p.abs_tile_z and
                low.pos.abs_tile_x >= min_tile_x and
                low.pos.abs_tile_x <= max_tile_x and
                low.pos.abs_tile_y >= min_tile_y and
                low.pos.abs_tile_y <= max_tile_y)
            {
                _ = makeEntityHighFrequency(game_state, entity_index);
            }
        }
    }
}

// GAME NEEDS FOUR THINGS
// - timing
// - controller/keyboard input
// - bitmap buffer
// - sound buffer
pub export fn updateAndRender(
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
        _ = addLowEntity(game_state, .none);
        game_state.high_entity_count = 1;

        game_state.backdrop = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_background.bmp");
        game_state.shadow = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_shadow.bmp");

        var bitmaps = &game_state.hero_bitmaps;
        bitmaps[0].head = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_right_head.bmp");
        bitmaps[0].cape = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_right_cape.bmp");
        bitmaps[0].torso = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_right_torso.bmp");
        bitmaps[0].align_x = 72;
        bitmaps[0].align_y = 182;

        bitmaps[1].head = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_back_head.bmp");
        bitmaps[1].cape = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_back_cape.bmp");
        bitmaps[1].torso = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_back_torso.bmp");
        bitmaps[1].align_x = 72;
        bitmaps[1].align_y = 182;

        bitmaps[2].head = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_left_head.bmp");
        bitmaps[2].cape = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_left_cape.bmp");
        bitmaps[2].torso = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_left_torso.bmp");
        bitmaps[2].align_x = 72;
        bitmaps[2].align_y = 182;

        bitmaps[3].head = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_front_head.bmp");
        bitmaps[3].cape = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_front_cape.bmp");
        bitmaps[3].torso = debugLoadBmp(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_front_torso.bmp");
        bitmaps[3].align_x = 72;
        bitmaps[3].align_y = 182;

        // TODO: Can we just use Zig's own arena allocator?
        initializeArena(
            &game_state.world_arena,
            memory.permanent_storage_size - @sizeOf(GameState),
            memory.permanent_storage + @sizeOf(GameState),
        );

        game_state.world = pushStruct(&game_state.world_arena, World);
        var world = game_state.world.?;
        world.tile_map = pushStruct(&game_state.world_arena, tile.TileMap);

        const tile_map = world.tile_map;

        tile.initializeTileMap(tile_map, 1.4);

        const tiles_per_width = 17;
        const tiles_per_height = 9;

        // TODO: Waiting for full sparseness
        const screen_base_x: i32 = 0;
        const screen_base_y: i32 = 0;
        const screen_base_z: i32 = 0;
        var screen_x: i32 = screen_base_x;
        var screen_y: i32 = screen_base_y;
        var abs_tile_z: i32 = screen_base_z;

        // TODO: Replace with real world generation
        var door_left = false;
        var door_right = false;
        var door_top = false;
        var door_bottom = false;
        var door_up = false;
        var door_down = false;

        for (0..2) |_| {
            var random_choice: usize = undefined;

            if (true or door_up or door_down) {
                random_choice = rand.random().int(usize) % 2;
            } else {
                random_choice = rand.random().int(usize) % 3;
            }

            var created_z_door = false;

            if (random_choice == 2) {
                created_z_door = true;

                if (abs_tile_z == screen_base_z) {
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
                    const abs_tile_x = screen_x * tiles_per_width + @as(i32, @intCast(tile_x));
                    const abs_tile_y = screen_y * tiles_per_height + @as(i32, @intCast(tile_y));

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

                    tile.setTileValue(&game_state.world_arena, tile_map, abs_tile_x, abs_tile_y, abs_tile_z, tile_value);

                    if (tile_value == 2) {
                        _ = addWall(game_state, abs_tile_x, abs_tile_y, abs_tile_z);
                    }
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
                if (abs_tile_z == screen_base_z)
                    abs_tile_z = screen_base_z + 1
                else
                    abs_tile_z = screen_base_z;
            } else if (random_choice == 1) {
                screen_x += 1;
            } else {
                screen_y += 1;
            }
        }

        var new_camera_p = tile.TileMapPosition{};
        new_camera_p.abs_tile_x = screen_base_x * tiles_per_width + 17 / 2;
        new_camera_p.abs_tile_y = screen_base_y * tiles_per_height + 9 / 2;
        new_camera_p.abs_tile_z = screen_base_z;
        setCamera(game_state, &new_camera_p);

        memory.is_initialized = true;
    }

    const world = game_state.world.?;
    const tile_map = world.tile_map;

    const tile_side_in_pixels: i32 = 60;
    const meters_to_pixels: f32 =
        @as(f32, @floatFromInt(tile_side_in_pixels)) /
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
            try platform.getController(input, controller_index);

        const low_index = game_state.player_controller_index[controller_index];

        if (low_index == 0) {
            if (controller.buttons.map.start.ended_down) {
                const entity_index = addPlayer(game_state);
                game_state.player_controller_index[controller_index] = entity_index;
            }
        } else {
            const entity = getHighEntity(game_state, low_index);

            if (entity.high) |high| {
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

                if (controller.buttons.map.action_up.ended_down) {
                    high.dz = 3.0;
                }

                movePlayer(game_state, entity, input.dt_for_frame, dd_pos);
            }
        }
    }

    const entity_offset_for_frame = Vec2{};
    const camera_following_entity = getHighEntity(game_state, game_state.camera_entity_index);

    if (camera_following_entity.high) |high| {
        const low = camera_following_entity.low.?;

        var new_camera_p = game_state.camera_p;

        new_camera_p.abs_tile_z = low.pos.abs_tile_z;

        if (true) {
            if (high.pos.x > (9.0 * tile_map.tile_side_in_meters)) {
                new_camera_p.abs_tile_x +%= 17;
            }

            if (high.pos.x < -(9.0 * tile_map.tile_side_in_meters)) {
                new_camera_p.abs_tile_x -%= 17;
            }

            if (high.pos.y > (5.0 * tile_map.tile_side_in_meters)) {
                new_camera_p.abs_tile_y +%= 9;
            }

            if (high.pos.y < -(5.0 * tile_map.tile_side_in_meters)) {
                new_camera_p.abs_tile_y -%= 9;
            }
        } else {
            if (high.pos.x > (1.0 * tile_map.tile_side_in_meters)) {
                new_camera_p.abs_tile_x +%= 1;
            }

            if (high.pos.x < -(1.0 * tile_map.tile_side_in_meters)) {
                new_camera_p.abs_tile_x -%= 1;
            }

            if (high.pos.y > (1.0 * tile_map.tile_side_in_meters)) {
                new_camera_p.abs_tile_y +%= 1;
            }

            if (high.pos.y < -(1.0 * tile_map.tile_side_in_meters)) {
                new_camera_p.abs_tile_y -%= 1;
            }
        }

        // TODO: Map new entities in and old entities out
        // TODO: Mapping tiles and stairs into the entity set

        setCamera(game_state, &new_camera_p);
    }

    //
    // NOTE: Render
    //
    drawBitmap(buffer, &game_state.backdrop, 0.0, 0.0, 0.0, 0.0, 1.0);

    const screen_center_x = 0.5 * @as(f32, @floatFromInt(buffer.width));
    const screen_center_y = 0.5 * @as(f32, @floatFromInt(buffer.height));

    if (false) {
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

                const tile_id = tile.getTileValue(
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

                    drawRectangle(
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
    }

    for (1..game_state.high_entity_count) |high_index| {
        const high = &game_state.high_entities_[high_index];
        const low = &game_state.low_entities[high.low_entity_index];

        high.pos = math.add(high.pos, entity_offset_for_frame);

        const dt = input.dt_for_frame;
        const ddz = -9.8;
        high.z = 0.5 * ddz * math.square(dt) + high.dz * dt + high.z;
        high.dz = ddz * dt + high.dz;

        if (high.z < 0) {
            high.z = 0;
        }

        var c_alpha = 1.0 - 0.5 * high.z;

        if (c_alpha < 0.0) {
            c_alpha = 0.0;
        }

        const player_r = 1.0;
        const player_g = 1.0;
        const player_b = 0.0;
        const pg_x = screen_center_x + meters_to_pixels * high.pos.x;
        const pg_y = screen_center_y - meters_to_pixels * high.pos.y;
        const z = -meters_to_pixels * high.z;

        const player_origin = Vec2{
            .x = pg_x - 0.5 * meters_to_pixels * low.width,
            .y = pg_y - 0.5 * meters_to_pixels * low.height,
        };

        const entity_dimensions = Vec2{
            .x = low.width,
            .y = low.height,
        };

        if (low.type == .hero) {
            var hero_bitmaps = game_state.hero_bitmaps[high.facing_direction];

            drawBitmap(buffer, &game_state.shadow, pg_x, pg_y, hero_bitmaps.align_x, hero_bitmaps.align_y, c_alpha);
            drawBitmap(buffer, &hero_bitmaps.torso, pg_x, pg_y + z, hero_bitmaps.align_x, hero_bitmaps.align_y, 1.0);
            drawBitmap(buffer, &hero_bitmaps.cape, pg_x, pg_y + z, hero_bitmaps.align_x, hero_bitmaps.align_y, 1.0);
            drawBitmap(buffer, &hero_bitmaps.head, pg_x, pg_y + z, hero_bitmaps.align_x, hero_bitmaps.align_y, 1.0);
        } else {
            drawRectangle(
                buffer,
                player_origin,
                math.add(
                    player_origin,
                    math.scale(entity_dimensions, meters_to_pixels),
                ),
                player_r,
                player_g,
                player_b,
            );
        }
    }
}

// NOTE: At the moment, this must be a very fast function
// It cannot be greater than ~1ms
// TODO: Reduce the pressure on this function's performance
// by measuring it or asking about it, etc.
pub export fn getSoundSamples(
    thread: *platform.ThreadContext,
    memory: *platform.GameMemory,
    sound_buffer: *platform.GameSoundBuffer,
) void {
    _ = thread;
    const game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    try gameOutputSound(
        sound_buffer,
        game_state,
    );
}
