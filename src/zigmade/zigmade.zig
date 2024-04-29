const std = @import("std");
const assert = std.debug.assert;
const rotl = std.math.rotl;
const platform = @import("zigmade_platform");
const world = @import("zigmade_world.zig");
const math = @import("zigmade_math.zig");
const Vec2 = math.Vec2;
const intrinsics = @import("zigmade_intrinsics.zig");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;

const HeroBitmaps = struct {
    head: Bitmap,
    cape: Bitmap,
    torso: Bitmap,
    alignment: Vec2,
};

const HighEntity = struct {
    pos: Vec2 = Vec2{}, // NOTE: Relative to the camera!
    d_pos: Vec2 = Vec2{},
    chunk_z: u32 = 0,
    facing_direction: u32 = 0,
    t_bob: f32 = 0,
    z: f32 = 0,
    dz: f32 = 0,
    low_entity_index: u32 = 0,
};

pub const LowEntity = struct {
    type: EntityType = .none,
    pos: world.WorldPosition = world.WorldPosition{},
    width: f32 = 0,
    height: f32 = 0,
    // NOTE: This is for "stairs"
    collides: bool = false,
    d_abs_tile_z: i32 = 0,
    high_entity_index: u32 = 0,
};

const EntityType = enum {
    none,
    hero,
    wall,
    familiar,
    monster,
};

const Entity = struct {
    low_index: u32 = 0,
    low: ?*LowEntity = null,
    high: ?*HighEntity = null,
};

const EntityVisiblePiece = struct {
    bitmap: *Bitmap,
    offset: Vec2,
    offset_z: f32,
    alpha: f32,
};

const EntityVisiblePieceGroup = struct {
    piece_count: u32 = 0,
    pieces: [8]EntityVisiblePiece,
};

const AddLowEntityResult = struct {
    low_index: u32,
    low: *LowEntity,
};

const GameState = struct {
    world: ?*world.World = null,
    world_arena: MemoryArena,
    // TODO: Should we allow split-screen?
    camera_entity_index: u32,
    camera_p: world.WorldPosition,
    low_entity_count: u32 = 0,
    low_entities: [100000]LowEntity,
    high_entity_count: u32 = 0,
    high_entities_: [256]HighEntity,
    player_controller_index: [5]u32,
    backdrop: Bitmap,
    shadow: Bitmap,
    hero_bitmaps: [4]HeroBitmaps,
    tree: Bitmap,
};

pub const MemoryArena = struct {
    size: u32,
    base: [*]u8,
    used: u32,
};

const Bitmap = struct {
    width: i32,
    height: i32,
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

// TODO: The seed value affects whether all entities spawn in the correct frequency or not
// and after some amount of navigation, entities are encountered in the incorrect frequency within
// the bounds of the camera no matter what the seed is
var rand = std.rand.DefaultPrng.init(8192);

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

        row += @as(u32, @intCast(buffer.pitch));
    }
}

fn drawBitmap(
    buffer: *platform.GameOffscreenBuffer,
    bitmap: *Bitmap,
    real_x: f32,
    real_y: f32,
    c_alpha: f32,
) void {
    var min_x: i32 = @intFromFloat(@round(real_x));
    var min_y: i32 = @intFromFloat(@round(real_y));
    var max_x: i32 = min_x + bitmap.width;
    var max_y: i32 = min_y + bitmap.height;
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
        @as(u32, @intCast(bitmap.width * (bitmap.height - 1))) -
        @as(u32, @intCast(bitmap.width * source_offset_y)) +
        @as(u32, @intCast(source_offset_x));

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
        source_row -= @as(usize, @intCast(bitmap.width));
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
    size: u32,
    base: [*]u8,
) void {
    arena.size = size;
    arena.base = base;
    arena.used = 0;
}

fn pushSize(
    arena: *MemoryArena,
    size: u32,
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

fn moveEntity(
    game_state: *GameState,
    entity: Entity,
    dt: f32,
    dd_pos: Vec2,
) void {
    const high = entity.high.?;
    const low = entity.low.?;

    const game_world = game_state.world.?;

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
                const test_high = &game_state.high_entities_[high_index];
                const test_low_index = test_high.low_entity_index;
                const test_low = &game_state.low_entities[test_low_index];

                if (test_low.collides) {
                    const diameter_w = test_low.width + low.width;
                    const diameter_h = test_low.height + low.height;
                    const min_corner = math.scale(Vec2{ .x = diameter_w, .y = diameter_h }, -0.5);
                    const max_corner = math.scale(Vec2{ .x = diameter_w, .y = diameter_h }, 0.5);
                    const rel = math.sub(high.pos, test_high.pos);

                    if (testWall(
                        min_corner.x,
                        rel.x,
                        rel.y,
                        player_delta.x,
                        player_delta.y,
                        &t_min,
                        min_corner.y,
                        max_corner.y,
                    )) {
                        wall_normal = Vec2{ .x = -1, .y = 0 };
                        hit_high_index = high_index;
                    }

                    if (testWall(
                        max_corner.x,
                        rel.x,
                        rel.y,
                        player_delta.x,
                        player_delta.y,
                        &t_min,
                        min_corner.y,
                        max_corner.y,
                    )) {
                        wall_normal = Vec2{ .x = 1, .y = 0 };
                        hit_high_index = high_index;
                    }

                    if (testWall(
                        min_corner.y,
                        rel.y,
                        rel.x,
                        player_delta.y,
                        player_delta.x,
                        &t_min,
                        min_corner.x,
                        max_corner.x,
                    )) {
                        wall_normal = Vec2{ .x = 0, .y = -1 };
                        hit_high_index = high_index;
                    }

                    if (testWall(
                        max_corner.y,
                        rel.y,
                        rel.x,
                        player_delta.y,
                        player_delta.x,
                        &t_min,
                        min_corner.x,
                        max_corner.x,
                    )) {
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

            //const hit_high = &game_state.high_entities_[hit_high_index];
            //const hit_low = &game_state.low_entities[hit_high.low_entity_index];
            // TODO: stairs
            //high.abs_tile_z += @bitCast(hit_low.d_abs_tile_z);
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

    var new_p = world.mapIntoChunkSpace(
        game_world,
        game_state.camera_p,
        high.pos,
    );

    // TODO: Bundle these together as the position update?
    world.changeEntityLocation(
        game_world,
        &game_state.world_arena,
        entity.low_index,
        &low.pos,
        &new_p,
    );

    low.pos = new_p;
}

inline fn getLowEntity(
    game_state: *GameState,
    index: u32,
) *LowEntity {
    var result: *LowEntity = undefined;

    if (index > 0 and index < game_state.low_entities.len) {
        result = &game_state.low_entities[index];
    }

    return result;
}

inline fn forceEntityIntoHigh(
    game_state: *GameState,
    low_index: u32,
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

inline fn getCameraSpaceP(
    game_state: *GameState,
    low_entity: *LowEntity,
) Vec2 {
    const game_world = game_state.world.?;

    // NOTE: Map entity into camera space
    const diff = world.subtract(game_world, &low_entity.pos, &game_state.camera_p);
    const result = diff.dxy;

    return result;
}

inline fn makeEntityHighFrequencyFromCamera(
    game_state: *GameState,
    low: *LowEntity,
    low_index: u32,
    camera_space_p: Vec2,
) *HighEntity {
    var high: *HighEntity = undefined;

    assert(low.high_entity_index == 0);

    if (low.high_entity_index == 0) {
        if (game_state.high_entity_count < game_state.high_entities_.len) {
            const high_index = game_state.high_entity_count;
            game_state.high_entity_count += 1;
            high = &game_state.high_entities_[high_index];

            high.pos = camera_space_p;
            high.d_pos = Vec2{};
            high.chunk_z = @intCast(low.pos.chunk_z);
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

inline fn makeEntityHighFrequency(
    game_state: *GameState,
    low_index: u32,
) *HighEntity {
    var high: *HighEntity = undefined;

    const low = &game_state.low_entities[low_index];

    if (low.high_entity_index > 0) {
        high = &game_state.high_entities_[low.high_entity_index];
    } else {
        const camera_space_p = getCameraSpaceP(game_state, low);

        high = makeEntityHighFrequencyFromCamera(
            game_state,
            low,
            low_index,
            camera_space_p,
        );
    }

    return high;
}

inline fn makeEntityLowFrequency(
    game_state: *GameState,
    low_index: u32,
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

inline fn validateEntityPairs(game_state: *GameState) bool {
    var valid = true;

    for (1..game_state.high_entity_count) |high_index| {
        const high = &game_state.high_entities_[high_index];

        valid = valid and (game_state
            .low_entities[high.low_entity_index]
            .high_entity_index ==
            high_index);
    }

    return valid;
}

inline fn offsetAndCheckFrequencyByArea(
    game_state: *GameState,
    offset: Vec2,
    high_frequency_bounds: math.Rectangle2,
) void {
    var high_index: usize = 1;

    while (high_index < game_state.high_entity_count) {
        var high = &game_state.high_entities_[high_index];
        high.pos = math.add(high.pos, offset);

        if (math.isInRectangle(high_frequency_bounds, high.pos)) {
            high_index += 1;
        } else {
            assert(game_state
                .low_entities[high.low_entity_index]
                .high_entity_index ==
                high_index);

            makeEntityLowFrequency(game_state, high.low_entity_index);
        }
    }
}

fn addLowEntity(
    game_state: *GameState,
    t: EntityType,
    pos: ?*world.WorldPosition,
) AddLowEntityResult {
    assert(game_state.low_entity_count < game_state.low_entities.len);

    const entity_index = game_state.low_entity_count;
    game_state.low_entity_count += 1;

    var low = &game_state.low_entities[entity_index];
    low.* = LowEntity{};
    low.type = t;

    if (pos) |p| {
        low.pos = p.*;

        world.changeEntityLocation(
            game_state.world.?,
            &game_state.world_arena,
            entity_index,
            null,
            pos,
        );
    }

    var result: AddLowEntityResult = undefined;
    result.low = low;
    result.low_index = entity_index;

    // TODO: Do we need a begin/end paradigm for adding
    // entities so that they can be brought into the high set
    // when they are added and are in the camera region?

    return result;
}

fn addWall(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) AddLowEntityResult {
    const game_world = game_state.world.?;

    var pos = world.chunkPosFromTilePos(
        game_world,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
    );

    const entity = addLowEntity(game_state, .wall, &pos);

    entity.low.height = game_world.tile_side_in_meters;
    entity.low.width = entity.low.height;
    entity.low.collides = true;

    return entity;
}

fn addPlayer(game_state: *GameState) AddLowEntityResult {
    var pos = game_state.camera_p;
    const entity = addLowEntity(game_state, .hero, &pos);

    entity.low.height = 0.5;
    entity.low.width = 1.0;
    entity.low.collides = true;

    if (game_state.camera_entity_index == 0) {
        game_state.camera_entity_index = entity.low_index;
    }

    return entity;
}

fn addMonster(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) AddLowEntityResult {
    const game_world = game_state.world.?;

    var pos = world.chunkPosFromTilePos(
        game_world,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
    );

    const entity = addLowEntity(game_state, .monster, &pos);
    entity.low.height = 0.5;
    entity.low.width = 1.0;
    entity.low.collides = true;

    return entity;
}

fn addFamiliar(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) AddLowEntityResult {
    const game_world = game_state.world.?;

    var pos = world.chunkPosFromTilePos(
        game_world,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
    );

    const entity = addLowEntity(game_state, .familiar, &pos);
    entity.low.height = 0.5;
    entity.low.width = 1.0;
    entity.low.collides = true;

    return entity;
}

fn setCamera(
    game_state: *GameState,
    new_camera_p: *world.WorldPosition,
) void {
    const game_world = game_state.world.?;
    const d_camera_p = world.subtract(game_world, new_camera_p, &game_state.camera_p);
    game_state.camera_p = new_camera_p.*;

    // TODO: Dim is chosen randomly
    const tile_span_x = 17 * 3;
    const tile_span_y = 9 * 3;
    const camera_bounds = math.rectCenterDim(
        Vec2{},
        math.scale(
            Vec2{ .x = tile_span_x, .y = tile_span_y },
            game_world.tile_side_in_meters,
        ),
    );

    const entity_offset_for_frame = math.negate(d_camera_p.dxy);

    offsetAndCheckFrequencyByArea(
        game_state,
        entity_offset_for_frame,
        camera_bounds,
    );

    assert(validateEntityPairs(game_state));

    // TODO: Do this in terms of tile chunks!
    const min_chunk_p = world.mapIntoChunkSpace(
        game_world,
        new_camera_p.*,
        math.getMinCorner(camera_bounds),
    );

    const max_chunk_p = world.mapIntoChunkSpace(
        game_world,
        new_camera_p.*,
        math.getMaxCorner(camera_bounds),
    );

    var chunk_y = min_chunk_p.chunk_y;

    while (chunk_y <= max_chunk_p.chunk_y) : (chunk_y += 1) {
        var chunk_x = min_chunk_p.chunk_x;

        while (chunk_x <= max_chunk_p.chunk_x) : (chunk_x += 1) {
            const chunk = world.getWorldChunk(
                game_world,
                chunk_x,
                chunk_y,
                new_camera_p.chunk_z,
                null,
            );

            if (chunk) |c| {
                var block: ?*world.WorldEntityBlock = &c.first_block;

                while (block) |b| : (block = block.?.next) {
                    for (0..b.entity_count) |entity_index| {
                        const low_index = b.low_entity_index[entity_index];
                        const low = &game_state.low_entities[low_index];

                        if (low.high_entity_index == 0) {
                            const camera_space_p = getCameraSpaceP(game_state, low);

                            if (math.isInRectangle(camera_bounds, camera_space_p)) {
                                _ = makeEntityHighFrequencyFromCamera(
                                    game_state,
                                    low,
                                    @intCast(low_index),
                                    camera_space_p,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    assert(validateEntityPairs(game_state));
}

fn pushPiece(
    group: *EntityVisiblePieceGroup,
    bitmap: *Bitmap,
    offset: Vec2,
    offset_z: f32,
    alignment: Vec2,
    alpha: f32,
) void {
    assert(group.piece_count < group.pieces.len);

    var piece = &group.pieces[group.piece_count];
    group.piece_count += 1;

    piece.bitmap = bitmap;
    piece.offset = math.sub(offset, alignment);
    piece.offset_z = offset_z;
    piece.alpha = alpha;
}

inline fn entityFromHighIndex(game_state: *GameState, high_index: usize) Entity {
    var result = Entity{};

    assert(high_index < game_state.high_entities_.len);

    result.high = &game_state.high_entities_[high_index];
    result.low_index = result.high.?.low_entity_index;
    result.low = &game_state.low_entities[result.low_index];

    return result;
}

fn updateFamiliar(game_state: *GameState, entity: Entity, dt: f32) void {
    var closest_hero = Entity{};
    var closest_hero_d_sq = math.square(10); // NOTE: Ten meter max search

    for (1..game_state.high_entity_count) |high_index| {
        const test_entity = entityFromHighIndex(game_state, high_index);

        if (test_entity.low.?.type == .hero) {
            const test_d_sq = math.lengthSquared(
                math.sub(
                    test_entity.high.?.pos,
                    entity.high.?.pos,
                ),
            );

            if (closest_hero_d_sq > test_d_sq) {
                closest_hero_d_sq = test_d_sq;
                closest_hero = test_entity;
            }
        }
    }

    var dd_p = Vec2{};

    if (closest_hero.high) |high| {
        if (closest_hero_d_sq > 0.1) {
            // TODO: Pull speed out of move entity
            const acceleration = 0.5;
            const one_over_length = acceleration / @sqrt(closest_hero_d_sq);

            dd_p = math.scale(
                math.sub(
                    high.pos,
                    entity.high.?.pos,
                ),
                one_over_length,
            );
        }
    }

    moveEntity(game_state, entity, dt, dd_p);
}

fn updateMonster(_: *GameState, _: Entity, _: f32) void {}

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
        _ = addLowEntity(game_state, .none, null);
        game_state.high_entity_count = 1;

        game_state.backdrop = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_background.bmp",
        );
        game_state.shadow = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_shadow.bmp",
        );
        game_state.tree = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test2/tree00.bmp",
        );

        var bitmaps = &game_state.hero_bitmaps;
        bitmaps[0].head = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_right_head.bmp",
        );
        bitmaps[0].cape = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_right_cape.bmp",
        );
        bitmaps[0].torso = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_right_torso.bmp",
        );
        bitmaps[0].alignment = Vec2{ .x = 72, .y = 182 };

        bitmaps[1].head = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_back_head.bmp",
        );
        bitmaps[1].cape = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_back_cape.bmp",
        );
        bitmaps[1].torso = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_back_torso.bmp",
        );
        bitmaps[1].alignment = Vec2{ .x = 72, .y = 182 };

        bitmaps[2].head = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_left_head.bmp",
        );
        bitmaps[2].cape = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_left_cape.bmp",
        );
        bitmaps[2].torso = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_left_torso.bmp",
        );
        bitmaps[2].alignment = Vec2{ .x = 72, .y = 182 };

        bitmaps[3].head = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_front_head.bmp",
        );
        bitmaps[3].cape = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_front_cape.bmp",
        );
        bitmaps[3].torso = debugLoadBmp(
            thread,
            memory.debugPlatformReadEntireFile,
            "data/test/test_hero_front_torso.bmp",
        );
        bitmaps[3].alignment = Vec2{ .x = 72, .y = 182 };

        // TODO: Can we just use Zig's own arena allocator?
        initializeArena(
            &game_state.world_arena,
            memory.permanent_storage_size - @sizeOf(GameState),
            memory.permanent_storage + @sizeOf(GameState),
        );

        game_state.world = pushStruct(&game_state.world_arena, world.World);
        const game_world = game_state.world.?;

        world.initializeWorld(game_world, 1.4);

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

        for (0..2000) |_| {
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

                    var tile_value: u32 = 1;

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

        //while (game_state.low_entity_count < game_state.low_entities.len - 16) {
        //    const coordinate: u32 = @intCast(1024 + game_state.low_entity_count);
        //    _ = addWall(game_state, coordinate, coordinate, coordinate);
        //}

        const camera_tile_x = screen_base_x * tiles_per_width + 17 / 2;
        const camera_tile_y = screen_base_y * tiles_per_height + 9 / 2;
        const camera_tile_z = screen_base_z;

        var new_camera_p = world.chunkPosFromTilePos(
            game_world,
            camera_tile_x,
            camera_tile_y,
            camera_tile_z,
        );

        _ = addMonster(
            game_state,
            camera_tile_x + 2,
            camera_tile_y + 2,
            camera_tile_z,
        );

        for (0..1) |_| {
            const familiar_offset_x = @mod(rand.random().int(i32), 10) - 7;
            const familiar_offset_y = @mod(rand.random().int(i32), 10) - 3;

            if (familiar_offset_x != 0 or familiar_offset_y != 0) {
                _ = addFamiliar(
                    game_state,
                    camera_tile_x + familiar_offset_x,
                    camera_tile_y + familiar_offset_y,
                    camera_tile_z,
                );
            }
        }

        setCamera(game_state, &new_camera_p);
        memory.is_initialized = true;
    }

    const game_world = game_state.world.?;

    const tile_side_in_pixels: i32 = 60;
    const meters_to_pixels: f32 =
        @as(f32, @floatFromInt(tile_side_in_pixels)) /
        game_world.tile_side_in_meters;

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
                const entity_index = addPlayer(game_state).low_index;
                game_state.player_controller_index[controller_index] = entity_index;
            }
        } else {
            const entity = forceEntityIntoHigh(game_state, low_index);

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

                moveEntity(game_state, entity, input.dt_for_frame, dd_pos);
            }
        }
    }

    const camera_following_entity = forceEntityIntoHigh(
        game_state,
        game_state.camera_entity_index,
    );

    if (camera_following_entity.high) |high| {
        const low = camera_following_entity.low.?;

        var new_camera_p = game_state.camera_p;

        new_camera_p.chunk_z = low.pos.chunk_z;

        if (false) {
            if (high.pos.x > (9.0 * game_world.tile_side_in_meters)) {
                new_camera_p.abs_tile_x +%= 17;
            }

            if (high.pos.x < -(9.0 * game_world.tile_side_in_meters)) {
                new_camera_p.abs_tile_x -%= 17;
            }

            if (high.pos.y > (5.0 * game_world.tile_side_in_meters)) {
                new_camera_p.abs_tile_y +%= 9;
            }

            if (high.pos.y < -(5.0 * game_world.tile_side_in_meters)) {
                new_camera_p.abs_tile_y -%= 9;
            }
        } else {
            new_camera_p = camera_following_entity.low.?.pos;
        }

        // TODO: Map new entities in and old entities out
        // TODO: Mapping tiles and stairs into the entity set

        setCamera(game_state, &new_camera_p);
    }

    //
    // NOTE: Render
    //
    if (true) {
        drawRectangle(
            buffer,
            Vec2{},
            Vec2{
                .x = @floatFromInt(buffer.width),
                .y = @floatFromInt(buffer.height),
            },
            0.5,
            0.5,
            0.5,
        );
    } else {
        drawBitmap(buffer, &game_state.backdrop, 0.0, 0.0, 0.0, 0.0, 1.0);
    }

    const screen_center_x = 0.5 * @as(f32, @floatFromInt(buffer.width));
    const screen_center_y = 0.5 * @as(f32, @floatFromInt(buffer.height));

    var piece_group: EntityVisiblePieceGroup = undefined;

    for (1..game_state.high_entity_count) |high_index| {
        piece_group.piece_count = 0;

        const high = &game_state.high_entities_[high_index];
        const low = &game_state.low_entities[high.low_entity_index];

        var entity: Entity = undefined;
        entity.low_index = high.low_entity_index;
        entity.low = low;
        entity.high = high;

        const dt = input.dt_for_frame;

        // TODO: This is incorrect, should be computed after update
        var shadow_alpha = 1.0 - 0.5 * high.z;

        if (shadow_alpha < 0.0) {
            shadow_alpha = 0.0;
        }

        var hero_bitmaps = game_state.hero_bitmaps[high.facing_direction];

        switch (low.type) {
            .hero => {
                // TODO: z
                pushPiece(&piece_group, &game_state.shadow, Vec2{}, 0, hero_bitmaps.alignment, shadow_alpha);
                pushPiece(&piece_group, &hero_bitmaps.torso, Vec2{}, 0, hero_bitmaps.alignment, 1);
                pushPiece(&piece_group, &hero_bitmaps.cape, Vec2{}, 0, hero_bitmaps.alignment, 1);
                pushPiece(&piece_group, &hero_bitmaps.head, Vec2{}, 0, hero_bitmaps.alignment, 1);
            },
            .wall => {
                pushPiece(&piece_group, &game_state.tree, Vec2{}, 0, Vec2{ .x = 40, .y = 80 }, 1);
            },
            .familiar => {
                updateFamiliar(game_state, entity, dt);
                high.t_bob += dt;

                if (high.t_bob > 2 * std.math.pi) {
                    high.t_bob -= 2 * std.math.pi;
                }

                pushPiece(&piece_group, &game_state.shadow, Vec2{}, 0, hero_bitmaps.alignment, shadow_alpha);
                pushPiece(&piece_group, &hero_bitmaps.head, Vec2{}, 10 * @sin(2 * high.t_bob), hero_bitmaps.alignment, 1);
            },
            .monster => {
                updateMonster(game_state, entity, dt);
                pushPiece(&piece_group, &game_state.shadow, Vec2{}, 0, hero_bitmaps.alignment, shadow_alpha);
                pushPiece(&piece_group, &hero_bitmaps.torso, Vec2{}, 0, hero_bitmaps.alignment, 1);
            },
            else => {
                std.debug.print("Invalid code path\n", .{});
                assert(false);
            },
        }

        const ddz = -9.8;
        high.z = 0.5 * ddz * math.square(dt) + high.dz * dt + high.z;
        high.dz = ddz * dt + high.dz;

        if (high.z < 0) {
            high.z = 0;
        }

        const eg_x = screen_center_x + meters_to_pixels * high.pos.x;
        const eg_y = screen_center_y - meters_to_pixels * high.pos.y;
        const entity_z = -meters_to_pixels * high.z;

        if (false) {
            const player_origin = Vec2{
                .x = eg_x - 0.5 * meters_to_pixels * low.width,
                .y = eg_y - 0.5 * meters_to_pixels * low.height,
            };

            const entity_dimensions = Vec2{
                .x = low.width,
                .y = low.height,
            };

            drawRectangle(
                buffer,
                player_origin,
                math.add(player_origin, math.scale(entity_dimensions, 0.9 * meters_to_pixels)),
                1.0,
                1.0,
                0.0,
            );
        }

        for (0..piece_group.piece_count) |index| {
            const piece = piece_group.pieces[index];

            drawBitmap(
                buffer,
                piece.bitmap,
                eg_x + piece.offset.x,
                eg_y + piece.offset.y + piece.offset_z + entity_z,
                piece.alpha,
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
