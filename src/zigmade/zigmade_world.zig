const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const Vec2 = math.Vec2;
const TILE_CHUNK_SAFE_MARGIN = std.math.maxInt(i32) / 64;
const TILE_CHUNK_UNINITIALIZED = std.math.maxInt(i32);
const TILES_PER_CHUNK = 16;

// TODO: Replace this with a Vec3 once we get to Vec3
pub const WorldDifference = struct {
    dxy: Vec2,
    dz: f32,
};

pub const WorldPosition = struct {
    // TODO: How can we get rid of abs_tile_* here,
    // and still allow references to entities to be able to figure out
    // where they are/which world chunk they are in?
    chunk_x: i32 = 0,
    chunk_y: i32 = 0,
    chunk_z: i32 = 0,
    // NOTE: Offset from chunk center
    offset_: Vec2 = Vec2{},
};

// TODO: Could make this just TileChunk and allow multiple chunks per x/y/z
pub const WorldEntityBlock = struct {
    entity_count: u32,
    low_entity_index: [16]u32,
    next: ?*WorldEntityBlock,
};

pub const WorldChunk = struct {
    chunk_x: i32,
    chunk_y: i32,
    chunk_z: i32,
    // TODO: Profile this to determine if a pointer would be better here
    first_block: WorldEntityBlock,
    next_in_hash: ?*WorldChunk = null,
};

pub const World = struct {
    tile_side_in_meters: f32,
    chunk_side_in_meters: f32,
    // TODO: tile_chunk_hash should probably switch to pointers if
    // tile entity blocks continue to be store en masse directly
    // inside the tile chunk
    // NOTE: At the moment, this must be a power of two
    chunk_hash: [4096]WorldChunk = undefined,
    first_free: ?*WorldEntityBlock = null,
};

pub fn initializeWorld(world: *World, tile_side_in_meters: f32) void {
    world.tile_side_in_meters = tile_side_in_meters;
    world.chunk_side_in_meters = TILES_PER_CHUNK * tile_side_in_meters;
    world.first_free = null;

    for (0..world.chunk_hash.len) |index| {
        world.chunk_hash[index].chunk_x = TILE_CHUNK_UNINITIALIZED;
        world.chunk_hash[index].first_block.entity_count = 0;
    }
}

pub inline fn getWorldChunk(
    world: *World,
    chunk_x: i32,
    chunk_y: i32,
    chunk_z: i32,
    arena: ?*game.MemoryArena,
) ?*WorldChunk {
    assert(chunk_x > -TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_y > -TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_z > -TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_x < TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_y < TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_z < TILE_CHUNK_SAFE_MARGIN);

    // TODO: BETTER HASH FUNCTION
    const hash_value = 19 * chunk_x + 7 * chunk_y + 3 * chunk_z;
    const hash_slot = @as(u32, @bitCast(hash_value)) & (world.chunk_hash.len - 1);
    assert(hash_slot < world.chunk_hash.len);
    var world_chunk: ?*WorldChunk = &world.chunk_hash[hash_slot];

    // NOTE: Without the doule indirection to world chunks,
    // chunk fetching will not behave correctly
    while (world_chunk) |*chunk| : (world_chunk = chunk.*.next_in_hash) {
        if ((chunk_x == chunk.*.chunk_x) and
            (chunk_y == chunk.*.chunk_y) and
            (chunk_z == chunk.*.chunk_z))
        {
            break;
        }

        if (arena) |a| {
            if (chunk.*.chunk_x != TILE_CHUNK_UNINITIALIZED and
                chunk.*.next_in_hash == null)
            {
                chunk.*.next_in_hash = game.pushStruct(a, WorldChunk);
                world_chunk = chunk.*.next_in_hash;
                chunk.*.chunk_x = TILE_CHUNK_UNINITIALIZED;
            }
        }

        if (arena != null and chunk.*.chunk_x == TILE_CHUNK_UNINITIALIZED) {
            chunk.*.chunk_x = chunk_x;
            chunk.*.chunk_y = chunk_y;
            chunk.*.chunk_z = chunk_z;

            chunk.*.next_in_hash = null;

            break;
        }
    }

    return world_chunk;
}

pub inline fn subtract(
    world: *World,
    a: *WorldPosition,
    b: *WorldPosition,
) WorldDifference {
    var result: WorldDifference = undefined;

    const d_tile_xy = Vec2{
        .x = @as(f32, @floatFromInt(a.chunk_x)) -
            @as(f32, @floatFromInt(b.chunk_x)),
        .y = @as(f32, @floatFromInt(a.chunk_y)) -
            @as(f32, @floatFromInt(b.chunk_y)),
    };

    const d_tile_z = @as(f32, @floatFromInt(a.chunk_z)) -
        @as(f32, @floatFromInt(b.chunk_z));

    result.dxy.x = world.chunk_side_in_meters * d_tile_xy.x +
        (a.offset_.x - b.offset_.x);
    result.dxy.y = world.chunk_side_in_meters * d_tile_xy.y +
        (a.offset_.y - b.offset_.y);
    // TODO: Think about what we want to do with z
    result.dz = world.chunk_side_in_meters * d_tile_z;

    return result;
}

pub inline fn centeredChunkPoint(
    chunk_x: u32,
    chunk_y: u32,
    chunk_z: u32,
) WorldPosition {
    var result: WorldPosition = undefined;

    result.chunk_x = chunk_x;
    result.chunk_y = chunk_y;
    result.chunk_z = chunk_z;

    return result;
}

pub inline fn changeEntityLocation(
    world: *World,
    arena: *game.MemoryArena,
    low_entity_index: u32,
    maybe_old_p: ?*WorldPosition,
    new_p: *WorldPosition,
) void {
    if (maybe_old_p != null and inSameChunk(world, maybe_old_p.?, new_p)) {
        // NOTE: Leave entity where it is
    } else {
        if (maybe_old_p) |old_p| {
            // NOTE: Pull entity out of its old entity block
            const chunk = getWorldChunk(
                world,
                old_p.chunk_x,
                old_p.chunk_y,
                old_p.chunk_z,
                null,
            );

            assert(chunk != null);

            if (chunk) |c| {
                const first_block = &c.first_block;
                var block: ?*WorldEntityBlock = first_block;
                var found = false;

                while (block) |b| : (block = b.next) {
                    if (found) break;

                    for (0..b.entity_count) |index| {
                        if (b.low_entity_index[index] == low_entity_index) {
                            assert(first_block.entity_count > 0);

                            first_block.entity_count -= 1;
                            b.low_entity_index[index] =
                                first_block.low_entity_index[first_block.entity_count];

                            if (first_block.entity_count == 0) {
                                if (first_block.next) |next| {
                                    var next_block = next;
                                    first_block.* = next_block.*;

                                    next_block.next = world.first_free;
                                    world.first_free = next_block;
                                }
                            }

                            found = true;
                            break;
                        }
                    }
                }
            }
        }

        // NOTE: Insert entity  into its new entity block
        const chunk = getWorldChunk(
            world,
            new_p.chunk_x,
            new_p.chunk_y,
            new_p.chunk_z,
            arena,
        );

        assert(chunk != null);

        if (chunk) |c| {
            var block = &c.first_block;

            if (block.entity_count == block.low_entity_index.len) {
                var old_block = world.first_free;

                if (old_block) |old| {
                    world.first_free = old.next;
                } else {
                    old_block = game.pushStruct(arena, WorldEntityBlock);
                }

                old_block.?.* = block.*;
                block.next = old_block;
                block.entity_count = 0;
            }

            assert(block.entity_count < block.low_entity_index.len);
            block.low_entity_index[block.entity_count] = low_entity_index;
            block.entity_count += 1;
        }
    }
}

inline fn tileRelIsCanonical(
    world: *World,
    tile_rel: f32,
) bool {
    const result =
        tile_rel >= -0.5 * world.chunk_side_in_meters and
        tile_rel <= 0.5 * world.chunk_side_in_meters;

    // TODO: Fix floating point math so this can be exact

    return result;
}

inline fn vecIsCanonical(
    world: *World,
    offset: Vec2,
) bool {
    const result =
        tileRelIsCanonical(world, offset.x) and
        tileRelIsCanonical(world, offset.y);

    return result;
}

inline fn recanonicalizeCoordinate(
    world: *World,
    tile: *i32,
    tile_rel: *f32,
) void {
    // TODO: Don't use the divide/multiply method for recanonicalizing
    // because this can round back onto the previous tile
    // TODO: Add bounds checking to prevent wrapping

    // NOTE: Wrapping is not allowed, all coordinates are assumed to be within
    // the safe margin
    // TODO: Assert that we are nowhere near the edges of the world
    const offset: i32 = @intFromFloat(@round(tile_rel.* / world.chunk_side_in_meters));
    tile.* += offset;
    tile_rel.* -= @as(f32, @floatFromInt(offset)) * world.chunk_side_in_meters;

    assert(tileRelIsCanonical(world, tile_rel.*));
}

pub inline fn mapIntoChunkSpace(
    world: *World,
    base_pos: WorldPosition,
    offset: Vec2,
) WorldPosition {
    var result = base_pos;

    result.offset_ = math.add(result.offset_, offset);
    recanonicalizeCoordinate(world, &result.chunk_x, &result.offset_.x);
    recanonicalizeCoordinate(world, &result.chunk_y, &result.offset_.y);

    return result;
}

pub fn chunkPosFromTilePos(
    world: *World,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) WorldPosition {
    var result = WorldPosition{};

    result.chunk_x = @divTrunc(abs_tile_x, TILES_PER_CHUNK);
    result.chunk_y = @divTrunc(abs_tile_y, TILES_PER_CHUNK);
    result.chunk_z = @divTrunc(abs_tile_z, TILES_PER_CHUNK);

    // TODO: Think this through and actually work out the math
    if (abs_tile_x < 0) result.chunk_x -= 1;
    if (abs_tile_y < 0) result.chunk_y -= 1;
    if (abs_tile_z < 0) result.chunk_z -= 1;

    // TODO: Decide on tile alignment in chunks
    result.offset_.x = @as(f32, @floatFromInt((abs_tile_x - TILES_PER_CHUNK / 2) -
        (result.chunk_x * TILES_PER_CHUNK))) * world.tile_side_in_meters;

    result.offset_.y = @as(f32, @floatFromInt((abs_tile_y - TILES_PER_CHUNK / 2) -
        (result.chunk_y * TILES_PER_CHUNK))) * world.tile_side_in_meters;
    // TODO: Move to 3D z

    assert(vecIsCanonical(world, result.offset_));

    return result;
}

pub inline fn inSameChunk(
    world: *World,
    a: *WorldPosition,
    b: *WorldPosition,
) bool {
    assert(vecIsCanonical(world, a.offset_));
    assert(vecIsCanonical(world, b.offset_));

    const result = (a.chunk_x == b.chunk_x and
        a.chunk_y == b.chunk_y and
        a.chunk_z == b.chunk_z);

    return result;
}
