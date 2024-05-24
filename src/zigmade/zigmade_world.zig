const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const TILE_CHUNK_SAFE_MARGIN = std.math.maxInt(i32) / 64;
const TILE_CHUNK_UNINITIALIZED = std.math.maxInt(i32);
const TILES_PER_CHUNK = 8;

const Vec3 = math.Vec3;
const MemoryArena = game.MemoryArena;
const LowEntity = game.LowEntity;

pub const WorldPosition = struct {
    // TODO: It seems like we have to store chunk_x/y/z with each
    // entity because even though the sim region gather doesn't need it
    // at first, and we could get by without it, entity references pull
    // in entities without going through their WorldChunk, and thus
    // still need to know chunk_x/y/z
    chunk_x: i32 = 0,
    chunk_y: i32 = 0,
    chunk_z: i32 = 0,
    // NOTE: Offset from chunk center
    offset_: Vec3 = Vec3.splat(0),
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
    chunk_dim_in_meters: Vec3,
    // TODO: tile_chunk_hash should probably switch to pointers if
    // tile entity blocks continue to be store en masse directly
    // inside the tile chunk
    // NOTE: At the moment, this must be a power of two
    chunk_hash: [4096]WorldChunk = undefined,
    first_free: ?*WorldEntityBlock = null,
};

pub fn initializeWorld(
    world: *World,
    chunk_dim_in_meters: Vec3,
) void {
    world.chunk_dim_in_meters = chunk_dim_in_meters;
    world.first_free = null;

    for (0..world.chunk_hash.len) |index| {
        world.chunk_hash[index].chunk_x = TILE_CHUNK_UNINITIALIZED;
        world.chunk_hash[index].first_block.entity_count = 0;
    }
}

pub inline fn nullPosition() WorldPosition {
    var result: WorldPosition = .{};

    result.chunk_x = TILE_CHUNK_UNINITIALIZED;

    return result;
}

pub inline fn isValid(p: *const WorldPosition) bool {
    const result = p.chunk_x != TILE_CHUNK_UNINITIALIZED;
    return result;
}

pub inline fn getWorldChunk(
    world: *World,
    chunk_x: i32,
    chunk_y: i32,
    chunk_z: i32,
    arena: ?*MemoryArena,
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
) Vec3 {
    var result: Vec3 = undefined;

    const d_tile = Vec3.init(
        @as(f32, @floatFromInt(a.chunk_x)) -
            @as(f32, @floatFromInt(b.chunk_x)),
        @as(f32, @floatFromInt(a.chunk_y)) -
            @as(f32, @floatFromInt(b.chunk_y)),
        @as(f32, @floatFromInt(a.chunk_z)) -
            @as(f32, @floatFromInt(b.chunk_z)),
    );

    result = Vec3.add(
        &Vec3.hadamard(&world.chunk_dim_in_meters, &d_tile),
        &Vec3.sub(&a.offset_, &b.offset_),
    );

    return result;
}

pub inline fn centeredChunkPoint(
    chunk_x: i32,
    chunk_y: i32,
    chunk_z: i32,
) WorldPosition {
    var result: WorldPosition = .{};

    result.chunk_x = chunk_x;
    result.chunk_y = chunk_y;
    result.chunk_z = chunk_z;

    return result;
}

pub inline fn centeredChunk(chunk: *WorldChunk) WorldPosition {
    const result = centeredChunkPoint(chunk.chunk_x, chunk.chunk_y, chunk.chunk_z);

    return result;
}

pub inline fn changeEntityLocationRaw(
    arena: *MemoryArena,
    world: *World,
    low_entity_index: u32,
    maybe_old_p: ?*const WorldPosition,
    maybe_new_p: ?*const WorldPosition,
) void {
    // TODO: If this moves an entity into the camera bounds, should it
    // automatically go into the high set immediately?
    // If it moves out of the camera bounds, should it be removed from
    // the high set immediately?

    assert(maybe_old_p == null or isValid(maybe_old_p.?));
    assert(maybe_new_p == null or isValid(maybe_new_p.?));

    if (maybe_old_p != null and
        maybe_new_p != null and
        inSameChunk(world, maybe_old_p.?, maybe_new_p.?))
    {
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

        if (maybe_new_p) |new_p| {
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
}

pub inline fn changeEntityLocation(
    arena: *MemoryArena,
    maybe_world: ?*World,
    low_entity_index: u32,
    low: *LowEntity,
    new_p_init: *const WorldPosition,
) void {
    var maybe_old_p: ?*const WorldPosition = null;
    var maybe_new_p: ?*const WorldPosition = null;

    if (isValid(&low.p) and !low.sim.flags.non_spatial) {
        maybe_old_p = &low.p;
    }

    if (isValid(new_p_init)) {
        maybe_new_p = new_p_init;
    }

    if (maybe_world) |world| {
        changeEntityLocationRaw(
            arena,
            world,
            low_entity_index,
            maybe_old_p,
            maybe_new_p,
        );
    }

    if (maybe_new_p) |new_p| {
        low.p = new_p.*;
        low.sim.flags.non_spatial = false;
    } else {
        low.p = nullPosition();
        low.sim.flags.non_spatial = true;
    }
}

inline fn tileRelIsCanonical(
    chunk_dim: f32,
    tile_rel: f32,
) bool {
    // TODO: Fix floating point math so this can be exact
    const epsilon = 0.01;
    const result =
        (tile_rel >= -0.5 * chunk_dim - epsilon) and
        (tile_rel <= 0.5 * chunk_dim + epsilon);

    return result;
}

pub inline fn vecIsCanonical(
    world: *World,
    offset: Vec3,
) bool {
    const result =
        tileRelIsCanonical(world.chunk_dim_in_meters.x(), offset.x()) and
        tileRelIsCanonical(world.chunk_dim_in_meters.y(), offset.y()) and
        tileRelIsCanonical(world.chunk_dim_in_meters.z(), offset.z());

    return result;
}

inline fn recanonicalizeCoordinate(
    chunk_dim: f32,
    tile: *i32,
    tile_rel: *f32,
) void {
    // TODO: Don't use the divide/multiply method for recanonicalizing
    // because this can round back onto the previous tile
    // TODO: Add bounds checking to prevent wrapping

    // NOTE: Wrapping is not allowed, all coordinates are assumed to be within
    // the safe margin
    // TODO: Assert that we are nowhere near the edges of the world
    const offset: i32 = @intFromFloat(@round(tile_rel.* / chunk_dim));
    tile.* += offset;
    tile_rel.* -= @as(f32, @floatFromInt(offset)) * chunk_dim;

    assert(tileRelIsCanonical(chunk_dim, tile_rel.*));
}

pub inline fn mapIntoChunkSpace(
    world: *World,
    base_pos: WorldPosition,
    offset: Vec3,
) WorldPosition {
    var result = base_pos;

    result.offset_ = Vec3.add(&result.offset_, &offset);

    recanonicalizeCoordinate(
        world.chunk_dim_in_meters.x(),
        &result.chunk_x,
        &result.offset_.v[0],
    );
    recanonicalizeCoordinate(
        world.chunk_dim_in_meters.y(),
        &result.chunk_y,
        &result.offset_.v[1],
    );
    recanonicalizeCoordinate(
        world.chunk_dim_in_meters.z(),
        &result.chunk_z,
        &result.offset_.v[2],
    );

    return result;
}

pub inline fn inSameChunk(
    world: *World,
    a: *const WorldPosition,
    b: *const WorldPosition,
) bool {
    assert(vecIsCanonical(world, a.offset_));
    assert(vecIsCanonical(world, b.offset_));

    const result = (a.chunk_x == b.chunk_x and
        a.chunk_y == b.chunk_y and
        a.chunk_z == b.chunk_z);

    return result;
}
