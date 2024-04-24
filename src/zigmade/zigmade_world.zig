const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const Vec2 = math.Vec2;
const TILE_CHUNK_SAFE_MARGIN = std.math.maxInt(i32) / 64;
const TILE_CHUNK_UNINITIALIZED = std.math.maxInt(i32);

// TODO: Replace this with a Vec3 once we get to Vec3
pub const WorldDifference = struct {
    dxy: Vec2,
    dz: f32,
};

pub const WorldPosition = struct {
    // TODO: How can we get rid of abs_tile_* here,
    // and still allow references to entities to be able to figure out
    // where they are/which world chunk they are in?
    //
    // NOTE: These are fixed point tile locations. The high bits are
    // the tile chunk index and the low bits are the tile index in
    // the chunk
    //
    // TODO: Think about what the approach here would be with 3D coordinates
    abs_tile_x: i32 = 0,
    abs_tile_y: i32 = 0,
    abs_tile_z: i32 = 0,
    // NOTE: Offset from tile center
    offset_: Vec2 = Vec2{},
};

// TODO: Could make this just TileChunk and allow multiple chunks per x/y/z
const WorldEntityBlock = struct {
    entity_count: u32,
    low_entity_index: [16]u32,
    next: ?*WorldEntityBlock,
};

pub const WorldChunk = struct {
    chunk_x: i32,
    chunk_y: i32,
    chunk_z: i32,
    first_block: WorldEntityBlock,
    next_in_hash: ?*WorldChunk = null,
};

pub const World = struct {
    chunk_shift: i32,
    chunk_mask: i32,
    chunk_dim: i32,
    tile_side_in_meters: f32,
    // TODO: tile_chunk_hash should probably switch to pointers if
    // tile entity blocks continue to be store en masse directly
    // inside the tile chunk
    // NOTE: At the moment, this must be a power of two
    chunk_hash: [4096]WorldChunk = undefined,
};

//pub fn getChunkPositionFor(
//    world: *World,
//    abs_tile_x: u32,
//    abs_tile_y: u32,
//    abs_tile_z: u32,
//) WorldPosition {
//    var result = WorldChunk{};
//
//    result.chunk_x = abs_tile_x >> world.chunk_shift;
//    result.chunk_y = abs_tile_y >> world.chunk_shift;
//    result.chunk_z = abs_tile_z;
//    result.rel_tile_x = abs_tile_x & world.chunk_mask;
//    result.rel_tile_y = abs_tile_y & world.chunk_mask;
//
//    return result;
//}

pub fn initializeWorld(world: *World, tile_side_in_meters: f32) void {
    world.chunk_shift = 4;
    world.chunk_mask = (@as(i32, @intCast(1)) <<
        @as(u5, @intCast(world.chunk_shift))) - 1;
    world.chunk_dim = (@as(i32, @intCast(1)) <<
        @as(u5, @intCast(world.chunk_shift)));
    world.tile_side_in_meters = tile_side_in_meters;

    for (0..world.chunk_hash.len) |index| {
        world.chunk_hash[index].chunk_x = TILE_CHUNK_UNINITIALIZED;
    }
}

inline fn getTileChunk(
    world: *World,
    chunk_x: i32,
    chunk_y: i32,
    chunk_z: i32,
    arena: ?*game.MemoryArena,
) ?*WorldChunk {
    var world_chunk: ?*WorldChunk = null;

    assert(chunk_x > -TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_y > -TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_z > -TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_x < TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_y < TILE_CHUNK_SAFE_MARGIN);
    assert(chunk_z < TILE_CHUNK_SAFE_MARGIN);

    // TODO: BETTER HASH FUNCTION
    const hash_value = 19 * chunk_x + 7 * chunk_y + 3 * chunk_z;
    const hash_slot = @as(usize, @intCast(hash_value)) & (world.chunk_hash.len - 1);
    assert(hash_slot < world.chunk_hash.len);
    world_chunk = &world.chunk_hash[hash_slot];

    while (world_chunk) |chunk| : (world_chunk = world_chunk.?.next_in_hash) {
        if ((chunk_x == chunk.chunk_x) and
            (chunk_y == chunk.chunk_y) and
            (chunk_z == chunk.chunk_z))
        {
            break;
        }

        if (arena != null and chunk_x != TILE_CHUNK_UNINITIALIZED and chunk.next_in_hash == null) {
            chunk.next_in_hash = game.pushStruct(arena.?, WorldChunk);
            world_chunk = chunk.next_in_hash;
            world_chunk.?.chunk_x = TILE_CHUNK_UNINITIALIZED;
        }

        if (arena != null and chunk_x == TILE_CHUNK_UNINITIALIZED) {
            chunk.tile_chunk_x = chunk_x;
            chunk.tile_chunk_y = chunk_y;
            chunk.tile_chunk_z = chunk_z;

            chunk.next_in_hash = null;

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
        .x = @as(f32, @floatFromInt(a.abs_tile_x)) -
            @as(f32, @floatFromInt(b.abs_tile_x)),
        .y = @as(f32, @floatFromInt(a.abs_tile_y)) -
            @as(f32, @floatFromInt(b.abs_tile_y)),
    };

    const d_tile_z = @as(f32, @floatFromInt(a.abs_tile_z)) -
        @as(f32, @floatFromInt(b.abs_tile_z));

    result.dxy.x = world.tile_side_in_meters * d_tile_xy.x +
        (a.offset_.x - b.offset_.x);
    result.dxy.y = world.tile_side_in_meters * d_tile_xy.y +
        (a.offset_.y - b.offset_.y);
    // TODO: Think about what we want to do with z
    result.dz = world.tile_side_in_meters * d_tile_z;

    return result;
}

pub inline fn centeredTilePoint(
    abs_tile_x: u32,
    abs_tile_y: u32,
    abs_tile_z: u32,
) WorldPosition {
    var result: WorldPosition = undefined;

    result.abs_tile_x = abs_tile_x;
    result.abs_tile_y = abs_tile_y;
    result.abs_tile_z = abs_tile_z;

    return result;
}

// TODO: Do these functions below belong in a "positioning" or "geometry" import?

inline fn recanonicalizeCoordinate(
    world: *World,
    tile: *i32,
    tile_rel: *f32,
) void {
    // TODO: Don't use the divide/multiply method for recanonicalizing
    // because this can round back onto the previous tile
    // TODO: Add bounds checking to prevent wrapping

    // NOTE: World is assumed to be toroidal topology, if you step off
    // one end you wind up on the other
    const offset: i32 = @intFromFloat(@round(tile_rel.* / world.tile_side_in_meters));
    tile.* +%= @as(i32, @bitCast(offset));
    tile_rel.* -= @as(f32, @floatFromInt(offset)) * world.tile_side_in_meters;

    assert(tile_rel.* > -0.5 * world.tile_side_in_meters);
    // TODO: Fix floating point math so this can be exact
    // NOTE: This assert only seems to trip with Casey's code
    // maybe this would trip if we swapped to f32
    assert(tile_rel.* < 0.5 * world.tile_side_in_meters);
}

pub inline fn mapIntoTileSpace(
    world: *World,
    base_pos: WorldPosition,
    offset: Vec2,
) WorldPosition {
    var result = base_pos;

    result.offset_ = math.add(result.offset_, offset);
    recanonicalizeCoordinate(world, &result.abs_tile_x, &result.offset_.x);
    recanonicalizeCoordinate(world, &result.abs_tile_y, &result.offset_.y);

    return result;
}

pub inline fn onSameTile(
    a: *WorldPosition,
    b: *WorldPosition,
) bool {
    const result = (a.abs_tile_x == b.abs_tile_x and
        a.abs_tile_y == b.abs_tile_y and
        a.abs_tile_z == b.abs_tile_z);

    return result;
}
