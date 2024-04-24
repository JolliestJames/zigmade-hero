const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const Vec2 = math.Vec2;
const TILE_CHUNK_SAFE_MARGIN = std.math.maxInt(i32) / 64;
const TILE_CHUNK_UNINITIALIZED = std.math.maxInt(i32);

// TODO: Replace this with a Vec3 once we get to Vec3
pub const TileMapDifference = struct {
    dxy: Vec2,
    dz: f32,
};

pub const TileMapPosition = struct {
    // NOTE: These are fixed point tile locations. The high bits are
    // the tile chunk index and the low bits are the tile index in
    // the chunk
    // TODO: Think about what the approach here would be with 3D coordinates
    abs_tile_x: i32 = 0,
    abs_tile_y: i32 = 0,
    abs_tile_z: i32 = 0,
    // NOTE: Offset from tile center
    offset_: Vec2 = Vec2{},
};

const TileChunkPosition = struct {
    tile_chunk_x: i32,
    tile_chunk_y: i32,
    tile_chunk_z: i32,
    rel_tile_x: i32,
    rel_tile_y: i32,
};

pub const TileChunk = struct {
    tile_chunk_x: i32,
    tile_chunk_y: i32,
    tile_chunk_z: i32,
    // TODO: Real structure for a tile
    tiles: ?[*]usize = undefined,
    next_in_hash: ?*TileChunk = null,
};

pub const TileMap = struct {
    chunk_shift: i32,
    chunk_mask: i32,
    chunk_dim: i32,
    tile_side_in_meters: f32,
    // NOTE: At the moment, this must be a power of two
    tile_chunk_hash: [4096]TileChunk = undefined,
};

pub fn initializeTileMap(tile_map: *TileMap, tile_side_in_meters: f32) void {
    tile_map.chunk_shift = 4;
    tile_map.chunk_mask = (@as(i32, @intCast(1)) <<
        @as(u5, @intCast(tile_map.chunk_shift))) - 1;
    tile_map.chunk_dim = (@as(i32, @intCast(1)) <<
        @as(u5, @intCast(tile_map.chunk_shift)));
    tile_map.tile_side_in_meters = tile_side_in_meters;

    for (0..tile_map.tile_chunk_hash.len) |index| {
        tile_map.tile_chunk_hash[index].tile_chunk_x = TILE_CHUNK_UNINITIALIZED;
    }
}

inline fn getTileChunk(
    tile_map: *TileMap,
    tile_chunk_x: i32,
    tile_chunk_y: i32,
    tile_chunk_z: i32,
    arena: ?*game.MemoryArena,
) ?*TileChunk {
    var tile_chunk: ?*TileChunk = null;

    assert(tile_chunk_x > -TILE_CHUNK_SAFE_MARGIN);
    assert(tile_chunk_y > -TILE_CHUNK_SAFE_MARGIN);
    assert(tile_chunk_z > -TILE_CHUNK_SAFE_MARGIN);
    assert(tile_chunk_x < TILE_CHUNK_SAFE_MARGIN);
    assert(tile_chunk_y < TILE_CHUNK_SAFE_MARGIN);
    assert(tile_chunk_z < TILE_CHUNK_SAFE_MARGIN);

    // TODO: BETTER HASH FUNCTION
    const hash_value = 19 * tile_chunk_x + 7 * tile_chunk_y + 3 * tile_chunk_z;
    const hash_slot = @as(usize, @intCast(hash_value)) & (tile_map.tile_chunk_hash.len - 1);
    assert(hash_slot < tile_map.tile_chunk_hash.len);
    tile_chunk = &tile_map.tile_chunk_hash[hash_slot];

    while (tile_chunk) |chunk| : (tile_chunk = tile_chunk.?.next_in_hash) {
        if ((tile_chunk_x == chunk.tile_chunk_x) and
            (tile_chunk_y == chunk.tile_chunk_y) and
            (tile_chunk_z == chunk.tile_chunk_z))
        {
            break;
        }

        if (arena != null and tile_chunk_x != TILE_CHUNK_UNINITIALIZED and chunk.next_in_hash == null) {
            chunk.next_in_hash = game.pushStruct(arena.?, TileChunk);
            tile_chunk = chunk.next_in_hash;
            tile_chunk.?.tile_chunk_x = TILE_CHUNK_UNINITIALIZED;
        }

        if (arena != null and tile_chunk_x == TILE_CHUNK_UNINITIALIZED) {
            const tile_count: usize = @intCast(tile_map.chunk_dim * tile_map.chunk_dim);
            chunk.tile_chunk_x = tile_chunk_x;
            chunk.tile_chunk_y = tile_chunk_y;
            chunk.tile_chunk_z = tile_chunk_z;
            chunk.tiles = game.pushArray(arena.?, tile_count, usize);

            // TODO: Do we always want to initialize?
            if (chunk.tiles) |tiles| {
                for (0..tile_count) |tile_index| {
                    tiles[tile_index] = 1;
                }
            }

            chunk.next_in_hash = null;

            break;
        }
    }

    return tile_chunk;
}

inline fn getTileValueUnchecked(
    tile_map: *TileMap,
    tile_chunk: ?*TileChunk,
    tile_x: i32,
    tile_y: i32,
) usize {
    assert(tile_chunk != null);
    assert(tile_x < tile_map.chunk_dim);
    assert(tile_y < tile_map.chunk_dim);

    const tile_index = tile_y * tile_map.chunk_dim + tile_x;
    const tiles = tile_chunk.?.tiles.?;
    const tile_chunk_value = tiles[tile_index];
    return tile_chunk_value;
}

inline fn getChunkPosition(
    tile_map: *TileMap,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) TileChunkPosition {
    var result = std.mem.zeroInit(TileChunkPosition, .{});

    result.tile_chunk_x = abs_tile_x >> @as(u5, @intCast(tile_map.chunk_shift));
    result.tile_chunk_y = abs_tile_y >> @as(u5, @intCast(tile_map.chunk_shift));
    result.tile_chunk_z = abs_tile_z;
    result.rel_tile_x = abs_tile_x & tile_map.chunk_mask;
    result.rel_tile_y = abs_tile_y & tile_map.chunk_mask;

    return result;
}

pub inline fn getTileValue(
    tile_map: *TileMap,
    abs_tile_x: usize,
    abs_tile_y: usize,
    abs_tile_z: usize,
) usize {
    const chunk_pos = getChunkPosition(
        tile_map,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
    );

    const tile_chunk = getTileChunk(
        tile_map,
        chunk_pos.tile_chunk_x,
        chunk_pos.tile_chunk_y,
        chunk_pos.tile_chunk_z,
    );

    const tile_chunk_value = getTileChunkValue(
        tile_map,
        tile_chunk,
        chunk_pos.rel_tile_x,
        chunk_pos.rel_tile_y,
    );

    return tile_chunk_value;
}

pub inline fn getTileValueFromPos(
    tile_map: *TileMap,
    pos: TileMapPosition,
) usize {
    const tile_chunk_value = getTileValue(
        tile_map,
        pos.abs_tile_x,
        pos.abs_tile_y,
        pos.abs_tile_z,
    );

    return tile_chunk_value;
}

inline fn getTileChunkValue(
    tile_map: *TileMap,
    tile_chunk: ?*TileChunk,
    test_tile_x: usize,
    test_tile_y: usize,
) usize {
    var tile_chunk_value: usize = 0;

    if (tile_chunk) |chunk| {
        if (chunk.tiles != null) {
            tile_chunk_value = getTileValueUnchecked(
                tile_map,
                chunk,
                test_tile_x,
                test_tile_y,
            );
        }
    }

    return tile_chunk_value;
}

pub inline fn isTileValueEmpty(value: usize) bool {
    const empty =
        (value == 1) or
        (value == 3) or
        (value == 4);

    return empty;
}

pub inline fn isTileMapPointEmpty(
    tile_map: *TileMap,
    tile_map_pos: TileMapPosition,
) bool {
    const tile_chunk_value = getTileValue(
        tile_map,
        tile_map_pos.abs_tile_x,
        tile_map_pos.abs_tile_y,
        tile_map_pos.abs_tile_z,
    );

    const empty = isTileValueEmpty(tile_chunk_value);

    return empty;
}

pub inline fn setTileValue(
    arena: *game.MemoryArena,
    tile_map: *TileMap,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
    tile_value: usize,
) void {
    const chunk_pos = getChunkPosition(
        tile_map,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
    );

    const tile_chunk = getTileChunk(
        tile_map,
        chunk_pos.tile_chunk_x,
        chunk_pos.tile_chunk_y,
        chunk_pos.tile_chunk_z,
        arena,
    );

    setTileChunkValue(
        tile_map,
        tile_chunk,
        chunk_pos.rel_tile_x,
        chunk_pos.rel_tile_y,
        tile_value,
    );
}

pub inline fn setTileChunkValue(
    tile_map: *TileMap,
    tile_chunk: ?*TileChunk,
    test_tile_x: i32,
    test_tile_y: i32,
    tile_value: usize,
) void {
    if (tile_chunk) |chunk| {
        if (chunk.tiles) |_| {
            setTileValueUnchecked(
                tile_map,
                chunk,
                test_tile_x,
                test_tile_y,
                tile_value,
            );
        }
    }
}

inline fn setTileValueUnchecked(
    tile_map: *TileMap,
    tile_chunk: ?*TileChunk,
    tile_x: i32,
    tile_y: i32,
    tile_value: usize,
) void {
    assert(tile_chunk != null);
    assert(tile_x < tile_map.chunk_dim);
    assert(tile_y < tile_map.chunk_dim);

    const tile_index: usize = @intCast(tile_y * tile_map.chunk_dim + tile_x);
    var tiles = tile_chunk.?.tiles.?;
    tiles[tile_index] = tile_value;
}

pub inline fn subtract(
    tile_map: *TileMap,
    a: *TileMapPosition,
    b: *TileMapPosition,
) TileMapDifference {
    var result: TileMapDifference = undefined;

    const d_tile_xy = Vec2{
        .x = @as(f32, @floatFromInt(a.abs_tile_x)) -
            @as(f32, @floatFromInt(b.abs_tile_x)),
        .y = @as(f32, @floatFromInt(a.abs_tile_y)) -
            @as(f32, @floatFromInt(b.abs_tile_y)),
    };

    const d_tile_z = @as(f32, @floatFromInt(a.abs_tile_z)) -
        @as(f32, @floatFromInt(b.abs_tile_z));

    result.dxy.x = tile_map.tile_side_in_meters * d_tile_xy.x +
        (a.offset_.x - b.offset_.x);
    result.dxy.y = tile_map.tile_side_in_meters * d_tile_xy.y +
        (a.offset_.y - b.offset_.y);
    // TODO: Think about what we want to do with z
    result.dz = tile_map.tile_side_in_meters * d_tile_z;

    return result;
}

pub inline fn centeredTilePoint(
    abs_tile_x: usize,
    abs_tile_y: usize,
    abs_tile_z: usize,
) TileMapPosition {
    var result: TileMapPosition = undefined;

    result.abs_tile_x = abs_tile_x;
    result.abs_tile_y = abs_tile_y;
    result.abs_tile_z = abs_tile_z;

    return result;
}

// TODO: Do these functions below belong in a "positioning" or "geometry" import?

inline fn recanonicalizeCoordinate(
    tile_map: *TileMap,
    tile: *i32,
    tile_rel: *f32,
) void {
    // TODO: Don't use the divide/multiply method for recanonicalizing
    // because this can round back onto the previous tile
    // TODO: Add bounds checking to prevent wrapping

    // NOTE: TileMap is assumed to be toroidal topology, if you step off
    // one end you wind up on the other
    const offset: i32 = @intFromFloat(@round(tile_rel.* / tile_map.tile_side_in_meters));
    tile.* +%= @as(i32, @bitCast(offset));
    tile_rel.* -= @as(f32, @floatFromInt(offset)) * tile_map.tile_side_in_meters;

    assert(tile_rel.* > -0.5 * tile_map.tile_side_in_meters);
    // TODO: Fix floating point math so this can be exact
    // NOTE: This assert only seems to trip with Casey's code
    // maybe this would trip if we swapped to f32
    assert(tile_rel.* < 0.5 * tile_map.tile_side_in_meters);
}

pub inline fn mapIntoTileSpace(
    tile_map: *TileMap,
    base_pos: TileMapPosition,
    offset: Vec2,
) TileMapPosition {
    var result = base_pos;

    result.offset_ = math.add(result.offset_, offset);
    recanonicalizeCoordinate(tile_map, &result.abs_tile_x, &result.offset_.x);
    recanonicalizeCoordinate(tile_map, &result.abs_tile_y, &result.offset_.y);

    return result;
}

pub inline fn onSameTile(
    a: *TileMapPosition,
    b: *TileMapPosition,
) bool {
    const result = (a.abs_tile_x == b.abs_tile_x and
        a.abs_tile_y == b.abs_tile_y and
        a.abs_tile_z == b.abs_tile_z);

    return result;
}
