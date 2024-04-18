const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const Vec2 = math.Vec2;

// TODO: Replace this with a Vec3 once we get to Vec3
pub const TileMapDifference = struct {
    dxy: Vec2,
    dz: f64,
};

pub const TileMapPosition = struct {
    // NOTE: These are fixed point tile locations. The high bits are
    // the tile chunk index and the low bits are the tile index in
    // the chunk
    // TODO: Think about what the approach here would be with 3D coordinates
    abs_tile_x: usize,
    abs_tile_y: usize,
    abs_tile_z: usize,
    // NOTE: Offset from tile center
    offset_: Vec2,
};

const TileChunkPosition = struct {
    tile_chunk_x: usize,
    tile_chunk_y: usize,
    tile_chunk_z: usize,
    rel_tile_x: usize,
    rel_tile_y: usize,
};

pub const TileChunk = struct {
    // TODO: Real structure for a tile
    tiles: ?[*]usize = undefined,
};

pub const TileMap = struct {
    chunk_shift: usize,
    chunk_mask: usize,
    chunk_dim: usize,
    tile_side_in_meters: f64,
    // TODO: Real sparseness so anywhere in the world can be represented
    // without the giant pointer array
    tile_chunks: ?[*]TileChunk = undefined,
    tile_chunk_count_x: usize,
    tile_chunk_count_y: usize,
    tile_chunk_count_z: usize,
};

inline fn get_tile_chunk(
    tile_map: *TileMap,
    tile_chunk_x: usize,
    tile_chunk_y: usize,
    tile_chunk_z: usize,
) ?*TileChunk {
    var tile_chunk: ?*TileChunk = null;

    if (tile_chunk_x >= 0 and
        tile_chunk_x < tile_map.tile_chunk_count_x and
        tile_chunk_y >= 0 and
        tile_chunk_y < tile_map.tile_chunk_count_y and
        tile_chunk_z >= 0 and
        tile_chunk_z < tile_map.tile_chunk_count_z)
    {
        const tile_chunk_index =
            tile_chunk_z *
            tile_map.tile_chunk_count_x *
            tile_map.tile_chunk_count_y +
            tile_chunk_y *
            tile_map.tile_chunk_count_x +
            tile_chunk_x;

        if (tile_map.tile_chunks) |tile_chunks| {
            tile_chunk = &tile_chunks[tile_chunk_index];
        }
    }

    return tile_chunk;
}

inline fn get_tile_value_unchecked(
    tile_map: *TileMap,
    tile_chunk: ?*TileChunk,
    tile_x: usize,
    tile_y: usize,
) usize {
    assert(tile_chunk != null);
    assert(tile_x < tile_map.chunk_dim);
    assert(tile_y < tile_map.chunk_dim);

    const tile_index = tile_y * tile_map.chunk_dim + tile_x;
    const tiles = tile_chunk.?.tiles.?;
    const tile_chunk_value = tiles[tile_index];
    return tile_chunk_value;
}

inline fn get_chunk_position(
    tile_map: *TileMap,
    abs_tile_x: usize,
    abs_tile_y: usize,
    abs_tile_z: usize,
) TileChunkPosition {
    var result = std.mem.zeroInit(TileChunkPosition, .{});

    result.tile_chunk_x = abs_tile_x >> @as(u5, @intCast(tile_map.chunk_shift));
    result.tile_chunk_y = abs_tile_y >> @as(u5, @intCast(tile_map.chunk_shift));
    result.tile_chunk_z = abs_tile_z;
    result.rel_tile_x = abs_tile_x & tile_map.chunk_mask;
    result.rel_tile_y = abs_tile_y & tile_map.chunk_mask;

    return result;
}

pub inline fn get_tile_value(
    tile_map: *TileMap,
    abs_tile_x: usize,
    abs_tile_y: usize,
    abs_tile_z: usize,
) usize {
    const chunk_pos = get_chunk_position(
        tile_map,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
    );

    const tile_chunk = get_tile_chunk(
        tile_map,
        chunk_pos.tile_chunk_x,
        chunk_pos.tile_chunk_y,
        chunk_pos.tile_chunk_z,
    );

    const tile_chunk_value = get_tile_chunk_value(
        tile_map,
        tile_chunk,
        chunk_pos.rel_tile_x,
        chunk_pos.rel_tile_y,
    );

    return tile_chunk_value;
}

pub inline fn get_tile_value_from_pos(
    tile_map: *TileMap,
    pos: TileMapPosition,
) usize {
    const tile_chunk_value = get_tile_value(
        tile_map,
        pos.abs_tile_x,
        pos.abs_tile_y,
        pos.abs_tile_z,
    );

    return tile_chunk_value;
}

inline fn get_tile_chunk_value(
    tile_map: *TileMap,
    tile_chunk: ?*TileChunk,
    test_tile_x: usize,
    test_tile_y: usize,
) usize {
    var tile_chunk_value: usize = 0;

    if (tile_chunk) |chunk| {
        if (chunk.tiles != null) {
            tile_chunk_value = get_tile_value_unchecked(
                tile_map,
                chunk,
                test_tile_x,
                test_tile_y,
            );
        }
    }

    return tile_chunk_value;
}

pub inline fn is_tile_value_empty(value: usize) bool {
    const empty =
        (value == 1) or
        (value == 3) or
        (value == 4);

    return empty;
}

pub inline fn is_tile_map_point_empty(
    tile_map: *TileMap,
    tile_map_pos: TileMapPosition,
) bool {
    const tile_chunk_value = get_tile_value(
        tile_map,
        tile_map_pos.abs_tile_x,
        tile_map_pos.abs_tile_y,
        tile_map_pos.abs_tile_z,
    );

    const empty = is_tile_value_empty(tile_chunk_value);

    return empty;
}

pub inline fn set_tile_value(
    arena: *game.MemoryArena,
    tile_map: *TileMap,
    abs_tile_x: usize,
    abs_tile_y: usize,
    abs_tile_z: usize,
    tile_value: usize,
) void {
    const chunk_pos = get_chunk_position(
        tile_map,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
    );

    const tile_chunk = get_tile_chunk(
        tile_map,
        chunk_pos.tile_chunk_x,
        chunk_pos.tile_chunk_y,
        chunk_pos.tile_chunk_z,
    );

    assert(tile_chunk != null);

    if (tile_chunk) |chunk| {
        if (chunk.tiles == null) {
            const tile_count = tile_map.chunk_dim * tile_map.chunk_dim;

            chunk.tiles = game.push_array(
                arena,
                tile_count,
                usize,
            );

            if (chunk.tiles) |tiles| {
                for (0..tile_count) |tile_index| {
                    tiles[tile_index] = 1;
                }
            }
        }
    }

    set_tile_chunk_value(
        tile_map,
        tile_chunk,
        chunk_pos.rel_tile_x,
        chunk_pos.rel_tile_y,
        tile_value,
    );
}

pub inline fn set_tile_chunk_value(
    tile_map: *TileMap,
    tile_chunk: ?*TileChunk,
    test_tile_x: usize,
    test_tile_y: usize,
    tile_value: usize,
) void {
    if (tile_chunk) |chunk| {
        if (chunk.tiles) |_| {
            set_tile_value_unchecked(
                tile_map,
                chunk,
                test_tile_x,
                test_tile_y,
                tile_value,
            );
        }
    }
}

inline fn set_tile_value_unchecked(
    tile_map: *TileMap,
    tile_chunk: ?*TileChunk,
    tile_x: usize,
    tile_y: usize,
    tile_value: usize,
) void {
    assert(tile_chunk != null);
    assert(tile_x < tile_map.chunk_dim);
    assert(tile_y < tile_map.chunk_dim);

    const tile_index = tile_y * tile_map.chunk_dim + tile_x;
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
        .x = @as(f64, @floatFromInt(a.abs_tile_x)) -
            @as(f64, @floatFromInt(b.abs_tile_x)),
        .y = @as(f64, @floatFromInt(a.abs_tile_y)) -
            @as(f64, @floatFromInt(b.abs_tile_y)),
    };

    const d_tile_z = @as(f64, @floatFromInt(a.abs_tile_z)) -
        @as(f64, @floatFromInt(b.abs_tile_z));

    result.dxy.x = tile_map.tile_side_in_meters * d_tile_xy.x +
        (a.offset_.x - b.offset_.x);
    result.dxy.y = tile_map.tile_side_in_meters * d_tile_xy.y +
        (a.offset_.y - b.offset_.y);
    // TODO: Think about what we want to do with z
    result.dz = tile_map.tile_side_in_meters * d_tile_z;

    return result;
}

pub inline fn centered_tile_point(
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

pub inline fn offset_pos(
    tile_map: *TileMap,
    pos: TileMapPosition,
    offset: Vec2,
) TileMapPosition {
    var result = pos;

    result.offset_ = math.add(pos.offset_, offset);
    result = recanonicalize_position(tile_map, result);

    return result;
}

// TODO: Do these functions below belong in a "positioning" or "geometry" import?

inline fn recanonicalize_coordinate(
    tile_map: *TileMap,
    tile: *usize,
    tile_rel: *f64,
) void {
    // TODO: Don't use the divide/multiply method for recanonicalizing
    // because this can round back onto the previous tile
    // TODO: Add bounds checking to prevent wrapping

    // NOTE: TileMap is assumed to be toroidal topology, if you step off
    // one end you wind up on the other
    const offset: i64 = @intFromFloat(@round(tile_rel.* / tile_map.tile_side_in_meters));

    tile.* +%= @as(usize, @bitCast(offset));
    tile_rel.* -= @as(f64, @floatFromInt(offset)) * tile_map.tile_side_in_meters;

    assert(tile_rel.* > -0.5001 * tile_map.tile_side_in_meters);
    // TODO: Fix floating point math so this can be exact
    // NOTE: This assert only seems to trip with Casey's code
    // maybe this would trip if we swapped to f32
    assert(tile_rel.* < 0.5001 * tile_map.tile_side_in_meters);
}

pub inline fn recanonicalize_position(
    tile_map: *TileMap,
    pos: TileMapPosition,
) TileMapPosition {
    var result = pos;

    recanonicalize_coordinate(
        tile_map,
        &result.abs_tile_x,
        &result.offset_.x,
    );

    recanonicalize_coordinate(
        tile_map,
        &result.abs_tile_y,
        &result.offset_.y,
    );

    return result;
}

pub inline fn on_same_tile(
    a: *TileMapPosition,
    b: *TileMapPosition,
) bool {
    const result = (a.abs_tile_x == b.abs_tile_x and
        a.abs_tile_y == b.abs_tile_y and
        a.abs_tile_z == b.abs_tile_z);

    return result;
}
