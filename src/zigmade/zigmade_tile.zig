const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");

pub const TileMapPosition = struct {
    // NOTE: These are fixed point tile locations. The high bits are
    // the tile chunk index and the low bits are the tile index in
    // the chunk
    // TODO: Think about what the approach here would be 3D coordinates
    abs_tile_x: usize,
    abs_tile_y: usize,
    // TODO: Should these be from the center of a tile?
    // TODO: Rename to offset x and y
    rel_tile_x: f32,
    rel_tile_y: f32,
};

const TileChunkPosition = struct {
    tile_chunk_x: usize,
    tile_chunk_y: usize,
    rel_tile_x: usize,
    rel_tile_y: usize,
};

pub const TileChunk = struct {
    tiles: ?[*]usize = undefined,
};

pub const TileMap = struct {
    chunk_shift: usize,
    chunk_mask: usize,
    chunk_dim: usize,
    tile_side_in_meters: f32,
    tile_side_in_pixels: i32,
    meters_to_pixels: f32,
    // TODO: Beginner's sparseness
    tile_chunks: ?[*]TileChunk = undefined,
    tile_chunk_count_x: usize,
    tile_chunk_count_y: usize,
};

inline fn get_tile_chunk(
    world: *TileMap,
    tile_chunk_x: usize,
    tile_chunk_y: usize,
) ?*TileChunk {
    var tile_chunk: ?*TileChunk = null;

    if (tile_chunk_x >= 0 and
        tile_chunk_x < world.tile_chunk_count_x and
        tile_chunk_y >= 0 and
        tile_chunk_y < world.tile_chunk_count_y)
    {
        const tile_chunk_index =
            tile_chunk_y *
            world.tile_chunk_count_x +
            tile_chunk_x;

        if (world.tile_chunks) |tile_chunks| {
            tile_chunk = &tile_chunks[tile_chunk_index];
        }
    }

    return tile_chunk;
}

inline fn get_tile_value_unchecked(
    world: *TileMap,
    tile_chunk: ?*TileChunk,
    tile_x: usize,
    tile_y: usize,
) usize {
    assert(tile_chunk != null);
    assert(tile_x < world.chunk_dim);
    assert(tile_y < world.chunk_dim);

    const tile_index = tile_y * world.chunk_dim + tile_x;
    const tiles = tile_chunk.?.tiles.?;
    const tile_chunk_value = tiles[tile_index];
    return tile_chunk_value;
}

inline fn get_chunk_position(
    world: *TileMap,
    abs_tile_x: usize,
    abs_tile_y: usize,
) TileChunkPosition {
    var result = std.mem.zeroInit(TileChunkPosition, .{});

    result.tile_chunk_x = abs_tile_x >> @as(u5, @intCast(world.chunk_shift));
    result.tile_chunk_y = abs_tile_y >> @as(u5, @intCast(world.chunk_shift));
    result.rel_tile_x = abs_tile_x & world.chunk_mask;
    result.rel_tile_y = abs_tile_y & world.chunk_mask;

    return result;
}

pub inline fn get_tile_value(
    world: *TileMap,
    abs_tile_x: usize,
    abs_tile_y: usize,
) usize {
    const chunk_pos = get_chunk_position(world, abs_tile_x, abs_tile_y);

    const tile_chunk = get_tile_chunk(
        world,
        chunk_pos.tile_chunk_x,
        chunk_pos.tile_chunk_y,
    );

    const tile_chunk_value = get_tile_chunk_value(
        world,
        tile_chunk,
        chunk_pos.rel_tile_x,
        chunk_pos.rel_tile_y,
    );

    return tile_chunk_value;
}

inline fn get_tile_chunk_value(
    world: *TileMap,
    tile_chunk: ?*TileChunk,
    test_tile_x: usize,
    test_tile_y: usize,
) usize {
    var tile_chunk_value: usize = 0;

    if (tile_chunk) |chunk| {
        tile_chunk_value = get_tile_value_unchecked(
            world,
            chunk,
            test_tile_x,
            test_tile_y,
        );
    }

    return tile_chunk_value;
}

pub inline fn is_world_point_empty(
    world: *TileMap,
    world_pos: TileMapPosition,
) bool {
    const tile_chunk_value = get_tile_value(
        world,
        world_pos.abs_tile_x,
        world_pos.abs_tile_y,
    );

    const empty = (tile_chunk_value == 0);

    return empty;
}

inline fn recanonicalize_coordinate(
    world: *TileMap,
    tile: *usize,
    tile_rel: *f32,
) void {
    // TODO: Don't use the divide/multiply method for recanonicalizing
    // because this can round back onto the previous tile
    // TODO: Add bounds checking to prevent wrapping

    // NOTE: TileMap is assumed to be toroidal topology, if you step off
    // one end you wind up on the other
    const offset: i64 = @intFromFloat(@round(tile_rel.* / world.tile_side_in_meters));

    tile.* +%= @as(usize, @bitCast(offset));
    tile_rel.* -= @as(f32, @floatFromInt(offset)) * world.tile_side_in_meters;

    assert(tile_rel.* >= -0.5 * world.tile_side_in_meters);
    // TODO: Fix floating point math so this can be <
    // NOTE: With <, this assert only seems to trip with Casey's code
    assert(tile_rel.* <= 0.5 * world.tile_side_in_meters);
}

pub inline fn recanonicalize_position(
    world: *TileMap,
    pos: TileMapPosition,
) TileMapPosition {
    var result = pos;

    recanonicalize_coordinate(
        world,
        &result.abs_tile_x,
        &result.rel_tile_x,
    );

    recanonicalize_coordinate(
        world,
        &result.abs_tile_y,
        &result.rel_tile_y,
    );

    return result;
}

pub inline fn set_tile_value(
    arena: *game.MemoryArena,
    tile_map: *TileMap,
    abs_tile_x: usize,
    abs_tile_y: usize,
    tile_value: usize,
) void {
    _ = arena;
    const chunk_pos = get_chunk_position(tile_map, abs_tile_x, abs_tile_y);

    const tile_chunk = get_tile_chunk(
        tile_map,
        chunk_pos.tile_chunk_x,
        chunk_pos.tile_chunk_y,
    );

    // TODO: On-demand tile chunk creation
    assert(tile_chunk != null);

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
        set_tile_value_unchecked(
            tile_map,
            chunk,
            test_tile_x,
            test_tile_y,
            tile_value,
        );
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
