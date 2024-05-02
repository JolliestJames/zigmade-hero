const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const world = @import("zigmade_world.zig");

const Vec2 = math.Vec2;
const Rectangle2 = math.Rectangle2;
const LowEntity = game.LowEntity;
const GameState = game.GameState;
const World = world.World;
const WorldPosition = world.WorldPosition;

const SimArena = struct {};

const SimEntity = struct {
    storage_index: u32,
    pos: Vec2 = .{},
    d_pos: Vec2 = .{},
    chunk_z: u32 = 0,
    z: f32 = 0,
    dz: f32 = 0,
};

//const StoredEntity = struct {};

const SimRegion = struct {
    // TODO: Need a hash table to map stored entity indices to
    // sim entities
    world: *game.World,
    origin: WorldPosition,
    bounds: Rectangle2,
    max_entity_count: u32,
    entity_count: u32,
    entities: [256]SimEntity,
};

fn addEntity(sim_region: *SimRegion) *SimEntity {
    var entity: *SimEntity = undefined;

    if (sim_region.entity_count < sim_region.max_entity_count) {
        entity = sim_region.entities[sim_region.entity_count];
        sim_region.entity_count += 1;

        // TODO: See what we want to do about clearing policy when
        // the entity system is more fleshed out
        entity = .{};
    } else {
        std.debug.print("Invalid code path\n", .{});
        assert(false);
    }

    return entity;
}

inline fn getSimSpaceP(
    sim_region: *SimRegion,
    stored: *LowEntity,
) Vec2 {
    const game_world = sim_region.world.?;

    // NOTE: Map entity into camera space
    const diff = world.subtract(game_world, &stored.pos, &sim_region.origin);
    const result = diff.dxy;

    return result;
}

fn addStoredEntity(
    sim_region: *SimRegion,
    source: *LowEntity,
    maybe_sim_pos: ?*Vec2,
) *SimEntity {
    const maybe_dest: ?*SimEntity = addEntity(sim_region);

    if (maybe_dest) |dest| {
        // TODO: Convert the stored entity to a simluation entity
        if (maybe_sim_pos) |sim_pos| {
            dest.pos = sim_pos.*;
        } else {
            dest.pos = getSimSpaceP(sim_region, source);
        }
    } else {
        std.debug.print("Invalid code path\n", .{});
        assert(false);
    }

    return maybe_dest;
}

fn beginSim(
    arena: *SimArena,
    game_state: *GameState,
    game_world: *World,
    origin: WorldPosition,
    bounds: Rectangle2,
) *SimRegion {
    // TODO: If entities are stored in the world, we wouldn't need game state here

    var sim_region = game.pushStruct(arena, SimRegion);

    sim_region = .{
        .world = game_world,
        .origin = origin,
        .bounds = bounds,
        // TODO: need to be more specific about entity counts
        .max_entity_count = 1024,
        .entity_count = 0,
    };

    sim_region.entities = game.pushArray(arena, sim_region.max_entity_count, SimEntity);

    const min_chunk_p = world.mapIntoChunkSpace(
        game_world,
        sim_region.center,
        math.getMinCorner(sim_region.bounds),
    );

    const max_chunk_p = world.mapIntoChunkSpace(
        game_world,
        sim_region.center,
        math.getMaxCorner(sim_region.bounds),
    );

    var chunk_y = min_chunk_p.chunk_y;

    while (chunk_y <= max_chunk_p.chunk_y) : (chunk_y += 1) {
        var chunk_x = min_chunk_p.chunk_x;

        while (chunk_x <= max_chunk_p.chunk_x) : (chunk_x += 1) {
            const chunk = world.getWorldChunk(
                game_world,
                chunk_x,
                chunk_y,
                sim_region.origin.chunk_z,
                null,
            );

            if (chunk) |c| {
                var block: ?*world.WorldEntityBlock = &c.first_block;

                while (block) |b| : (block = b.next) {
                    for (0..b.entity_count) |entity_index| {
                        const low_index = b.low_entity_index[entity_index];
                        const low = &game_state.low_entities[low_index];

                        const sim_space_p = getSimSpaceP(sim_region, low);

                        if (math.isInRectangle(sim_region.bounds, sim_space_p)) {
                            // TODO: Check a second rectangle to set the entity
                            // to be "movable" or not
                            addEntity(sim_region, low, sim_space_p);
                        }
                    }
                }
            }
        }
    }
}

fn endSim(
    region: *SimRegion,
    game_state: *game.GameState,
) void {
    // TODO: Maybe don't take a game state here, low entities should be stored in the world?
    if (game_state.world) |game_world| {
        for (0..region.entity_count) |index| {
            const entity = &region.entities[index];

            var stored = game_state.low_entities[entity.storage_index];

            // TODO: Save state back to the stored entity, once high entities
            // do state decompression, etc.

            var new_p = world.mapIntoChunkSpace(game_world, region.origin, entity.pos);

            world.changeEntityLocation(
                &game_state.world_arena,
                game_state.world,
                entity.storage_index,
                entity,
                &stored.pos,
                &new_p,
            );

            // TODO: Entity mapping hash table
            const camera_following_entity = game.forceEntityIntoHigh(
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
                    new_camera_p = low.pos;
                }

                // TODO: Map new entities in and old entities out
                // TODO: Mapping tiles and stairs into the entity set

                setCamera(game_state, &new_camera_p);
            }
        }
    }
}
