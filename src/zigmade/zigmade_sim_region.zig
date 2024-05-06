const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const world = @import("zigmade_world.zig");
const ety = @import("zigmade_entity.zig");

const Vec2 = math.Vec2;
const Rectangle2 = math.Rectangle2;
const LowEntity = game.LowEntity;
const GameState = game.GameState;
const World = world.World;
const WorldPosition = world.WorldPosition;

pub const HIT_POINT_SUB_COUNT = 4;

const HitPoint = struct {
    // TODO: Bake this down into one variable (packed struct?)
    flags: u8,
    filled_amount: u8,
};

pub const MoveSpec = struct {
    unit_max_acc_vector: bool,
    speed: f32,
    drag: f32,
};

pub const EntityType = enum {
    none,
    hero,
    wall,
    familiar,
    monster,
    sword,
};

const EntityReferenceTag = enum {
    ptr,
    index,
};

const EntityReference = union(EntityReferenceTag) {
    ptr: *Entity,
    index: u32,
};

const EntityFlags = packed struct(u32) {
    // TODO: Does it make more sense for this flag to be non_colliding?
    collides: bool = false,
    non_spatial: bool = false,
    simming: bool = false,
    _padding: u29 = 0,
};

pub const Entity = struct {
    // NOTE: These are only for the sim region
    storage_index: u32 = 0,
    updatable: bool = false,
    type: EntityType = .none,
    flags: EntityFlags = .{},
    pos: Vec2 = .{},
    d_pos: Vec2 = .{},
    z: f32 = 0,
    dz: f32 = 0,
    chunk_z: u32 = 0,
    width: f32 = 0,
    height: f32 = 0,
    facing_direction: u32 = 0,
    t_bob: f32 = 0,
    d_abs_tile_z: i32 = 0,
    // TODO: Should hit points themselves be entities?
    hit_point_max: u32 = 0,
    hit_points: [16]HitPoint = undefined,
    sword: EntityReference = .{ .index = 0 },
    distance_remaining: f32 = 0,
    // TODO: Generation index so we know how "up to date" this entity is
};

const SimEntityHash = struct {
    ptr: ?*Entity,
    index: u32,
};

pub const SimRegion = struct {
    // TODO: Need a hash table to map stored entity indices to
    // sim entities
    world: *World,
    origin: WorldPosition,
    bounds: Rectangle2,
    updatable_bounds: Rectangle2,
    max_entity_count: u32,
    entity_count: u32,
    entities: [*]Entity,
    // TODO: Do we really want a hash for this?
    // NOTE: Must be a power of two
    hash: [4096]SimEntityHash,
};

fn getHashFromStorageIndex(region: *SimRegion, storage_index: u32) *SimEntityHash {
    assert(storage_index > 0);

    var result: *SimEntityHash = undefined;

    const hash_value: usize = storage_index;

    for (0..region.hash.len) |offset| {
        const hash_mask = region.hash.len - 1;
        const hash_index = (hash_value + offset) & hash_mask;
        const entry = &region.hash[hash_index];

        if (entry.index == 0 or entry.index == storage_index) {
            result = entry;
            break;
        }
    }

    return result;
}

inline fn getEntityByStorageIndex(
    region: *SimRegion,
    storage_index: u32,
) ?*Entity {
    const entry = getHashFromStorageIndex(region, storage_index);
    const result = entry.ptr;
    return result;
}

inline fn loadEntityReference(
    game_state: *GameState,
    region: *SimRegion,
    ref: *EntityReference,
) void {
    switch (ref.*) {
        .index => {
            if (ref.index > 0) {
                var entry = getHashFromStorageIndex(region, ref.index);

                if (entry.ptr == null) {
                    entry.index = ref.index;
                    const low = game.getLowEntity(game_state, ref.index);
                    var p = getSimSpaceP(region, low);
                    entry.ptr = addEntity(game_state, region, ref.index, low, &p);
                }

                ref.* = .{ .ptr = entry.ptr.? };
            }
        },
        else => {},
    }
}

inline fn storeEntityReference(ref: *EntityReference) void {
    switch (ref.*) {
        .ptr => {
            const copy = ref.ptr;
            ref.* = .{ .index = copy.storage_index };
        },
        else => {},
    }
}

fn addEntityRaw(
    game_state: *GameState,
    region: *SimRegion,
    storage_index: u32,
    maybe_source: ?*LowEntity,
) ?*Entity {
    assert(storage_index > 0);

    var maybe_entity: ?*Entity = null;

    var entry = getHashFromStorageIndex(region, storage_index);

    if (entry.ptr == null) {
        if (region.entity_count < region.max_entity_count) {
            var entity = &region.entities[region.entity_count];
            region.entity_count += 1;

            entry = getHashFromStorageIndex(region, storage_index);

            entry.index = storage_index;
            entry.ptr = entity;

            if (maybe_source) |source| {
                // TODO: This should really be a decompression step, not a copy
                entity.* = source.sim;

                loadEntityReference(game_state, region, &entity.sword);

                assert(!source.sim.flags.simming);
                source.sim.flags.simming = true;
            }

            entity.storage_index = storage_index;
            entity.updatable = false;
            maybe_entity = entity;
        } else {
            std.debug.print("Invalid code path\n", .{});
            assert(false);
        }
    }

    return maybe_entity;
}

inline fn getSimSpaceP(
    sim_region: *SimRegion,
    maybe_stored: ?*LowEntity,
) Vec2 {
    // NOTE: Map entity into camera space
    // TODO: Do we want to set this to signaling NAN in
    // debug to make sure nobody ever uses the position
    // of a nonspatial entity?
    var result = ety.invalidPos();

    if (maybe_stored) |stored| {
        if (!stored.sim.flags.non_spatial) {
            const diff = world.subtract(sim_region.world, &stored.pos, &sim_region.origin);
            result = diff.dxy;
        }
    }

    return result;
}

fn addEntity(
    game_state: *GameState,
    sim_region: *SimRegion,
    storage_index: u32,
    source: ?*LowEntity,
    maybe_sim_pos: ?*Vec2,
) ?*Entity {
    const maybe_dest = addEntityRaw(game_state, sim_region, storage_index, source);

    if (maybe_dest) |dest| {
        if (maybe_sim_pos) |sim_pos| {
            dest.pos = sim_pos.*;
            dest.updatable = math.isInRectangle(sim_region.updatable_bounds, dest.pos);
        } else {
            dest.pos = getSimSpaceP(sim_region, source);
        }
    }

    return maybe_dest;
}

pub fn beginSim(
    arena: *game.MemoryArena,
    game_state: *GameState,
    game_world: *World,
    origin: WorldPosition,
    bounds: Rectangle2,
) *SimRegion {
    // TODO: If entities are stored in the world, we wouldn't need game state here

    var sim_region = game.pushStruct(arena, SimRegion);
    game.zeroStruct(@TypeOf(sim_region.hash), &sim_region.hash);

    // TODO: IMPORTANT: Calculate this eventually from the maximum value
    // of all entities radii plus their speed
    const update_safety_margin: f32 = 1.0;

    sim_region.world = game_world;
    sim_region.origin = origin;
    sim_region.updatable_bounds = bounds;

    sim_region.bounds = math.addRadius(
        sim_region.updatable_bounds,
        update_safety_margin,
        update_safety_margin,
    );

    // TODO: need to be more specific about entity counts
    sim_region.max_entity_count = 1024;
    sim_region.entity_count = 0;

    sim_region.entities = game.pushArray(arena, sim_region.max_entity_count, Entity);

    const min_chunk_p = world.mapIntoChunkSpace(
        game_world,
        sim_region.origin,
        math.getMinCorner(sim_region.bounds),
    );

    const max_chunk_p = world.mapIntoChunkSpace(
        game_world,
        sim_region.origin,
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

                        if (!low.sim.flags.non_spatial) {
                            var sim_space_p = getSimSpaceP(sim_region, low);

                            if (math.isInRectangle(sim_region.bounds, sim_space_p)) {
                                _ = addEntity(game_state, sim_region, low_index, low, &sim_space_p);
                            }
                        }
                    }
                }
            }
        }
    }

    return sim_region;
}

pub fn endSim(
    region: *SimRegion,
    game_state: *game.GameState,
) void {
    // TODO: Maybe don't take a game state here, low entities should be stored in the world?

    if (game_state.world) |game_world| {
        for (0..region.entity_count) |index| {
            const entity = &region.entities[index];

            var stored = &game_state.low_entities[entity.storage_index];

            assert(stored.sim.flags.simming);
            stored.sim = entity.*;
            assert(!stored.sim.flags.simming);

            storeEntityReference(&stored.sim.sword);

            // TODO: Save state back to the stored entity, once high entities
            // do state decompression, etc.

            const new_p = if (entity.flags.non_spatial)
                world.nullPosition()
            else
                world.mapIntoChunkSpace(game_world, region.origin, entity.pos);

            world.changeEntityLocation(
                &game_state.world_arena,
                game_world,
                entity.storage_index,
                &game_state.low_entities[entity.storage_index],
                new_p,
            );

            if (entity.storage_index == game_state.camera_entity_index) {
                var new_camera_p = game_state.camera_p;

                new_camera_p.chunk_z = stored.pos.chunk_z;

                if (false) {
                    if (stored.pos.x > (9.0 * game_world.tile_side_in_meters)) {
                        new_camera_p.abs_tile_x +%= 17;
                    }

                    if (stored.pos.x < -(9.0 * game_world.tile_side_in_meters)) {
                        new_camera_p.abs_tile_x -%= 17;
                    }

                    if (stored.pos.y > (5.0 * game_world.tile_side_in_meters)) {
                        new_camera_p.abs_tile_y +%= 9;
                    }

                    if (stored.pos.y < -(5.0 * game_world.tile_side_in_meters)) {
                        new_camera_p.abs_tile_y -%= 9;
                    }
                } else {
                    new_camera_p = stored.pos;
                }

                game_state.camera_p = new_camera_p;
            }
        }
    }
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

pub fn moveEntity(
    region: *SimRegion,
    entity: *Entity,
    dt: f32,
    move_spec: *MoveSpec,
    dd_pos: Vec2,
) void {
    assert(!entity.flags.non_spatial);

    var acceleration = dd_pos;

    if (move_spec.unit_max_acc_vector) {
        const acc_length = math.lengthSquared(acceleration);

        if (acc_length > 1.0) {
            acceleration = math.scale(acceleration, 1.0 / @sqrt(acc_length));
        }
    }

    acceleration = math.scale(acceleration, move_spec.speed);

    // TODO: ODE here
    acceleration = math.add(
        acceleration,
        math.scale(entity.d_pos, -move_spec.drag),
    );

    var player_delta = math.add(
        math.scale(acceleration, 0.5 * math.square(dt)),
        math.scale(entity.d_pos, dt),
    );

    entity.d_pos = math.add(
        math.scale(acceleration, dt),
        entity.d_pos,
    );

    const ddz = -9.8;
    entity.z = 0.5 * ddz * math.square(dt) + entity.dz * dt + entity.z;
    entity.dz = ddz * dt + entity.dz;

    if (entity.z < 0) {
        entity.z = 0;
    }

    for (0..4) |_| {
        var t_min: f32 = 1.0;
        var wall_normal: Vec2 = .{};
        var hit_entity: ?*Entity = null;
        const desired_position = math.add(entity.pos, player_delta);

        if (entity.flags.collides and
            !entity.flags.non_spatial)
        {
            // TODO: Spatial partition here!
            for (1..region.entity_count) |high_index| {
                const test_entity = &region.entities[high_index];
                if (entity != test_entity) {
                    if (test_entity.flags.collides and
                        !test_entity.flags.non_spatial)
                    {
                        const diameter_w = test_entity.width + entity.width;
                        const diameter_h = test_entity.height + entity.height;
                        const min_corner = math.scale(.{ .x = diameter_w, .y = diameter_h }, -0.5);
                        const max_corner = math.scale(.{ .x = diameter_w, .y = diameter_h }, 0.5);
                        const rel = math.sub(entity.pos, test_entity.pos);

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
                            wall_normal = .{ .x = -1, .y = 0 };
                            hit_entity = test_entity;
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
                            wall_normal = .{ .x = 1, .y = 0 };
                            hit_entity = test_entity;
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
                            wall_normal = .{ .x = 0, .y = -1 };
                            hit_entity = test_entity;
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
                            wall_normal = .{ .x = 0, .y = 1 };
                            hit_entity = test_entity;
                        }
                    }
                }
            }
        }

        entity.pos = math.add(
            entity.pos,
            math.scale(player_delta, t_min),
        );

        if (hit_entity) |_| {
            entity.d_pos = math.sub(
                entity.d_pos,
                math.scale(
                    wall_normal,
                    1 * math.inner(entity.d_pos, wall_normal),
                ),
            );

            player_delta = math.sub(desired_position, entity.pos);

            player_delta = math.sub(
                player_delta,
                math.scale(
                    wall_normal,
                    1 * math.inner(player_delta, wall_normal),
                ),
            );

            // TODO: stairs
            //high.abs_tile_z += @bitCast(hit_low.d_abs_tile_z);
        } else {
            break;
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
