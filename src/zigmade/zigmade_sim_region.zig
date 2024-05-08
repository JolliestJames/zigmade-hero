const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const world = @import("zigmade_world.zig");
const ety = @import("zigmade_entity.zig");

const Vec3 = math.Vec3;
const Rectangle3 = math.Rectangle3;
const LowEntity = game.LowEntity;
const GameState = game.GameState;
const PairwiseCollisionRule = game.PairwiseCollisionRule;
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
    pos: Vec3 = Vec3.splat(0),
    d_pos: Vec3 = Vec3.splat(0),
    distance_limit: f32 = 0,
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
    bounds: Rectangle3,
    updatable_bounds: Rectangle3,
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
) Vec3 {
    // NOTE: Map entity into camera space
    // TODO: Do we want to set this to signaling NAN in
    // debug to make sure nobody ever uses the position
    // of a nonspatial entity?
    var result = ety.invalidPos();

    if (maybe_stored) |stored| {
        if (!stored.sim.flags.non_spatial) {
            result = world.subtract(sim_region.world, &stored.pos, &sim_region.origin);
        }
    }

    return result;
}

fn addEntity(
    game_state: *GameState,
    sim_region: *SimRegion,
    storage_index: u32,
    source: ?*LowEntity,
    maybe_sim_pos: ?*Vec3,
) ?*Entity {
    const maybe_dest = addEntityRaw(game_state, sim_region, storage_index, source);

    if (maybe_dest) |dest| {
        if (maybe_sim_pos) |sim_pos| {
            dest.pos = sim_pos.*;
            dest.updatable = Rectangle3.isInRectangle(sim_region.updatable_bounds, dest.pos);
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
    bounds: Rectangle3,
) *SimRegion {
    // TODO: If entities are stored in the world, we wouldn't need game state here

    var sim_region = game.pushStruct(arena, SimRegion);
    game.zeroStruct(@TypeOf(sim_region.hash), &sim_region.hash);

    // TODO: IMPORTANT: Calculate this eventually from the maximum value
    // of all entities radii plus their speed
    const update_safety_margin: f32 = 1.0;
    const update_safety_margin_z: f32 = 1.0;

    sim_region.world = game_world;
    sim_region.origin = origin;
    sim_region.updatable_bounds = bounds;

    sim_region.bounds = Rectangle3.addRadius(
        sim_region.updatable_bounds,
        Vec3.init(
            update_safety_margin,
            update_safety_margin,
            update_safety_margin_z,
        ),
    );

    // TODO: need to be more specific about entity counts
    sim_region.max_entity_count = 1024;
    sim_region.entity_count = 0;

    sim_region.entities = game.pushArray(arena, sim_region.max_entity_count, Entity);

    const min_corner = Rectangle3.getMinCorner(sim_region.bounds);
    const max_corner = Rectangle3.getMaxCorner(sim_region.bounds);

    const min_chunk_p = world.mapIntoChunkSpace(
        game_world,
        sim_region.origin,
        min_corner,
    );

    const max_chunk_p = world.mapIntoChunkSpace(
        game_world,
        sim_region.origin,
        max_corner,
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

                            if (Rectangle3.isInRectangle(sim_region.bounds, sim_space_p)) {
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

fn shouldCollide(
    game_state: *GameState,
    entity_a: *Entity,
    entity_b: *Entity,
) bool {
    var result = false;

    var a = entity_a;
    var b = entity_b;

    if (a != b) {
        if (a.storage_index > b.storage_index) {
            const temp = a;
            a = b;
            b = temp;
        }

        if (!a.flags.non_spatial and
            !b.flags.non_spatial)
        {
            // TODO: Property-based logic goes here
            result = true;
        }

        // TODO: BETTER HASH FUNCTION
        const bucket = a.storage_index & (game_state.collision_rule_hash.len - 1);
        var maybe_rule = game_state.collision_rule_hash[bucket];

        while (maybe_rule) |rule| : (maybe_rule = rule.next_in_hash) {
            if (rule.storage_index_a == a.storage_index and
                rule.storage_index_b == b.storage_index)
            {
                result = rule.should_collide;
                break;
            }
        }
    }

    return result;
}

fn handleCollision(
    entity: *Entity,
    hit: *Entity,
) bool {
    var stops_on_collision = false;

    if (entity.type == .sword) {
        stops_on_collision = false;
    } else {
        stops_on_collision = true;
    }

    var a = entity;
    var b = hit;

    if (@intFromEnum(a.type) > @intFromEnum(b.type)) {
        const temp = a;
        a = b;
        b = temp;
    }

    if (a.type == .monster and b.type == .sword) {
        if (a.hit_point_max > 0) {
            a.hit_point_max -%= 1;
        }
    }

    // TODO: stairs
    //high.abs_tile_z += @bitCast(hit_low.d_abs_tile_z);

    // TODO: Real "stops on collision"
    return stops_on_collision;
}

pub fn moveEntity(
    game_state: *GameState,
    region: *SimRegion,
    entity: *Entity,
    dt: f32,
    move_spec: *MoveSpec,
    dd_pos: Vec3,
) void {
    assert(!entity.flags.non_spatial);

    var acceleration = dd_pos;

    if (move_spec.unit_max_acc_vector) {
        const acc_length = Vec3.lengthSquared(&acceleration);

        if (acc_length > 1.0) {
            acceleration = Vec3.scale(&acceleration, 1.0 / @sqrt(acc_length));
        }
    }

    acceleration = Vec3.scale(&acceleration, move_spec.speed);

    // TODO: ODE here
    acceleration = Vec3.add(
        &acceleration,
        &Vec3.scale(&entity.d_pos, -move_spec.drag),
    );

    acceleration = Vec3.add(
        &acceleration,
        &Vec3.init(0, 0, -9.8),
    );

    var player_d = Vec3.add(
        &Vec3.scale(&acceleration, 0.5 * math.square(dt)),
        &Vec3.scale(&entity.d_pos, dt),
    );

    entity.d_pos = Vec3.add(
        &Vec3.scale(&acceleration, dt),
        &entity.d_pos,
    );

    var distance_remaining = entity.distance_limit;

    if (distance_remaining == 0) {
        // TODO: Do we want to formalize this number?
        distance_remaining = 10000;
    }

    for (0..4) |_| {
        var t_min: f32 = 1.0;
        const player_delta_length = Vec3.length(&player_d);

        // TODO: What do we want to do for epsilons here?
        // Think this through for the final collision code
        if (player_delta_length > 0) {
            if (player_delta_length > distance_remaining) {
                t_min = distance_remaining / player_delta_length;
            }

            var wall_normal = Vec3.splat(0);
            var hit_entity: ?*Entity = null;

            const desired_position = Vec3.add(&entity.pos, &player_d);

            // NOTE: This is just an optimization to avoid entering the
            // loop in the case where the test entity is non spatial
            if (!entity.flags.non_spatial) {
                // TODO: Spatial partition here!
                for (0..region.entity_count) |high_index| {
                    const test_entity = &region.entities[high_index];

                    if (shouldCollide(game_state, entity, test_entity)) {
                        // TODO: Entities have height?
                        const minkowski_diameter = Vec3.init(
                            test_entity.width + entity.width,
                            test_entity.height + entity.height,
                            game_state.world.?.tile_depth_in_meters,
                        );

                        const min_c = Vec3.scale(&minkowski_diameter, -0.5);
                        const max_c = Vec3.scale(&minkowski_diameter, 0.5);
                        const rel = Vec3.sub(&entity.pos, &test_entity.pos);

                        if (testWall(min_c.x(), rel.x(), rel.y(), player_d.x(), player_d.y(), &t_min, min_c.y(), max_c.y())) {
                            wall_normal = Vec3.init(-1, 0, 0);
                            hit_entity = test_entity;
                        }

                        if (testWall(max_c.x(), rel.x(), rel.y(), player_d.x(), player_d.y(), &t_min, min_c.y(), max_c.y())) {
                            wall_normal = Vec3.init(1, 0, 0);
                            hit_entity = test_entity;
                        }

                        if (testWall(min_c.y(), rel.y(), rel.x(), player_d.y(), player_d.x(), &t_min, min_c.x(), max_c.x())) {
                            wall_normal = Vec3.init(0, -1, 0);
                            hit_entity = test_entity;
                        }

                        if (testWall(max_c.y(), rel.y(), rel.x(), player_d.y(), player_d.x(), &t_min, min_c.x(), max_c.x())) {
                            wall_normal = Vec3.init(0, 1, 0);
                            hit_entity = test_entity;
                        }
                    }
                }
            }

            entity.pos = Vec3.add(
                &entity.pos,
                &Vec3.scale(&player_d, t_min),
            );

            distance_remaining -= t_min * player_delta_length;

            if (hit_entity) |hit| {
                player_d = Vec3.sub(&desired_position, &entity.pos);

                const stops_on_collision = handleCollision(entity, hit);

                if (stops_on_collision) {
                    // NOTE: Why were we still updating the player delta here?
                    //const p_product = 1 * math.inner(player_d, wall_normal);
                    //const p_magnitude = math.scale(wall_normal, p_product);
                    //player_d = math.sub(player_d, p_magnitude);

                    const d_product = 1 * Vec3.inner(&entity.d_pos, &wall_normal);
                    const d_magnitude = Vec3.scale(&wall_normal, d_product);
                    entity.d_pos = Vec3.sub(&entity.d_pos, &d_magnitude);
                } else {
                    game.addCollisionRule(
                        game_state,
                        entity.storage_index,
                        hit.storage_index,
                        false,
                    );
                }
            } else break;
        } else break;
    }

    // TODO: This has to become real height handling
    if (entity.pos.z() < 0) {
        entity.pos.v[2] = 0;
    }

    if (entity.distance_limit != 0) {
        entity.distance_limit = distance_remaining;
    }

    // TODO: Change to using the acceleration vector
    if (entity.d_pos.x() == 0.0 and entity.d_pos.y() == 0.0) {
        // Leave facing_direction alone
    } else if (@abs(entity.d_pos.x()) > @abs(entity.d_pos.y())) {
        if (entity.d_pos.x() > 0) {
            entity.facing_direction = 0;
        } else {
            entity.facing_direction = 2;
        }
    } else if (@abs(entity.d_pos.x()) < @abs(entity.d_pos.y())) {
        if (entity.d_pos.y() > 0) {
            entity.facing_direction = 1;
        } else {
            entity.facing_direction = 3;
        }
    }
}
