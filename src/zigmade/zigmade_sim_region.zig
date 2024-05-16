const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const world = @import("zigmade_world.zig");
const ety = @import("zigmade_entity.zig");

const Vec3 = math.Vec3;
const Rectangle3 = math.Rectangle3;
const MemoryArena = game.MemoryArena;
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
    stairwell,
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
    movable: bool = false,
    simming: bool = false,
    z_supported: bool = false,
    _padding: u27 = 0,
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
    dim: Vec3 = Vec3.splat(0),
    chunk_z: u32 = 0,
    facing_direction: u32 = 0,
    t_bob: f32 = 0,
    d_abs_tile_z: i32 = 0,
    // TODO: Should hit points themselves be entities?
    hit_point_max: u32 = 0,
    hit_points: [16]HitPoint = undefined,
    sword: EntityReference = .{ .index = 0 },
    // TODO: Only for stairwells
    walkable_height: f32 = 0,
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
    max_entity_radius: f32,
    max_entity_velocity: f32,
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
            game.invalidCodePath();
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

pub inline fn entityOverlaps(pos: Vec3, dim: Vec3, rect: Rectangle3) bool {
    const grown = Rectangle3.addRadius(rect, Vec3.scale(&dim, 0.5));
    const result = Rectangle3.isInRectangle(grown, pos);

    return result;
}

fn addEntity(
    game_state: *GameState,
    sim_region: *SimRegion,
    storage_index: u32,
    source: ?*LowEntity,
    maybe_sim_pos: ?*Vec3,
) ?*Entity {
    var maybe_dest = addEntityRaw(game_state, sim_region, storage_index, source);

    if (maybe_dest) |dest| {
        if (maybe_sim_pos) |sim_pos| {
            dest.pos = sim_pos.*;

            dest.updatable = entityOverlaps(
                dest.pos,
                dest.dim,
                sim_region.updatable_bounds,
            );
        } else {
            dest.pos = getSimSpaceP(sim_region, source);
        }

        maybe_dest = dest;
    }

    return maybe_dest;
}

pub fn beginSim(
    arena: *MemoryArena,
    game_state: *GameState,
    game_world: *World,
    origin: WorldPosition,
    bounds: Rectangle3,
    dt: f32,
) *SimRegion {
    // TODO: If entities are stored in the world, we wouldn't need game state here

    var sim_region = game.pushStruct(arena, SimRegion);
    game.zeroStruct(@TypeOf(sim_region.hash), &sim_region.hash);

    // TODO: Try to enforce these more rigorously
    // TODO: Perhaps try a dual system where we support
    // entities larger than the max entity radius by adding
    // them multiple times to the spatial partition?
    sim_region.max_entity_radius = 5;
    sim_region.max_entity_velocity = 30;

    const update_safety_margin: f32 = sim_region.max_entity_radius +
        dt * sim_region.max_entity_velocity;

    const update_safety_margin_z: f32 = 1;

    sim_region.world = game_world;
    sim_region.origin = origin;
    sim_region.updatable_bounds = Rectangle3.addRadius(
        bounds,
        Vec3.splat(sim_region.max_entity_radius),
    );

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

    var chunk_z = min_chunk_p.chunk_z;

    while (chunk_z <= max_chunk_p.chunk_z) : (chunk_z += 1) {
        var chunk_y = min_chunk_p.chunk_y;

        while (chunk_y <= max_chunk_p.chunk_y) : (chunk_y += 1) {
            var chunk_x = min_chunk_p.chunk_x;

            while (chunk_x <= max_chunk_p.chunk_x) : (chunk_x += 1) {
                const chunk = world.getWorldChunk(
                    game_world,
                    chunk_x,
                    chunk_y,
                    chunk_z,
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

                                if (entityOverlaps(
                                    sim_space_p,
                                    low.sim.dim,
                                    sim_region.bounds,
                                )) {
                                    _ = addEntity(game_state, sim_region, low_index, low, &sim_space_p);
                                }
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
    game_state: *GameState,
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

            var new_p = if (entity.flags.non_spatial)
                world.nullPosition()
            else
                world.mapIntoChunkSpace(game_world, region.origin, entity.pos);

            world.changeEntityLocation(
                &game_state.world_arena,
                game_world,
                entity.storage_index,
                &game_state.low_entities[entity.storage_index],
                &new_p,
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
                    const z_offset = new_camera_p.offset_.z();
                    new_camera_p = stored.pos;
                    new_camera_p.offset_.v[2] = z_offset;
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

fn canCollide(
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
                result = rule.can_collide;
                break;
            }
        }
    }

    return result;
}

fn handleCollision(
    game_state: *GameState,
    entity: *Entity,
    hit: *Entity,
) bool {
    var stops_on_collision = false;

    if (entity.type == .sword) {
        game.addCollisionRule(
            game_state,
            entity.storage_index,
            hit.storage_index,
            false,
        );

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

fn canOverlap(
    _: *GameState,
    mover: *Entity,
    region: *Entity,
) bool {
    var result = false;

    if (mover != region) {
        if (region.type == .stairwell) {
            result = true;
        }
    }

    return result;
}

inline fn getStairGround(entity: *Entity, at_ground_point: Vec3) f32 {
    assert(entity.type == .stairwell);

    const region_rect = Rectangle3.centerDim(entity.pos, entity.dim);

    const bary = Vec3.clamp01(&Rectangle3.getBarycentric(
        region_rect,
        at_ground_point,
    ));

    const result =
        region_rect.min.z() +
        bary.y() *
        entity.walkable_height;

    return result;
}

pub inline fn getEntityGroundPoint(entity: *Entity) Vec3 {
    const result = Vec3.add(
        &entity.pos,
        &Vec3.init(0, 0, -0.5 * entity.dim.z()),
    );

    return result;
}

fn handleOverlap(
    _: *GameState,
    mover: *Entity,
    region: *Entity,
    _: f32,
    ground: *f32,
) void {
    if (region.type == .stairwell) {
        ground.* = getStairGround(region, getEntityGroundPoint(mover));
    }
}

pub fn speculativeCollide(
    mover: *Entity,
    region: *Entity,
) bool {
    var result = true;

    if (region.type == .stairwell) {

        // TODO: Needs work
        const step_height = 0.1;

        if (false) {
            //const ground_diff = getEntityGroundPoint(mover).z() - ground;
            //result = (@abs(ground_diff) > step_height) or
            //    (bary.y() > 0.1 and bary.y() < 0.9);
        }

        const mover_ground_point = getEntityGroundPoint(mover);
        const ground = getStairGround(region, mover_ground_point);

        result = @abs(mover_ground_point.z() - ground) > step_height;
    }

    return result;
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

    if (!entity.flags.z_supported) {
        acceleration = Vec3.add(&acceleration, &Vec3.init(0, 0, -9.8));
    }

    var player_d = Vec3.add(
        &Vec3.scale(&acceleration, 0.5 * math.square(dt)),
        &Vec3.scale(&entity.d_pos, dt),
    );

    entity.d_pos = Vec3.add(
        &Vec3.scale(&acceleration, dt),
        &entity.d_pos,
    );

    // TODO: Upgrade physical motion routines to handle capping
    // max velocity?
    assert(Vec3.lengthSquared(&entity.d_pos) <=
        math.square(region.max_entity_velocity));

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

                    if (canCollide(game_state, entity, test_entity)) {
                        // TODO: Entities have height?
                        const minkowski_diameter = Vec3.init(
                            test_entity.dim.x() + entity.dim.x(),
                            test_entity.dim.y() + entity.dim.y(),
                            test_entity.dim.z() + entity.dim.z(),
                        );

                        const min_c = Vec3.scale(&minkowski_diameter, -0.5);
                        const max_c = Vec3.scale(&minkowski_diameter, 0.5);
                        const rel = Vec3.sub(&entity.pos, &test_entity.pos);

                        // TODO: Do we want an open inclusion at the max corner?
                        if (rel.z() >= min_c.z() and rel.z() < max_c.z()) {
                            var t_min_test = t_min;
                            var test_wall_normal = Vec3.splat(0);
                            var hit_this = false;

                            if (testWall(min_c.x(), rel.x(), rel.y(), player_d.x(), player_d.y(), &t_min_test, min_c.y(), max_c.y())) {
                                test_wall_normal = Vec3.init(-1, 0, 0);
                                hit_this = true;
                            }

                            if (testWall(max_c.x(), rel.x(), rel.y(), player_d.x(), player_d.y(), &t_min_test, min_c.y(), max_c.y())) {
                                test_wall_normal = Vec3.init(1, 0, 0);
                                hit_this = true;
                            }

                            if (testWall(min_c.y(), rel.y(), rel.x(), player_d.y(), player_d.x(), &t_min_test, min_c.x(), max_c.x())) {
                                test_wall_normal = Vec3.init(0, -1, 0);
                                hit_this = true;
                            }

                            if (testWall(max_c.y(), rel.y(), rel.x(), player_d.y(), player_d.x(), &t_min_test, min_c.x(), max_c.x())) {
                                test_wall_normal = Vec3.init(0, 1, 0);
                                hit_this = true;
                            }

                            // TODO: We need a concept of stepping onto vs stepping off
                            // of here so that we can prevent you from _leaving_
                            // stairs intead of just preventing you from getting onto them
                            if (hit_this) {
                                //const test_p = Vec3.add(&entity.pos, &Vec3.scale(&player_d, t_min_test),);

                                if (speculativeCollide(entity, test_entity)) {
                                    t_min = t_min_test;
                                    wall_normal = test_wall_normal;
                                    hit_entity = test_entity;
                                }
                            }
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

                const stops_on_collision = handleCollision(game_state, entity, hit);

                if (stops_on_collision) {
                    const p_product = 1 * Vec3.inner(&player_d, &wall_normal);
                    const p_magnitude = Vec3.scale(&wall_normal, p_product);
                    player_d = Vec3.sub(&player_d, &p_magnitude);

                    const d_product = 1 * Vec3.inner(&entity.d_pos, &wall_normal);
                    const d_magnitude = Vec3.scale(&wall_normal, d_product);
                    entity.d_pos = Vec3.sub(&entity.d_pos, &d_magnitude);
                }
            } else break;
        } else break;
    }

    var ground: f32 = 0;

    // NOTE: Handle events based on area overlapping
    // TODO: Handle overlapping precisely by moving it into the collision loop?
    {
        const entity_rect = Rectangle3.centerDim(entity.pos, entity.dim);

        // TODO: Spatial partition here!
        for (0..region.entity_count) |high_index| {
            const test_entity = &region.entities[high_index];

            if (canOverlap(game_state, entity, test_entity)) {
                const test_entity_rect = Rectangle3.centerDim(test_entity.pos, test_entity.dim);

                if (Rectangle3.rectanglesIntersect(entity_rect, test_entity_rect)) {
                    handleOverlap(game_state, entity, test_entity, dt, &ground);
                }
            }
        }
    }

    ground += entity.pos.z() - getEntityGroundPoint(entity).z();

    // TODO: This has to become real height handling
    if (entity.pos.z() <= ground or
        (entity.flags.z_supported and
        entity.d_pos.z() == 0))
    {
        entity.pos.v[2] = ground;
        entity.d_pos.v[2] = 0;
        entity.flags.z_supported = true;
    } else {
        entity.flags.z_supported = false;
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
