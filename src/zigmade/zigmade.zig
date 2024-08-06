//
// TODO:
//
// - Rendering
//   - Lighting
//   - Straighten out all coordinate systems
//     - Screen
//     - World
//     - Texture
//   - Optimiziation
// ARCHITECTURE EXPLORATION
//
// - Z
//   - Need to make a solid concept of ground levels so the camera can
//     be freely placed in Z and have multiple ground levels in one sim
//     region
//   - Concept of ground in the collision loop so it can handle collisions
//     coming onto and off of stairwells
//   - Make sure flying things can go over low walls
//   - Figure out how you go up and down and how is this rendered?
//     "Frinstances"
//     z_fudge
// - Collision detection?
//   - Fix sword collisions
//   - Clean up predicate proliferation. Can we make a nice clean
//     set of flags/rules so that it's easy to understand how things work
//     in terms of special handling. This may involve making the iteration
//     handle everything instead of handling overlap outside and so on.
//   - Transient collision rules! Clear based on flag.
//     - Allow non-transient rules to override transient ones
//     - Entry/exit?
//   - What's the plan for robustness/shape definition?
//   - Implement reprojection to handle interpenetration
//   - "Things pushing other things"
// - Implement multiple sim regions per frame
//   - Per-entity clocking
//   - Sim region merging? Multiple players?
//   - Simple zoomed-out view for testing?
//
// - Debug code
//   - Fonts
//   - Logging
//   - Diagramming
//   - (Just enough GUI) Switches/sliders/etc.
//   - Draw tile chunks so we can verify that things are aligned/in the
//     chunks we want them to be in/etc.
//
// - Asset streaming
//
// - Audio
//   - Sound effect triggers
//   - Ambience sounds
//   - Music
//
// - Metagame/save game?
//   - How do you enter "save slot"?
//   - Persistent unlocks/etc.
//   - Do we allow saved games? Probably, just only for "pausing"
//   * Continuous save for crash recovery?
// - Rudimentary world gen (no quality, just "what sorts of things" we do
//   - Placement of background objects
//   - Connectivity?
//   - Non-overlapping?
//   - Map display
//     - Magnets - how they work?
// - AI
//   - Rudimentary monster behavior example
//   * Pathfinding
//   - AI "storage"
//
// * Animation, probably should lead into rendering
//   - Skeletal animation
//   - Particle systems
//
// PRODUCTION
// - Game
//   - Entity system
//   - World generation
//

const std = @import("std");
const assert = std.debug.assert;
const rotl = std.math.rotl;
const platform = @import("zigmade_platform");
const world = @import("zigmade_world.zig");
const math = @import("zigmade_math.zig");
const sim = @import("zigmade_sim_region.zig");
const ety = @import("zigmade_entity.zig");
const intrinsics = @import("zigmade_intrinsics.zig");
const render = @import("zigmade_render.zig");
const random = @import("zigmade_random.zig");
const INTERNAL = @import("builtin").mode == std.builtin.Mode.Debug;

const lossyCast = std.math.lossyCast;

const Vec2 = math.Vec2;
const Vec3 = math.Vec3;
const Vec4 = math.Vec4;
const vec2 = math.vec2;
const vec3 = math.vec3;
const vec4 = math.vec4;
const Rectangle3 = math.Rectangle3;
const Entity = sim.Entity;
const SimRegion = sim.SimRegion;
const EntityType = sim.EntityType;
const MoveSpec = sim.MoveSpec;
const EntityCollisionVolume = sim.EntityCollisionVolume;
const EntityCollisionVolumeGroup = sim.EntityCollisionVolumeGroup;
const World = world.World;
const WorldPosition = world.WorldPosition;
const Bitmap = render.Bitmap;
const RenderGroup = render.RenderGroup;
const RenderBasis = render.RenderBasis;
const EnvironmentMap = render.EnvironmentMap;

const HeroBitmaps = struct {
    head: Bitmap,
    cape: Bitmap,
    torso: Bitmap,
};

pub const LowEntity = struct {
    // TODO: It's kind of busted that pos can be invalid here
    // and we can store whether they would be invalid in the flags
    // Can we do something better?
    sim: Entity = .{},
    p: WorldPosition = .{},
};

const AddLowEntityResult = struct {
    low_index: u32,
    low: *LowEntity,
};

const ControlledHero = struct {
    entity_index: u32 = 0,
    ddp: Vec2 = Vec2.splat(0),
    d_sword: Vec2 = Vec2.splat(0),
    dz: f32 = 0,
};

pub const PairwiseCollisionRule = struct {
    can_collide: bool,
    storage_index_a: u32,
    storage_index_b: u32,
    next_in_hash: ?*PairwiseCollisionRule,
};

pub const GroundBuffer = struct {
    // NOTE: An invalid p tells us this GroundBuffer has not been filled
    // NOTE: This is the center of the bitmap
    p: WorldPosition,
    bitmap: Bitmap,
};

pub const GameState = struct {
    world_arena: MemoryArena,
    world: ?*World = null,
    typical_floor_height: f32,
    // TODO: Should we allow split-screen?
    camera_entity_index: u32,
    camera_p: WorldPosition,
    controlled_heroes: [5]ControlledHero,
    low_entity_count: u32 = 0,
    // TODO: Change name to StoredEntity
    low_entities: [100000]LowEntity,
    grass: [2]Bitmap,
    stone: [4]Bitmap,
    tuft: [3]Bitmap,
    backdrop: Bitmap,
    shadow: Bitmap,
    hero_bitmaps: [4]HeroBitmaps,
    tree: Bitmap,
    sword: Bitmap,
    stairwell: Bitmap,
    // TODO: Must be power of two
    collision_rule_hash: [256]?*PairwiseCollisionRule,
    first_free_collision_rule: ?*PairwiseCollisionRule,
    null_collision: *EntityCollisionVolumeGroup,
    sword_collision: *EntityCollisionVolumeGroup,
    stair_collision: *EntityCollisionVolumeGroup,
    player_collision: *EntityCollisionVolumeGroup,
    familiar_collision: *EntityCollisionVolumeGroup,
    monster_collision: *EntityCollisionVolumeGroup,
    wall_collision: *EntityCollisionVolumeGroup,
    standard_room_collision: *EntityCollisionVolumeGroup,
    time: f32,
    test_diffuse: Bitmap,
    test_normal: Bitmap,
};

const TransientState = struct {
    is_initialized: bool = false,
    arena: MemoryArena,
    ground_buffer_count: u32,
    ground_buffers: [*]GroundBuffer,
    env_map_width: usize,
    env_map_height: usize,
    // NOTE: 0 is bottom, 1 is middle, 2 is top
    env_maps: [3]EnvironmentMap,
};

pub const TemporaryMemory = struct {
    used: usize,
    arena: *MemoryArena,
};

pub const MemoryArena = struct {
    size: usize,
    base: [*]u8,
    used: usize,
    temp_count: usize,
};

const BitmapHeader = packed struct {
    file_type: u16,
    file_size: u32,
    reserved_1: u16,
    reserved_2: u16,
    bitmap_offset: u32,
    size: u32,
    width: i32,
    height: i32,
    planes: u16,
    bits_per_pixel: u16,
    compression: u32,
    size_of_bitmap: u32,
    horz_resolution: i32,
    vert_resolution: i32,
    colors_used: u32,
    colors_important: u32,
    red_mask: u32,
    green_mask: u32,
    blue_mask: u32,
};

fn gameOutputSound(
    sound_buffer: *platform.GameSoundBuffer,
    game_state: *GameState,
) !void {
    _ = game_state;
    const tone_volume: i16 = 3_000;
    _ = tone_volume;
    const wave_period = @divTrunc(
        @as(i32, @intCast(sound_buffer.samples_per_second)),
        400,
    );
    _ = wave_period;
    var sample_out = sound_buffer.samples;

    for (0..@intCast(sound_buffer.sample_count)) |i| {
        //var sine_value = @sin(game_state.t_sine);
        //var sample_value = @as(i16, @intFromFloat(
        //    sine_value * @as(
        //        f16,
        //        @floatFromInt(tone_volume),
        //    ),
        //));

        const sample_value: i16 = 0;
        sample_out[2 * i] = sample_value;
        sample_out[2 * i + 1] = sample_value;

        //game_state.t_sine +=
        //    2.0 *
        //    std.math.pi *
        //    1.0 /
        //    @as(f32, @floatFromInt(wave_period));

        //if (game_state.t_sine > 2.0 * std.math.pi) {
        //    game_state.t_sine -= 2.0 * std.math.pi;
        //}
    }
}

fn debugLoadBMPDefault(
    thread: *platform.ThreadContext,
    readEntireFile: platform.debugPlatformReadEntireFile,
    file_name: [*:0]const u8,
) Bitmap {
    var result = debugLoadBMP(thread, readEntireFile, file_name, 0, 0);
    result.align_percentage = Vec2.splat(0.5);
    return result;
}

fn debugLoadBMP(
    thread: *platform.ThreadContext,
    readEntireFile: platform.debugPlatformReadEntireFile,
    file_name: [*:0]const u8,
    align_x: i32,
    top_down_align_y: i32,
) Bitmap {
    var result: Bitmap = undefined;

    const read_result = readEntireFile(thread, file_name);

    if (read_result.size != 0) {
        const header: *BitmapHeader = @alignCast(@ptrCast(read_result.contents));
        const pixels = @as([*]void, @ptrCast(read_result.contents)) + header.bitmap_offset;
        result.memory = pixels;
        result.width = @intCast(header.width);
        result.height = @intCast(header.height);
        result.align_percentage = topDownAlign(&result, Vec2.fromInt(align_x, top_down_align_y));
        result.width_over_height = math.safeRatio0(@floatFromInt(result.width), @floatFromInt(result.height));

        assert(result.height >= 0);
        assert(header.compression == 3);

        // NOTE: If using this generically, remember that BMP
        // files can go in either direction and height will be
        // negative for top-down
        // Also, there can be compression, etc. this is not complete
        // BMP loading code
        //
        // NOTE: Byte order memory is determined by the header itself,
        // so we have to read out the masks and convert pixels ourselves

        var source_dest: [*]align(@alignOf(u8)) u32 = @ptrCast(pixels);

        const red_mask = header.red_mask;
        const green_mask = header.green_mask;
        const blue_mask = header.blue_mask;
        const alpha_mask = ~(red_mask | green_mask | blue_mask);

        const red_scan = intrinsics.findLeastSigSetBit(red_mask);
        const green_scan = intrinsics.findLeastSigSetBit(green_mask);
        const blue_scan = intrinsics.findLeastSigSetBit(blue_mask);
        const alpha_scan = intrinsics.findLeastSigSetBit(alpha_mask);

        const red_shift_down = @as(u5, @intCast(red_scan.index));
        const green_shift_down = @as(u5, @intCast(green_scan.index));
        const blue_shift_down = @as(u5, @intCast(blue_scan.index));
        const alpha_shift_down = @as(u5, @intCast(alpha_scan.index));

        assert(red_scan.found);
        assert(green_scan.found);
        assert(blue_scan.found);
        assert(alpha_scan.found);

        for (0..@intCast(header.height)) |_| {
            for (0..@intCast(header.width)) |_| {
                const c = source_dest[0];

                var texel = vec4(
                    @floatFromInt((c & red_mask) >> red_shift_down),
                    @floatFromInt((c & green_mask) >> green_shift_down),
                    @floatFromInt((c & blue_mask) >> blue_shift_down),
                    @floatFromInt((c & alpha_mask) >> alpha_shift_down),
                );

                texel = render.SRGB255ToLinear1(texel);

                if (true) {
                    texel = texel.premultipliedAlpha(texel.a());
                }

                texel = render.linear1ToSRGB255(texel);

                source_dest[0] = (lossyCast(u32, texel.a() + 0.5) << 24) |
                    (lossyCast(u32, texel.r() + 0.5) << 16) |
                    (lossyCast(u32, texel.g() + 0.5) << 8) |
                    (lossyCast(u32, texel.b() + 0.5) << 0);

                source_dest += 1;
            }
        }
    }

    result.pitch = result.width * platform.BITMAP_BYTES_PER_PIXEL;

    if (false) {
        result.memory = @ptrCast(
            @as([*]u8, @ptrCast(result.memory)) +
                @as(usize, @intCast(result.pitch * (result.height - 1))),
        );
        result.pitch = -result.width * platform.BITMAP_BYTES_PER_PIXEL;
    }

    return result;
}

inline fn initializeArena(
    arena: *MemoryArena,
    size: u32,
    base: [*]void,
) void {
    arena.size = size;
    arena.base = @as([*]u8, @ptrCast(base));
    arena.used = 0;
    arena.temp_count = 0;
}

pub inline fn pushSize(
    arena: *MemoryArena,
    alignment: comptime_int,
    size: usize,
) [*]align(alignment) u8 {
    const address = arena.base + arena.used;
    const masked = @as(usize, @intFromPtr(address)) & alignment - 1;
    const offset = if (masked != 0) alignment - masked else 0;
    const alignment_correct_size = size + offset;

    assert((arena.used + alignment_correct_size) <= arena.size);

    const result: [*]align(alignment) u8 =
        @alignCast(@ptrCast(arena.base + arena.used + offset));

    arena.used += alignment_correct_size;

    return result;
}

pub inline fn pushStruct(
    arena: *MemoryArena,
    comptime T: type,
) *T {
    const result = pushSize(arena, @alignOf(T), @sizeOf(T));
    return @as(*T, @alignCast(@ptrCast(result)));
}

pub inline fn pushArray(
    arena: *MemoryArena,
    count: u32,
    comptime T: type,
) [*]T {
    const result = pushSize(arena, @alignOf(T), count * @sizeOf(T));
    return @as([*]T, @alignCast(@ptrCast(result)));
}

inline fn beginTemporaryMemory(arena: *MemoryArena) TemporaryMemory {
    var result: TemporaryMemory = undefined;

    result.arena = arena;
    result.used = arena.used;

    arena.temp_count += 1;

    return result;
}

inline fn endTemporaryMemory(memory: TemporaryMemory) void {
    var arena = memory.arena;

    assert(arena.used >= memory.used);

    arena.used = memory.used;

    assert(arena.temp_count > 0);

    arena.temp_count -= 1;
}

inline fn checkArena(arena: *MemoryArena) void {
    assert(arena.temp_count == 0);
}

inline fn zeroSize(size: u32, ptr: [*]void) void {
    // TODO: Check this for performance

    var byte: [*]u8 = @ptrCast(ptr);

    var index: usize = size;
    while (index > 0) : (index -= 1) {
        byte[0] = 0;
        byte += 1;
    }
}

pub inline fn zeroStruct(
    comptime T: type,
    ptr: *T,
) void {
    zeroSize(@sizeOf(T), @ptrCast(ptr));
}

pub inline fn getLowEntity(
    game_state: *GameState,
    index: u32,
) ?*LowEntity {
    var result: ?*LowEntity = null;

    if (index > 0 and index < game_state.low_entities.len) {
        result = &game_state.low_entities[index];
    }

    return result;
}

fn addLowEntity(
    game_state: *GameState,
    t: EntityType,
    p: *const WorldPosition,
) AddLowEntityResult {
    assert(game_state.low_entity_count < game_state.low_entities.len);

    const entity_index = game_state.low_entity_count;
    game_state.low_entity_count += 1;

    const low = &game_state.low_entities[entity_index];
    low.* = .{};
    low.sim.type = t;
    low.sim.collision = game_state.null_collision;
    low.p = world.nullPosition();

    world.changeEntityLocation(
        &game_state.world_arena,
        game_state.world,
        entity_index,
        low,
        p,
    );

    const result = AddLowEntityResult{
        .low = low,
        .low_index = entity_index,
    };

    // TODO: Do we need a begin/end paradigm for adding
    // entities so that they can be brought into the high set
    // when they are added and are in the camera region?

    return result;
}

fn addGroundedEntity(
    game_state: *GameState,
    t: EntityType,
    p: *WorldPosition,
    collision: *EntityCollisionVolumeGroup,
) AddLowEntityResult {
    var result: AddLowEntityResult = undefined;

    var entity = addLowEntity(game_state, t, p);
    entity.low.sim.collision = collision;
    result = entity;

    return result;
}

pub fn chunkPosFromTilePos(
    game_world: *World,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
    additional_offset: Vec3,
) WorldPosition {
    const base_pos: WorldPosition = .{};
    const tile_side_in_meters = 1.4;
    const tile_depth_in_meters = 3.0;

    const tile_dim = vec3(
        tile_side_in_meters,
        tile_side_in_meters,
        tile_depth_in_meters,
    );

    const offset = Vec3.hadamard(
        &tile_dim,
        &Vec3.fromInt(abs_tile_x, abs_tile_y, abs_tile_z),
    );

    const result = world.mapIntoChunkSpace(
        game_world,
        base_pos,
        Vec3.add(&additional_offset, &offset),
    );

    assert(world.vecIsCanonical(game_world, result.offset_));

    return result;
}

fn addStandardRoom(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) AddLowEntityResult {
    var pos = chunkPosFromTilePos(
        game_state.world.?,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
        Vec3.splat(0),
    );

    var entity = addGroundedEntity(
        game_state,
        .space,
        &pos,
        game_state.standard_room_collision,
    );

    entity.low.sim.flags.traversable = true;

    return entity;
}

fn addWall(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) AddLowEntityResult {
    var pos = chunkPosFromTilePos(
        game_state.world.?,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
        Vec3.splat(0),
    );

    var entity = addGroundedEntity(
        game_state,
        .wall,
        &pos,
        game_state.wall_collision,
    );

    entity.low.sim.flags.collides = true;

    return entity;
}

fn addStair(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) AddLowEntityResult {
    const game_world = game_state.world.?;

    var pos = chunkPosFromTilePos(
        game_world,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
        Vec3.splat(0),
    );

    const entity = addGroundedEntity(
        game_state,
        .stairwell,
        &pos,
        game_state.stair_collision,
    );

    entity.low.sim.flags.collides = true;
    entity.low.sim.walkable_dim = entity.low.sim.collision.total_volume.dim.xy();
    entity.low.sim.walkable_height = game_state.typical_floor_height;

    return entity;
}

fn initHitPoints(low: *LowEntity, count: u32) void {
    assert(count < low.sim.hit_points.len);
    low.sim.hit_point_max = count;

    for (0..low.sim.hit_point_max) |index| {
        var hit_point = &low.sim.hit_points[index];
        hit_point.flags = 0;
        hit_point.filled_amount = sim.HIT_POINT_SUB_COUNT;
    }
}

fn addPlayer(game_state: *GameState) AddLowEntityResult {
    var p = game_state.camera_p;
    const entity = addGroundedEntity(
        game_state,
        .hero,
        &p,
        game_state.player_collision,
    );
    entity.low.sim.flags.collides = true;
    entity.low.sim.flags.movable = true;

    initHitPoints(entity.low, 3);

    const sword = addSword(game_state);
    entity.low.sim.sword.index = sword.low_index;

    if (game_state.camera_entity_index == 0) {
        game_state.camera_entity_index = entity.low_index;
    }

    return entity;
}

fn addMonster(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) AddLowEntityResult {
    const game_world = game_state.world.?;

    var pos = chunkPosFromTilePos(
        game_world,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
        Vec3.splat(0),
    );

    const entity = addGroundedEntity(
        game_state,
        .monster,
        &pos,
        game_state.monster_collision,
    );
    entity.low.sim.flags.collides = true;
    entity.low.sim.flags.movable = true;

    initHitPoints(entity.low, 3);

    return entity;
}

fn addSword(game_state: *GameState) AddLowEntityResult {
    const entity = addLowEntity(game_state, .sword, &world.nullPosition());

    entity.low.sim.collision = game_state.sword_collision;
    entity.low.sim.flags.movable = true;

    return entity;
}

fn addFamiliar(
    game_state: *GameState,
    abs_tile_x: i32,
    abs_tile_y: i32,
    abs_tile_z: i32,
) AddLowEntityResult {
    const game_world = game_state.world.?;

    var pos = chunkPosFromTilePos(
        game_world,
        abs_tile_x,
        abs_tile_y,
        abs_tile_z,
        Vec3.splat(0),
    );

    var entity = addGroundedEntity(
        game_state,
        .familiar,
        &pos,
        game_state.familiar_collision,
    );
    entity.low.sim.flags.collides = true;
    entity.low.sim.flags.movable = true;

    return entity;
}

fn drawHitPoints(
    entity: *Entity,
    piece_group: *RenderGroup,
) void {
    if (entity.hit_point_max >= 1) {
        const health_dim = Vec2.splat(0.2);
        const spacing_x = 1.5 * health_dim.x();
        var hit_p = vec2(
            -0.5 * @as(f32, @floatFromInt(entity.hit_point_max - 1)) * spacing_x,
            -0.25,
        );

        const d_hit_p = vec2(spacing_x, 0);

        for (0..entity.hit_point_max) |index| {
            const hit_point = entity.hit_points[index];
            var color = vec4(1, 0, 0, 1);

            if (hit_point.filled_amount == 0) {
                color.v[0] = 0.2;
                color.v[1] = 0.2;
                color.v[2] = 0.2;
            }

            render.pushRect(piece_group, vec3(hit_p.x(), hit_p.y(), 0), health_dim, color);
            hit_p = Vec2.add(&hit_p, &d_hit_p);
        }
    }
}

fn clearCollisionRulesFor(game_state: *GameState, storage_index: u32) void {
    // TODO: Need to make a better data structure that allows removal
    // of collision rules without searching the entire table
    //
    // NOTE: One way to make removal easy would be to always
    // add both orders of pairs of sotrage indices to the hash
    // so no matter which position the entity is in it can be found
    // When doing first removal pass, remember the top of the free
    // list and when finished, do a pass through on all new
    // entries to the free list to remove the reverse of those
    // pairs
    //
    for (0..game_state.collision_rule_hash.len) |bucket| {
        var maybe_rule = &game_state.collision_rule_hash[bucket];
        while (maybe_rule.*) |rule| {
            if (rule.storage_index_a == storage_index or
                rule.storage_index_b == storage_index)
            {
                const removed = rule;
                maybe_rule.* = rule.next_in_hash;
                removed.next_in_hash = game_state.first_free_collision_rule;
                game_state.first_free_collision_rule = removed;
            } else {
                maybe_rule = &rule.next_in_hash;
            }
        }
    }
}

pub fn addCollisionRule(
    game_state: *GameState,
    storage_index_a: u32,
    storage_index_b: u32,
    can_collide: bool,
) void {
    var a = storage_index_a;
    var b = storage_index_b;

    // TODO: Collapse this with canCollide
    if (a > b) {
        const temp = a;
        a = b;
        b = temp;
    }

    // TODO: BETTER HASH FUNCTION
    var maybe_found: ?*PairwiseCollisionRule = null;
    const bucket = a & (game_state.collision_rule_hash.len - 1);

    var maybe_rule = game_state.collision_rule_hash[bucket];
    while (maybe_rule) |rule| : (maybe_rule = rule.next_in_hash) {
        if (rule.storage_index_a == a and
            rule.storage_index_b == b)
        {
            maybe_found = rule;
            break;
        }
    }

    if (maybe_found == null) {
        maybe_found = game_state.first_free_collision_rule;

        if (maybe_found) |found| {
            game_state.first_free_collision_rule = found.next_in_hash;
        } else {
            maybe_found = pushStruct(&game_state.world_arena, PairwiseCollisionRule);
        }

        if (maybe_found) |found| {
            found.next_in_hash = game_state.collision_rule_hash[bucket];
            maybe_found = found;
            game_state.collision_rule_hash[bucket] = found;
        }
    }

    if (maybe_found) |found| {
        found.storage_index_a = a;
        found.storage_index_b = b;
        found.can_collide = can_collide;
    }
}

fn makeSimpleGroundedCollision(
    game_state: *GameState,
    dim_x: f32,
    dim_y: f32,
    dim_z: f32,
) *EntityCollisionVolumeGroup {
    // TODO: Do not use world_arena, change to using fundamental_types arena, etc.
    var group = pushStruct(&game_state.world_arena, EntityCollisionVolumeGroup);

    group.volume_count = 1;

    group.volumes = pushArray(
        &game_state.world_arena,
        group.volume_count,
        EntityCollisionVolume,
    );

    group.total_volume.offset_p = vec3(0, 0, 0.5 * dim_z);
    group.total_volume.dim = vec3(dim_x, dim_y, dim_z);
    group.volumes.?[0] = group.total_volume;

    return group;
}

fn makeNullCollision(game_state: *GameState) *EntityCollisionVolumeGroup {
    // TODO: Do not use world_arena, change to using fundamental_types arena, etc.
    var group = pushStruct(&game_state.world_arena, EntityCollisionVolumeGroup);

    group.volume_count = 0;
    group.volumes = null;
    group.total_volume.offset_p = Vec3.splat(0);
    // TODO: Should this be negative?
    group.total_volume.dim = Vec3.splat(0);

    return group;
}

fn fillGroundChunk(
    transient_state: *TransientState,
    game_state: *GameState,
    ground_buffer: *GroundBuffer,
    chunk_p: *const WorldPosition,
) void {
    // TODO: Decide what our pushbuffer size is
    const ground_memory = beginTemporaryMemory(&transient_state.arena);

    // TODO: Need to be able to set an orthographic display mode here
    var buffer = &ground_buffer.bitmap;
    buffer.align_percentage = Vec2.splat(0.5);
    buffer.width_over_height = 1.0;
    const render_group = render.allocateRenderGroup(&transient_state.arena, platform.Megabytes(4), @intCast(buffer.width), @intCast(buffer.height));

    render.clear(render_group, vec4(1, 1, 0, 1));

    ground_buffer.p = chunk_p.*;

    const game_world = game_state.world.?;
    const width: f32 = game_world.chunk_dim_in_meters.x();
    const height: f32 = game_world.chunk_dim_in_meters.y();
    var half_dim = Vec2.scale(&vec2(width, height), 0.5);

    // TODO: Once we switch to orthographic STOP MULTIPLYING THIS
    half_dim = Vec2.scale(&half_dim, 2.0);

    var chunk_offset_y: i32 = -1;
    while (chunk_offset_y <= 1) : (chunk_offset_y += 1) {
        var chunk_offset_x: i32 = -1;
        while (chunk_offset_x <= 1) : (chunk_offset_x += 1) {
            const chunk_x = chunk_p.chunk_x + chunk_offset_x;
            const chunk_y = chunk_p.chunk_y + chunk_offset_y;
            const chunk_z = chunk_p.chunk_z;

            // TODO: Maybe make random number generation more systemic
            // TODO: Look into wang hashing or some other spatial seed
            // generation thing
            var series = random.seed(
                @abs(139 * chunk_x +
                    593 * chunk_y +
                    329 * chunk_z),
            );

            const center = vec2(@as(f32, @floatFromInt(chunk_offset_x)) * width, @as(f32, @floatFromInt(chunk_offset_y)) * height);

            for (0..100) |_| {
                var stamp: *Bitmap = undefined;

                if (random.choice(&series, 2) > 0) {
                    stamp = &game_state.grass[random.choice(&series, game_state.grass.len)];
                } else {
                    stamp = &game_state.stone[random.choice(&series, game_state.stone.len)];
                }

                const offset = Vec2.hadamard(&half_dim, &vec2(random.bilateral(&series), random.bilateral(&series)));
                const p = Vec2.add(&center, &offset);

                render.pushBitmap(render_group, stamp, 4.0, vec3(p.x(), p.y(), 0), Vec4.splat(1));
            }
        }
    }

    chunk_offset_y = -1;

    while (chunk_offset_y <= 1) : (chunk_offset_y += 1) {
        var chunk_offset_x: i32 = -1;
        while (chunk_offset_x <= 1) : (chunk_offset_x += 1) {
            const chunk_x = chunk_p.chunk_x + chunk_offset_x;
            const chunk_y = chunk_p.chunk_y + chunk_offset_y;
            const chunk_z = chunk_p.chunk_z;

            // TODO: Maybe make random number generation more systemic
            // TODO: Look into wang hashing or some other spatial seed
            // generation thing
            var series = random.seed(
                @abs(139 * chunk_x +
                    593 * chunk_y +
                    329 * chunk_z),
            );

            const center = vec2(
                @as(f32, @floatFromInt(chunk_offset_x)) * width,
                @as(f32, @floatFromInt(chunk_offset_y)) * height,
            );

            for (0..50) |_| {
                const stamp = &game_state.tuft[random.choice(&series, game_state.tuft.len)];

                const offset = Vec2.hadamard(&half_dim, &vec2(random.bilateral(&series), random.bilateral(&series)));
                const p = Vec2.add(&center, &offset);

                render.pushBitmap(render_group, stamp, 0.4, vec3(p.x(), p.y(), 0), Vec4.splat(1));
            }
        }
    }

    render.renderGroupToOutput(render_group, buffer);
    endTemporaryMemory(ground_memory);
}

fn clearBitmap(bitmap: *Bitmap) void {
    if (bitmap.memory) |memory| {
        const total_bitmap_size: u32 =
            @intCast(bitmap.width * bitmap.height * platform.BITMAP_BYTES_PER_PIXEL);

        zeroSize(total_bitmap_size, memory);
    }
}

fn makeEmptyBitmap(arena: *MemoryArena, width: i32, height: i32, clear_to_zero: bool) Bitmap {
    var result: Bitmap = undefined;

    result.width = width;
    result.height = height;
    result.pitch = result.width * platform.BITMAP_BYTES_PER_PIXEL;

    const total_bitmap_size: u32 =
        @intCast(width * height * platform.BITMAP_BYTES_PER_PIXEL);

    result.memory = @ptrCast(pushSize(
        arena,
        @alignOf(@TypeOf(total_bitmap_size)),
        total_bitmap_size,
    ));

    if (clear_to_zero) {
        clearBitmap(&result);
    }

    return result;
}

fn makeSphereNormalMap(
    bitmap: *Bitmap,
    roughness: f32,
    cx: f32,
    cy: f32,
) void {
    const inv_width = 1.0 / @as(f32, @floatFromInt(bitmap.width - 1));
    const inv_height = 1.0 / @as(f32, @floatFromInt(bitmap.height - 1));

    var row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(bitmap.memory)));

    for (0..@intCast(bitmap.height)) |y| {
        var pixel: [*]u32 = @alignCast(@ptrCast(row));

        for (0..@intCast(bitmap.width)) |x| {
            const bitmap_uv = vec2(
                inv_width * @as(f32, @floatFromInt(x)),
                inv_height * @as(f32, @floatFromInt(y)),
            );

            const nx = cx * (2.0 * bitmap_uv.x() - 1.0);
            const ny = cy * (2.0 * bitmap_uv.y() - 1.0);
            const root_term = 1.0 - nx * nx - ny * ny;
            var normal = vec3(0, 0.7071, 0.7071);

            if (root_term >= 0) {
                const nz = @sqrt(root_term);
                normal = vec3(nx, ny, nz);
            }

            const color = vec4(
                255.0 * (0.5 * (normal.x() + 1.0)),
                255.0 * (0.5 * (normal.y() + 1.0)),
                255.0 * (0.5 * (normal.z() + 1.0)),
                255.0 * roughness,
            );

            pixel[0] = (lossyCast(u32, color.a() + 0.5) << 24) |
                (lossyCast(u32, color.r() + 0.5) << 16) |
                (lossyCast(u32, color.g() + 0.5) << 8) |
                (lossyCast(u32, color.b() + 0.5) << 0);

            pixel += 1;
        }

        row += @as(u32, @intCast(bitmap.pitch));
    }
}

fn makeSphereDiffuseMap(
    bitmap: *Bitmap,
    cx: f32,
    cy: f32,
) void {
    const inv_width = 1.0 / @as(f32, @floatFromInt(bitmap.width - 1));
    const inv_height = 1.0 / @as(f32, @floatFromInt(bitmap.height - 1));

    var row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(bitmap.memory)));

    for (0..@intCast(bitmap.height)) |y| {
        var pixel: [*]u32 = @alignCast(@ptrCast(row));

        for (0..@intCast(bitmap.width)) |x| {
            const bitmap_uv = vec2(
                inv_width * @as(f32, @floatFromInt(x)),
                inv_height * @as(f32, @floatFromInt(y)),
            );

            const nx = cx * (2.0 * bitmap_uv.x() - 1.0);
            const ny = cy * (2.0 * bitmap_uv.y() - 1.0);
            const root_term = 1.0 - nx * nx - ny * ny;
            var alpha: f32 = 0;

            if (root_term >= 0) {
                alpha = 1;
            }

            const base_color = Vec3.splat(0);
            alpha *= 255;
            const color = vec4(
                alpha * base_color.x(),
                alpha * base_color.y(),
                alpha * base_color.z(),
                alpha,
            );

            pixel[0] = (lossyCast(u32, color.a() + 0.5) << 24) |
                (lossyCast(u32, color.r() + 0.5) << 16) |
                (lossyCast(u32, color.g() + 0.5) << 8) |
                (lossyCast(u32, color.b() + 0.5) << 0);

            pixel += 1;
        }

        row += @as(u32, @intCast(bitmap.pitch));
    }
}

fn makePyramidNormalMap(
    bitmap: *Bitmap,
    roughness: f32,
) void {
    var row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(bitmap.memory)));

    for (0..@intCast(bitmap.height)) |y| {
        var pixel: [*]u32 = @alignCast(@ptrCast(row));

        for (0..@intCast(bitmap.width)) |x| {
            const inv_x = (@as(usize, @intCast(bitmap.width)) - 1) - x;
            const seven = 0.7071;
            var normal = vec3(0, 0, seven);

            if (x < y) {
                if (inv_x < y) {
                    normal.v[0] = -seven;
                } else {
                    normal.v[1] = seven;
                }
            } else {
                if (inv_x < y) {
                    normal.v[1] = -seven;
                } else {
                    normal.v[0] = seven;
                }
            }

            const color = vec4(
                255.0 * (0.5 * (normal.x() + 1.0)),
                255.0 * (0.5 * (normal.y() + 1.0)),
                255.0 * (0.5 * (normal.z() + 1.0)),
                255.0 * roughness,
            );

            pixel[0] = (lossyCast(u32, color.a() + 0.5) << 24) |
                (lossyCast(u32, color.r() + 0.5) << 16) |
                (lossyCast(u32, color.g() + 0.5) << 8) |
                (lossyCast(u32, color.b() + 0.5) << 0);

            pixel += 1;
        }

        row += @as(u32, @intCast(bitmap.pitch));
    }
}

fn topDownAlign(bitmap: *Bitmap, alignment: Vec2) Vec2 {
    var new_align = alignment;

    new_align.v[1] = @as(f32, @floatFromInt(bitmap.height - 1)) - alignment.y();

    new_align.v[0] = math.safeRatio0(new_align.x(), @floatFromInt(bitmap.width));
    new_align.v[1] = math.safeRatio0(new_align.y(), @floatFromInt(bitmap.height));

    return new_align;
}

fn setTopDownAlign(bitmap: *HeroBitmaps, alignment: Vec2) void {
    const new_alignment = topDownAlign(&bitmap.head, alignment);

    bitmap.head.align_percentage = new_alignment;
    bitmap.cape.align_percentage = new_alignment;
    bitmap.torso.align_percentage = new_alignment;
}

// GAME NEEDS FOUR THINGS
// - timing
// - controller/keyboard input
// - bitmap buffer
// - sound buffer
pub export fn updateAndRender(
    thread: *platform.ThreadContext,
    memory: *platform.GameMemory,
    input: *platform.GameInput,
    buffer: *platform.GameOffscreenBuffer,
) void {
    assert(@sizeOf(@TypeOf(input.controllers[0].buttons.map)) ==
        @sizeOf(platform.GameButtonState) * input.controllers[0].buttons.array.len);

    const ground_buffer_width = 256;
    const ground_buffer_height = 256;

    // TODO: Remove this
    const pixels_to_meters = 1.0 / 42.0;

    assert(@sizeOf(GameState) <= memory.permanent_storage_size);
    var game_state: *GameState = @alignCast(@ptrCast(memory.permanent_storage));

    if (!memory.is_initialized) {
        const tiles_per_width = 17;
        const tiles_per_height = 9;

        game_state.typical_floor_height = 3.0;

        const world_chunk_dim_in_meters = vec3(
            pixels_to_meters * @as(f32, @floatFromInt(ground_buffer_width)),
            pixels_to_meters * @as(f32, @floatFromInt(ground_buffer_height)),
            game_state.typical_floor_height,
        );

        // TODO: Can we just use a Zig allocator?
        // TODO: Let's start partitioning our memory space
        initializeArena(&game_state.world_arena, memory.permanent_storage_size - @sizeOf(GameState), memory.permanent_storage + @sizeOf(GameState));
        defer checkArena(&game_state.world_arena);

        // NOTE: Reserve entity slot 0 as the null entity
        _ = addLowEntity(game_state, .none, &world.nullPosition());

        game_state.world = pushStruct(&game_state.world_arena, World);
        const game_world = game_state.world.?;
        world.initializeWorld(game_world, world_chunk_dim_in_meters);

        const tile_side_in_meters = 1.4;
        const tile_depth_in_meters = game_state.typical_floor_height;

        game_state.null_collision = makeNullCollision(game_state);
        game_state.sword_collision = makeSimpleGroundedCollision(game_state, 1, 0.5, 0.1);

        game_state.stair_collision = makeSimpleGroundedCollision(
            game_state,
            tile_side_in_meters,
            2 * tile_side_in_meters,
            1.1 * tile_depth_in_meters,
        );

        game_state.player_collision = makeSimpleGroundedCollision(game_state, 1, 0.5, 1.2);
        game_state.monster_collision = makeSimpleGroundedCollision(game_state, 1, 0.5, 0.5);
        game_state.familiar_collision = makeSimpleGroundedCollision(game_state, 1, 0.5, 0.5);
        game_state.wall_collision = makeSimpleGroundedCollision(game_state, tile_side_in_meters, tile_side_in_meters, tile_depth_in_meters);

        game_state.standard_room_collision = makeSimpleGroundedCollision(
            game_state,
            tiles_per_width * tile_side_in_meters,
            tiles_per_height * tile_side_in_meters,
            0.9 * tile_depth_in_meters,
        );

        game_state.grass[0] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/grass00.bmp");
        game_state.grass[1] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/grass01.bmp");

        game_state.stone[0] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/ground00.bmp");
        game_state.stone[1] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/ground01.bmp");
        game_state.stone[2] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/ground02.bmp");
        game_state.stone[3] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/ground03.bmp");

        game_state.tuft[0] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/tuft00.bmp");
        game_state.tuft[1] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/tuft01.bmp");
        game_state.tuft[2] = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/tuft02.bmp");

        game_state.backdrop = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_background.bmp");
        game_state.shadow = debugLoadBMP(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_shadow.bmp", 72, 182);
        game_state.tree = debugLoadBMP(thread, memory.debugPlatformReadEntireFile, "data/test2/tree00.bmp", 40, 80);
        game_state.stairwell = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test2/rock02.bmp");
        game_state.sword = debugLoadBMP(thread, memory.debugPlatformReadEntireFile, "data/test2/rock03.bmp", 29, 10);

        var bitmaps = &game_state.hero_bitmaps;
        bitmaps[0].head = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_right_head.bmp");
        bitmaps[0].cape = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_right_cape.bmp");
        bitmaps[0].torso = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_right_torso.bmp");
        setTopDownAlign(&bitmaps[0], vec2(72, 182));

        bitmaps[1].head = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_back_head.bmp");
        bitmaps[1].cape = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_back_cape.bmp");
        bitmaps[1].torso = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_back_torso.bmp");
        setTopDownAlign(&bitmaps[1], vec2(72, 182));

        bitmaps[2].head = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_left_head.bmp");
        bitmaps[2].cape = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_left_cape.bmp");
        bitmaps[2].torso = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_left_torso.bmp");
        setTopDownAlign(&bitmaps[2], vec2(72, 182));

        bitmaps[3].head = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_front_head.bmp");
        bitmaps[3].cape = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_front_cape.bmp");
        bitmaps[3].torso = debugLoadBMPDefault(thread, memory.debugPlatformReadEntireFile, "data/test/test_hero_front_torso.bmp");
        setTopDownAlign(&bitmaps[3], vec2(72, 182));

        var series = random.seed(0);

        const screen_base_x: i32 = 0;
        const screen_base_y: i32 = 0;
        const screen_base_z: i32 = 0;
        var screen_x: i32 = screen_base_x;
        var screen_y: i32 = screen_base_y;
        var abs_tile_z: i32 = screen_base_z;

        // TODO: Replace with real world generation
        var door_left = false;
        var door_right = false;
        var door_top = false;
        var door_bottom = false;
        var door_up = false;
        var door_down = false;

        for (0..2000) |_| {
            var door_direction: usize = undefined;

            door_direction = random.choice(
                &series,
                if (door_up or door_down) 2 else 4,
            );

            var created_z_door = false;

            //door_direction = 3;

            if (door_direction == 3) {
                created_z_door = true;
                door_down = true;
            } else if (door_direction == 2) {
                created_z_door = true;
                door_up = true;
            } else if (door_direction == 1) {
                door_right = true;
            } else {
                door_top = true;
            }

            _ = addStandardRoom(
                game_state,
                screen_x * tiles_per_width + tiles_per_width / 2,
                screen_y * tiles_per_height + tiles_per_height / 2,
                abs_tile_z,
            );

            for (0..tiles_per_height) |tile_y| {
                for (0..tiles_per_width) |tile_x| {
                    const abs_tile_x = screen_x * tiles_per_width +
                        @as(i32, @intCast(tile_x));
                    const abs_tile_y = screen_y * tiles_per_height +
                        @as(i32, @intCast(tile_y));

                    var should_be_door = false;

                    if (tile_x == 0 and (!door_left or (tile_y != tiles_per_height / 2))) {
                        should_be_door = true;
                    }

                    if (tile_x == (tiles_per_width - 1) and
                        (!door_right or (tile_y != tiles_per_height / 2)))
                    {
                        should_be_door = true;
                    }

                    if (tile_y == 0 and (!door_bottom or (tile_x != tiles_per_width / 2))) {
                        should_be_door = true;
                    }

                    if (tile_y == (tiles_per_height - 1) and
                        (!door_top or tile_x != (tiles_per_width / 2)))
                    {
                        should_be_door = true;
                    }

                    if (should_be_door) {
                        _ = addWall(game_state, abs_tile_x, abs_tile_y, abs_tile_z);
                    } else if (created_z_door) {
                        if ((@mod(abs_tile_z, 2) == 0 and tile_x == 10 and tile_y == 5) or
                            (@mod(abs_tile_z, 2) != 0 and tile_x == 6 and tile_y == 5))
                        {
                            _ = addStair(game_state, abs_tile_x, abs_tile_y, if (door_down) abs_tile_z - 1 else abs_tile_z);
                        }
                    }
                }
            }

            door_left = door_right;
            door_bottom = door_top;

            if (created_z_door) {
                door_down = !door_down;
                door_up = !door_up;
            } else {
                door_up = false;
                door_down = false;
            }

            door_right = false;
            door_top = false;

            if (door_direction == 3) {
                abs_tile_z -= 1;
            } else if (door_direction == 2) {
                abs_tile_z += 1;
            } else if (door_direction == 1) {
                screen_x += 1;
            } else {
                screen_y += 1;
            }
        }

        //while (game_state.low_entity_count < game_state.low_entities.len - 16) {
        //    const coordinate: u32 = @intCast(1024 + game_state.low_entity_count);
        //    _ = addWall(game_state, coordinate, coordinate, coordinate);
        //}

        const camera_tile_x = screen_base_x * tiles_per_width + 17 / 2;
        const camera_tile_y = screen_base_y * tiles_per_height + 9 / 2;
        const camera_tile_z = screen_base_z;

        const new_camera_p = chunkPosFromTilePos(
            game_world,
            camera_tile_x,
            camera_tile_y,
            camera_tile_z,
            Vec3.splat(0),
        );

        game_state.camera_p = new_camera_p;

        _ = addMonster(
            game_state,
            camera_tile_x - 3,
            camera_tile_y + 2,
            camera_tile_z,
        );

        for (0..1) |_| {
            const familiar_offset_x = random.i32Between(&series, -7, 7);
            const familiar_offset_y = random.i32Between(&series, -3, -1);

            if (familiar_offset_x != 0 or familiar_offset_y != 0) {
                _ = addFamiliar(
                    game_state,
                    camera_tile_x + familiar_offset_x,
                    camera_tile_y + familiar_offset_y,
                    camera_tile_z,
                );
            }
        }

        memory.is_initialized = true;
    }

    // NOTE: Transient initialization
    assert(@sizeOf(TransientState) <= memory.transient_storage_size);
    var transient_state: *TransientState = @alignCast(@ptrCast(memory.transient_storage));

    if (!transient_state.is_initialized) {
        initializeArena(&transient_state.arena, memory.transient_storage_size - @sizeOf(TransientState), memory.transient_storage + @sizeOf(TransientState));
        defer checkArena(&transient_state.arena);

        // TODO: Pick a real number here
        transient_state.ground_buffer_count = 64; // 128;
        transient_state.ground_buffers = pushArray(
            &transient_state.arena,
            transient_state.ground_buffer_count,
            GroundBuffer,
        );

        for (0..transient_state.ground_buffer_count) |ground_buffer_index| {
            var ground_buffer = &transient_state.ground_buffers[ground_buffer_index];

            ground_buffer.bitmap = makeEmptyBitmap(
                &transient_state.arena,
                ground_buffer_width,
                ground_buffer_height,
                false,
            );

            ground_buffer.p = world.nullPosition();
        }

        game_state.test_diffuse = makeEmptyBitmap(&transient_state.arena, 256, 256, false);

        render.drawRectangle(
            &game_state.test_diffuse,
            Vec2.splat(0),
            Vec2.fromInt(game_state.test_diffuse.width, game_state.test_diffuse.height),
            vec4(0.5, 0.5, 0.5, 1),
        );

        game_state.test_normal = makeEmptyBitmap(
            &transient_state.arena,
            game_state.test_diffuse.width,
            game_state.test_diffuse.height,
            false,
        );

        makeSphereNormalMap(&game_state.test_normal, 0, 1, 1);
        makeSphereDiffuseMap(&game_state.test_diffuse, 1, 1);
        //makeSphereNormalMap(&game_state.test_normal, 0, 0, 1);
        //makePyramidNormalMap(&game_state.test_normal, 0);

        transient_state.env_map_width = 512;
        transient_state.env_map_height = 256;

        for (0..transient_state.env_maps.len) |map_index| {
            var map = &transient_state.env_maps[map_index];
            var width = transient_state.env_map_width;
            var height = transient_state.env_map_height;

            for (0..map.lod.len) |lod_index| {
                map.lod[lod_index] = makeEmptyBitmap(
                    &transient_state.arena,
                    @intCast(width),
                    @intCast(height),
                    false,
                );

                width >>= 1;
                height >>= 1;
            }
        }

        transient_state.is_initialized = true;
    }

    if (false)
        if (input.executable_reloaded) {
            for (0..transient_state.ground_buffer_count) |ground_buffer_index| {
                var ground_buffer = &transient_state.ground_buffers[ground_buffer_index];
                ground_buffer.p = world.nullPosition();
            }
        };

    const game_world = game_state.world.?;

    //
    // NOTE: Movement
    //

    for (0..input.controllers.len) |controller_index| {
        const controller: *platform.GameControllerInput =
            try platform.getController(input, controller_index);

        var hero = &game_state.controlled_heroes[controller_index];

        if (hero.entity_index == 0) {
            if (controller.buttons.map.start.ended_down) {
                hero.* = .{};
                hero.entity_index = addPlayer(game_state).low_index;
            }
        } else {
            hero.dz = 0;
            hero.ddp = Vec2.splat(0);
            hero.d_sword = Vec2.splat(0);

            if (controller.is_analog) {
                // NOTE: Use analog movement tuning
                hero.ddp = vec2(controller.stick_average_x, controller.stick_average_y);
            } else {
                // NOTE: Use digital movement tuning

                if (controller.buttons.map.move_up.ended_down) {
                    hero.ddp.v[1] = 1.0;
                }

                if (controller.buttons.map.move_down.ended_down) {
                    hero.ddp.v[1] = -1.0;
                }

                if (controller.buttons.map.move_left.ended_down) {
                    hero.ddp.v[0] = -1.0;
                }

                if (controller.buttons.map.move_right.ended_down) {
                    hero.ddp.v[0] = 1.0;
                }
            }

            if (controller.buttons.map.start.ended_down) {
                hero.dz = 3.0;
            }

            if (controller.buttons.map.action_up.ended_down) {
                hero.d_sword = vec2(0, 1);
            }

            if (controller.buttons.map.action_down.ended_down) {
                hero.d_sword = vec2(0, -1);
            }

            if (controller.buttons.map.action_left.ended_down) {
                hero.d_sword = vec2(-1, 0);
            }

            if (controller.buttons.map.action_right.ended_down) {
                hero.d_sword = vec2(1, 0);
            }
        }
    }

    //
    // NOTE: Render
    //

    const render_memory = beginTemporaryMemory(&transient_state.arena);
    defer endTemporaryMemory(render_memory);

    const draw_buffer = &Bitmap{
        .width = buffer.width,
        .height = buffer.height,
        .pitch = buffer.pitch,
        .memory = @ptrCast(buffer.memory),
    };

    // TODO: Decide what our pushbuffer size is
    var render_group = render.allocateRenderGroup(
        &transient_state.arena,
        platform.Megabytes(4),
        @intCast(draw_buffer.width),
        @intCast(draw_buffer.height),
    );

    render.clear(render_group, vec4(0.25, 0.25, 0.25, 0));

    const screen_center = vec2(
        0.5 * @as(f32, @floatFromInt(draw_buffer.width)),
        0.5 * @as(f32, @floatFromInt(draw_buffer.height)),
    );

    const screen_bounds = render.getCameraRectangleAtTarget(render_group);

    var camera_bounds_in_meters = Rectangle3.rectMinMax(
        vec3(screen_bounds.min.x(), screen_bounds.min.y(), 0),
        vec3(screen_bounds.max.x(), screen_bounds.max.y(), 0),
    );

    camera_bounds_in_meters.min.v[2] = -3.0 * game_state.typical_floor_height;
    camera_bounds_in_meters.max.v[2] = 1 * game_state.typical_floor_height;

    // NOTE: Draw the ground
    if (true) {
        // NOTE: Rendering
        for (0..transient_state.ground_buffer_count) |ground_buffer_index| {
            var ground_buffer = &transient_state.ground_buffers[ground_buffer_index];

            if (world.isValid(&ground_buffer.p)) {
                const bitmap = &ground_buffer.bitmap;
                const delta = world.subtract(game_world, &ground_buffer.p, &game_state.camera_p);

                if (delta.z() >= -1.0 and delta.z() < 1.0) {
                    var basis: *RenderBasis = pushStruct(&transient_state.arena, RenderBasis);
                    render_group.default_basis = basis;
                    basis.p = delta;

                    const ground_side_in_meters = game_world.chunk_dim_in_meters.x();
                    render.pushBitmap(render_group, bitmap, ground_side_in_meters, Vec3.splat(0), Vec4.splat(1));

                    if (true)
                        render.pushRectOutline(
                            render_group,
                            Vec3.splat(0),
                            Vec2.splat(ground_side_in_meters),
                            vec4(1, 1, 0, 1),
                        );
                }
            }
        }

        // NOTE: Updating
        {
            const min_chunk_p = world.mapIntoChunkSpace(
                game_world,
                game_state.camera_p,
                Rectangle3.getMinCorner(&camera_bounds_in_meters),
            );

            const max_chunk_p = world.mapIntoChunkSpace(
                game_world,
                game_state.camera_p,
                Rectangle3.getMaxCorner(&camera_bounds_in_meters),
            );

            var chunk_z = min_chunk_p.chunk_z;
            while (chunk_z <= max_chunk_p.chunk_z) : (chunk_z += 1) {
                var chunk_y = min_chunk_p.chunk_y;
                while (chunk_y <= max_chunk_p.chunk_y) : (chunk_y += 1) {
                    var chunk_x = min_chunk_p.chunk_x;
                    while (chunk_x <= max_chunk_p.chunk_x) : (chunk_x += 1) {
                        var chunk_center_p = world.centeredChunkPoint(chunk_x, chunk_y, chunk_z);

                        const rel_p = world.subtract(
                            game_world,
                            &chunk_center_p,
                            &game_state.camera_p,
                        );
                        _ = rel_p;

                        // TODO: This is super inefficient, fix it
                        var furthest_buffer_length_sq: f32 = 0;
                        var maybe_furthest_buffer: ?*GroundBuffer = null;

                        for (0..transient_state.ground_buffer_count) |ground_buffer_index| {
                            const ground_buffer = &transient_state.ground_buffers[ground_buffer_index];

                            if (world.inSameChunk(game_world, &ground_buffer.p, &chunk_center_p)) {
                                maybe_furthest_buffer = null;
                                break;
                            } else if (world.isValid(&ground_buffer.p)) {
                                var d = world.subtract(game_world, &ground_buffer.p, &game_state.camera_p);

                                const buffer_length_sq = Vec2.lengthSquared(&d.xy());

                                if (furthest_buffer_length_sq < buffer_length_sq) {
                                    furthest_buffer_length_sq = buffer_length_sq;
                                    maybe_furthest_buffer = ground_buffer;
                                }
                            } else {
                                furthest_buffer_length_sq = std.math.floatMax(f32);
                                maybe_furthest_buffer = ground_buffer;
                            }
                        }

                        if (maybe_furthest_buffer) |furthest_buffer| {
                            fillGroundChunk(
                                transient_state,
                                game_state,
                                furthest_buffer,
                                &chunk_center_p,
                            );
                        }
                    }
                }
            }
        }
    }

    // TODO: How big do we actually want to expand here?
    // TODO: Do we want to simulate upper floors, etc.
    const sim_bounds_expansion = vec3(15, 15, 0);
    const sim_bounds = Rectangle3.addRadius(&camera_bounds_in_meters, &sim_bounds_expansion);
    const sim_memory = beginTemporaryMemory(&transient_state.arena);
    defer endTemporaryMemory(sim_memory);

    var sim_center_p = game_state.camera_p;
    var region = sim.beginSim(&transient_state.arena, game_state, game_world, sim_center_p, sim_bounds, input.dt_for_frame);
    // TODO: Make sure we hoist the camera update out to a place where the renderer can know
    // about the location of the camera at the end of the frame so there isn't a frame of lag
    // in camera updating compared to the hero
    defer sim.endSim(region, game_state);

    // NOTE: This is the camera position relative to the origin of this region
    const camera_p = world.subtract(game_world, &game_state.camera_p, &sim_center_p);

    var _basis: *RenderBasis = pushStruct(&transient_state.arena, RenderBasis);
    _basis.p = Vec3.splat(0);
    render_group.default_basis = _basis;

    render.pushRectOutline(render_group, Vec3.splat(0), screen_bounds.getDim(), vec4(1, 1, 0, 1));
    //render.pushRectOutline(render_group, Vec3.splat(0), camera_bounds_in_meters.getDim().xy(), vec4(1, 1, 1, 1));
    render.pushRectOutline(render_group, Vec3.splat(0), sim_bounds.getDim().xy(), vec4(0, 1, 1, 1));
    render.pushRectOutline(render_group, Vec3.splat(0), region.bounds.getDim().xy(), vec4(1, 0, 1, 1));

    // TODO: Move this out into the zigmade_entity
    for (0..region.entity_count) |index| {
        var entity = &region.entities[index];

        if (entity.updatable) {
            const dt = input.dt_for_frame;

            // TODO: This is incorrect, should be computed after update
            var shadow_alpha = 1.0 - 0.5 * entity.p.z();

            if (shadow_alpha < 0.0) {
                shadow_alpha = 0.0;
            }

            var move_spec = ety.defaultMoveSpec();
            var ddp = Vec3.splat(0);

            var basis: *RenderBasis = pushStruct(&transient_state.arena, RenderBasis);
            render_group.default_basis = basis;

            // TODO: Probably indicates we want to separate update and render for entities
            // sometime soon
            const camera_relative_ground_p = Vec3.sub(&sim.getEntityGroundPoint(entity), &camera_p);
            const fade_top_end_z: f32 = 0.75 * game_state.typical_floor_height;
            const fade_top_start_z: f32 = 0.5 * game_state.typical_floor_height;
            const fade_bottom_start_z: f32 = -2.0 * game_state.typical_floor_height;
            const fade_bottom_end_z: f32 = -2.25 * game_state.typical_floor_height;
            render_group.global_alpha = 1.0;

            if (camera_relative_ground_p.z() > fade_top_start_z) {
                render_group.global_alpha = math.clamp01MapToRange(fade_top_end_z, camera_relative_ground_p.z(), fade_top_start_z);
            } else if (camera_relative_ground_p.z() < fade_bottom_start_z) {
                render_group.global_alpha = math.clamp01MapToRange(fade_bottom_end_z, camera_relative_ground_p.z(), fade_bottom_start_z);
            }

            var hero_bitmaps = &game_state.hero_bitmaps[entity.facing_direction];

            switch (entity.type) {
                .hero => {
                    // TODO: Now that we have some real usage examples, let's
                    // solidify the positioning system
                    for (0..game_state.controlled_heroes.len) |control_index| {
                        const hero = &game_state.controlled_heroes[control_index];

                        if (entity.storage_index == hero.entity_index) {
                            if (hero.dz != 0) {
                                entity.dp.v[2] = hero.dz;
                            }

                            move_spec = .{
                                .unit_max_acc_vector = true,
                                .speed = 50,
                                .drag = 8,
                            };

                            ddp = vec3(hero.ddp.x(), hero.ddp.y(), 0);

                            if (hero.d_sword.x() != 0 or hero.d_sword.y() != 0) {
                                switch (entity.sword) {
                                    .ptr => {
                                        const sword = entity.sword.ptr;

                                        if (sword.flags.non_spatial) {
                                            sword.distance_limit = 5;

                                            var dp = Vec3.scale(
                                                &vec3(hero.d_sword.x(), hero.d_sword.y(), 0),
                                                5,
                                            );

                                            dp = Vec3.add(&entity.dp, &dp);

                                            ety.makeEntitySpatial(sword, entity.p, dp);

                                            addCollisionRule(
                                                game_state,
                                                sword.storage_index,
                                                entity.storage_index,
                                                false,
                                            );
                                        }
                                    },
                                    else => {},
                                }
                            }
                        }
                    }

                    // TODO: z
                    const hero_size_c = 2.5;
                    render.pushBitmap(render_group, &game_state.shadow, hero_size_c * 1.0, Vec3.splat(0), vec4(1, 1, 1, shadow_alpha));
                    render.pushBitmap(render_group, &hero_bitmaps.torso, hero_size_c * 1.2, Vec3.splat(0), Vec4.splat(1));
                    render.pushBitmap(render_group, &hero_bitmaps.cape, hero_size_c * 1.2, Vec3.splat(0), Vec4.splat(1));
                    render.pushBitmap(render_group, &hero_bitmaps.head, hero_size_c * 1.2, Vec3.splat(0), Vec4.splat(1));

                    drawHitPoints(entity, render_group);
                },
                .wall => {
                    render.pushBitmap(render_group, &game_state.tree, 2.5, Vec3.splat(0), Vec4.splat(1));
                },
                .stairwell => {
                    render.pushRect(render_group, Vec3.splat(0), entity.walkable_dim, vec4(1, 0.5, 0, 1));
                    render.pushRect(render_group, vec3(0, 0, entity.walkable_height), entity.walkable_dim, vec4(1, 1, 0, 1));
                },
                .sword => {
                    move_spec = .{
                        .unit_max_acc_vector = false,
                        .speed = 0,
                        .drag = 0,
                    };

                    // TODO: IMPORTANT: Add the ability in collision routines to understand
                    // movement limit for an entity and then update this routine to use this
                    // to know when to remove the sword
                    //
                    // TODO: Need to handle the fact that distance_traveled might
                    // not have enough distance for the total entity move
                    // for the frame

                    if (entity.distance_limit == 0) {
                        clearCollisionRulesFor(game_state, entity.storage_index);
                        ety.makeEntityNonSpatial(entity);
                    }

                    render.pushBitmap(render_group, &game_state.shadow, 0.5, Vec3.splat(0), vec4(0, 0, 0, shadow_alpha));
                    render.pushBitmap(render_group, &game_state.sword, 0.5, Vec3.splat(0), Vec4.splat(1));
                },
                .familiar => {
                    var maybe_closest_hero: ?*Entity = null;
                    var closest_hero_d_sq = math.square(10); // NOTE: Ten meter max search

                    if (false)
                        // TODO: Make spatial queries easy for things
                        for (0..region.entity_count) |test_index| {
                            const test_entity = &region.entities[test_index];

                            if (test_entity.type == .hero) {
                                const diff = Vec3.sub(&test_entity.p, &entity.p);
                                const test_d_sq = Vec3.lengthSquared(&diff);

                                if (closest_hero_d_sq > test_d_sq) {
                                    maybe_closest_hero = test_entity;
                                    closest_hero_d_sq = test_d_sq;
                                }
                            }
                        };

                    if (maybe_closest_hero) |closest_hero| {
                        if (closest_hero_d_sq > math.square(3)) {
                            const acceleration = 0.5;
                            const one_over_length = acceleration / @sqrt(closest_hero_d_sq);

                            const diff = Vec3.sub(&closest_hero.p, &entity.p);
                            ddp = Vec3.scale(&diff, one_over_length);
                        }
                    }

                    move_spec = .{
                        .unit_max_acc_vector = true,
                        .speed = 50,
                        .drag = 8,
                    };

                    entity.t_bob += dt;

                    if (entity.t_bob > 2 * std.math.pi) {
                        entity.t_bob -= 2 * std.math.pi;
                    }

                    const bob_sin = @sin(2 * entity.t_bob);
                    const shadow_color = vec4(1, 1, 1, 0.5 * shadow_alpha + 0.2 * bob_sin);
                    render.pushBitmap(render_group, &game_state.shadow, 2.5, Vec3.splat(0), shadow_color);
                    render.pushBitmap(render_group, &hero_bitmaps.head, 2.5, vec3(0, 0, 0.25 * bob_sin), Vec4.splat(1));
                },
                .monster => {
                    render.pushBitmap(render_group, &game_state.shadow, 4.5, Vec3.splat(0), vec4(0, 0, 0, shadow_alpha));
                    render.pushBitmap(render_group, &hero_bitmaps.torso, 4.5, Vec3.splat(0), Vec4.splat(1));
                    drawHitPoints(entity, render_group);
                },
                .space => {
                    if (false) {
                        for (0..entity.collision.volume_count) |volume_index| {
                            const volume = &entity.collision.volumes.?[volume_index];

                            render.pushRectOutline(render_group, Vec3.sub(&volume.offset_p, &vec3(0, 0, 0.5 * volume.dim.z())), volume.dim.xy(), vec4(0, 0.5, 1, 1));
                        }
                    }
                },
                else => unreachable,
            }

            if (!entity.flags.non_spatial and entity.flags.movable) {
                sim.moveEntity(game_state, region, entity, input.dt_for_frame, &move_spec, ddp);
            }

            basis.p = sim.getEntityGroundPoint(entity);
        }
    }

    render_group.global_alpha = 1.0;

    // NOTE: Normal map sampling
    if (false) {
        game_state.time += input.dt_for_frame;

        const map_color = [3]Vec3{
            vec3(1, 0, 0),
            vec3(0, 1, 0),
            vec3(0, 0, 1),
        };

        for (0..transient_state.env_maps.len) |map_index| {
            var map = &transient_state.env_maps[map_index];
            const lod = &map.lod[0];
            const checker_width = 16;
            const checker_height = 16;
            var row_checker_on = false;

            var y: usize = 0;
            while (y < lod.height) : (y += checker_height) {
                var checker_on = row_checker_on;

                var x: usize = 0;
                while (x < lod.width) : (x += checker_width) {
                    const color = if (checker_on)
                        map_color[map_index].toVec4(1)
                    else
                        vec4(0, 0, 0, 1);

                    const min_p = Vec2.fromInt(x, y);
                    const max_p = Vec2.add(&min_p, &Vec2.fromInt(checker_width, checker_height));
                    render.drawRectangle(lod, min_p, max_p, color);
                    checker_on = !checker_on;
                }

                row_checker_on = !row_checker_on;
            }
        }

        transient_state.env_maps[0].pz = -1.5;
        transient_state.env_maps[1].pz = 0.0;
        transient_state.env_maps[2].pz = 1.5;

        render.drawBitmap(&transient_state.env_maps[0].lod[0], &transient_state.ground_buffers[transient_state.ground_buffer_count - 1].bitmap, 125, 50, 1);

        // angle = 0;

        // TODO: Let's add a perp operator
        const origin = screen_center;

        const angle = 0.5 * game_state.time;
        const displacement = if (true)
            vec2(100 * @cos(5 * angle), 100 * @sin(3 * angle))
        else
            Vec2.splat(0);

        var x_axis: Vec2 = undefined;
        var y_axis: Vec2 = undefined;

        if (true) {
            x_axis = Vec2.scale(&vec2(@cos(10 * angle), @sin(10 * angle)), 100);
            y_axis = Vec2.perp(&x_axis);
        } else {
            x_axis = vec2(100, 0);
            y_axis = vec2(0, 100);
        }

        //var p_index: usize = 0;
        const c_angle = 5 * angle;

        const color = if (false)
            vec4(
                0.5 + 0.5 * @sin(c_angle),
                0.5 + 0.5 * @sin(2.9 * c_angle),
                0.5 + 0.5 * @cos(9.9 * c_angle),
                0.5 + 0.5 * @sin(10 * c_angle),
            )
        else
            Vec4.splat(1);

        //_ = displacement;
        _ = render.coordinateSystem(
            render_group,
            Vec2.add(
                &displacement,
                &Vec2.sub(
                    &Vec2.sub(&origin, &Vec2.scale(&x_axis, 0.5)),
                    &Vec2.scale(&y_axis, 0.5),
                ),
            ),
            x_axis,
            y_axis,
            color,
            &game_state.test_diffuse,
            &game_state.test_normal,
            &transient_state.env_maps[2],
            &transient_state.env_maps[1],
            &transient_state.env_maps[0],
        );

        var map_p = Vec2.splat(0);

        for (0..transient_state.env_maps.len) |map_index| {
            var map = &transient_state.env_maps[map_index];
            const lod = &map.lod[0];

            x_axis = Vec2.scale(&vec2(@floatFromInt(lod.width), 0), 0.5);
            y_axis = Vec2.scale(&vec2(0, @floatFromInt(lod.height)), 0.5);

            _ = render.coordinateSystem(
                render_group,
                map_p,
                x_axis,
                y_axis,
                Vec4.splat(1),
                lod,
                null,
                null,
                null,
                null,
            );

            map_p = Vec2.add(&map_p, &Vec2.add(
                &y_axis,
                &vec2(0, 6),
            ));
        }

        if (false)
            render.saturation(render_group, 0.5 + 0.5 * @sin(10 * game_state.time));
    }

    render.renderGroupToOutput(
        render_group,
        draw_buffer,
    );
}

// NOTE: At the moment, this must be a very fast function
// It cannot be greater than ~1ms
// TODO: Reduce the pressure on this function's performance
// by measuring it or asking about it, etc.
pub export fn getSoundSamples(
    thread: *platform.ThreadContext,
    memory: *platform.GameMemory,
    sound_buffer: *platform.GameSoundBuffer,
) void {
    _ = thread;
    const game_state: *GameState = @as(
        *GameState,
        @alignCast(@ptrCast(memory.permanent_storage)),
    );

    try gameOutputSound(
        sound_buffer,
        game_state,
    );
}
