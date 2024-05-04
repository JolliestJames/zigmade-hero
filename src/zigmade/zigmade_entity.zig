const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const world = @import("zigmade_world.zig");
const sim = @import("zigmade_sim_region.zig");

const Vec2 = math.Vec2;
const Rectangle2 = math.Rectangle2;
const LowEntity = game.LowEntity;
const Entity = sim.SimEntity;
const SimRegion = sim.SimRegion;
const MoveSpec = sim.MoveSpec;
const GameState = game.GameState;
const World = world.World;
const WorldPosition = world.WorldPosition;

pub inline fn defaultMoveSpec() MoveSpec {
    const result: MoveSpec = .{
        .unit_max_acc_vector = true,
        .speed = 1,
        .drag = 0,
    };

    return result;
}

pub fn updateFamiliar(region: *SimRegion, entity: *Entity, dt: f32) void {
    var maybe_closest_hero: ?*Entity = null;
    var closest_hero_d_sq = math.square(10); // NOTE: Ten meter max search

    // TODO: Make spatial queries easy for things
    for (0..region.entity_count) |index| {
        const test_entity = &region.entities[index];

        if (test_entity.type == .hero) {
            const diff = math.sub(test_entity.pos, entity.pos);
            var test_d_sq = math.lengthSquared(diff);

            test_d_sq *= 0.75;

            if (closest_hero_d_sq > test_d_sq) {
                maybe_closest_hero = test_entity;
                closest_hero_d_sq = test_d_sq;
            }
        }
    }

    var dd_p: Vec2 = .{};

    if (maybe_closest_hero) |closest_hero| {
        if (closest_hero_d_sq > math.square(3)) {
            const acceleration = 0.5;
            const one_over_length = acceleration / @sqrt(closest_hero_d_sq);

            const diff =
                math.sub(closest_hero.pos, entity.pos);
            dd_p = math.scale(diff, one_over_length);
        }
    }

    var move_spec = defaultMoveSpec();

    move_spec = .{
        .unit_max_acc_vector = true,
        .speed = 50,
        .drag = 8,
    };

    sim.moveEntity(region, entity, dt, &move_spec, dd_p);
}

pub fn updateMonster(_: *SimRegion, _: *Entity, _: f32) void {}

pub fn updateSword(region: *SimRegion, entity: *Entity, dt: f32) void {
    var move_spec = defaultMoveSpec();

    move_spec = .{
        .unit_max_acc_vector = false,
        .speed = 0,
        .drag = 0,
    };

    const old_pos = entity.pos;
    sim.moveEntity(region, entity, dt, &move_spec, .{});
    const diff = math.sub(entity.pos, old_pos);
    const distance_traveled = math.length(diff);
    entity.distance_remaining -= distance_traveled;

    if (entity.distance_remaining < 0) {
        std.debug.print("NEED TO MAKE ENTITIES BE ABLE TO NOT BE THERE\n", .{});
        assert(false);
    }
}
