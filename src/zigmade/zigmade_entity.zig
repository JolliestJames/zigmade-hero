const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const world = @import("zigmade_world.zig");
const sim = @import("zigmade_sim_region.zig");

const Vec3 = math.Vec3;
const Rectangle2 = math.Rectangle2;
const LowEntity = game.LowEntity;
const Entity = sim.Entity;
const SimRegion = sim.SimRegion;
const MoveSpec = sim.MoveSpec;
const GameState = game.GameState;
const World = world.World;
const WorldPosition = world.WorldPosition;

pub inline fn invalidPos() Vec3 {
    return Vec3.init(100000, 100000, 100000);
}

pub inline fn defaultMoveSpec() MoveSpec {
    const result: MoveSpec = .{
        .unit_max_acc_vector = true,
        .speed = 1,
        .drag = 0,
    };

    return result;
}

pub inline fn makeEntityNonSpatial(entity: *Entity) void {
    entity.flags.non_spatial = true;
    entity.pos = invalidPos();
}

pub inline fn makeEntitySpatial(
    entity: *Entity,
    pos: Vec3,
    d_pos: Vec3,
) void {
    entity.flags.non_spatial = false;
    entity.pos = pos;
    entity.d_pos = d_pos;
}
