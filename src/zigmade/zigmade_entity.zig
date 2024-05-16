const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const world = @import("zigmade_world.zig");
const sim = @import("zigmade_sim_region.zig");

const Vec3 = math.Vec3;
const Entity = sim.Entity;
const MoveSpec = sim.MoveSpec;

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
    entity.p = invalidPos();
}

pub inline fn makeEntitySpatial(
    entity: *Entity,
    pos: Vec3,
    d_pos: Vec3,
) void {
    entity.flags.non_spatial = false;
    entity.p = pos;
    entity.dp = d_pos;
}
