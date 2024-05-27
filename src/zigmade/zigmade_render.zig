const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");

const Bitmap = game.Bitmap;
const MemoryArena = game.MemoryArena;
const Vec2 = math.Vec2;
const Vec3 = math.Vec3;
const Vec4 = math.Vec4;

pub const RenderBasis = struct {
    p: Vec3,
};

pub const EntityVisiblePiece = struct {
    basis: *RenderBasis,
    bitmap: ?*game.Bitmap = null,
    offset: Vec2,
    offset_z: f32,
    entity_zc: f32,
    r: f32,
    g: f32,
    b: f32,
    a: f32,
    dim: Vec2,
};

// TODO: This is dumb, this should just be part of
// the renderer pushbuffer. Add correction of coordinates
// in there and be done with it
pub const RenderGroup = struct {
    default_basis: *RenderBasis,
    meters_to_pixels: f32,
    piece_count: u32 = 0,
    max_push_buffer_size: usize,
    push_buffer_size: usize,
    push_buffer_base: [*]u8,
};

pub inline fn pushRenderElement(
    group: *RenderGroup,
    size: usize,
) [*]void {
    var result: [*]void = undefined;

    if ((group.push_buffer_size + size) < group.max_push_buffer_size) {
        result = @ptrCast(group.push_buffer_base + group.push_buffer_size);
        group.push_buffer_size += size;
    } else {
        game.invalidCodePath();
    }

    return result;
}

pub inline fn pushPiece(
    group: *RenderGroup,
    bitmap: ?*Bitmap,
    offset: Vec2,
    offset_z: f32,
    alignment: Vec2,
    dim: Vec2,
    color: Vec4,
    entity_zc: f32,
) void {
    const piece: *EntityVisiblePiece = @alignCast(@ptrCast(pushRenderElement(
        group,
        @sizeOf(EntityVisiblePiece),
    )));

    piece.basis = group.default_basis;
    piece.bitmap = bitmap;

    piece.offset = Vec2.sub(
        &Vec2.scale(
            &Vec2.init(offset.x(), -offset.y()),
            group.meters_to_pixels,
        ),
        &alignment,
    );

    piece.offset_z = offset_z;
    piece.entity_zc = entity_zc;
    piece.r = color.r();
    piece.g = color.g();
    piece.b = color.b();
    piece.a = color.a();
    piece.dim = dim;
}

pub inline fn pushBitmap(
    group: *RenderGroup,
    bitmap: *Bitmap,
    offset: Vec2,
    offset_z: f32,
    alignment: Vec2,
    alpha: f32,
    entity_zc: f32,
) void {
    pushPiece(
        group,
        bitmap,
        offset,
        offset_z,
        alignment,
        Vec2.splat(0),
        Vec4.init(1, 1, 1, alpha),
        entity_zc,
    );
}

pub inline fn pushRect(
    group: *RenderGroup,
    offset: Vec2,
    offset_z: f32,
    dim: Vec2,
    color: Vec4,
    entity_zc: f32,
) void {
    pushPiece(group, null, offset, offset_z, Vec2.splat(0), dim, color, entity_zc);
}

inline fn pushRectOutline(
    group: *RenderGroup,
    offset: Vec2,
    offset_z: f32,
    dim: Vec2,
    color: Vec4,
    entity_zc: f32,
) void {
    const thickness = 0.1;

    // NOTE: Top and bottom
    pushPiece(
        group,
        null,
        Vec2.sub(&offset, &Vec2.init(0, 0.5 * dim.y())),
        offset_z,
        Vec2.splat(0),
        Vec2.init(dim.x(), thickness),
        color,
        entity_zc,
    );

    pushPiece(
        group,
        null,
        Vec2.add(&offset, &Vec2.init(0, 0.5 * dim.y())),
        offset_z,
        Vec2.splat(0),
        Vec2.init(dim.x(), thickness),
        color,
        entity_zc,
    );

    // NOTE: Left and right
    pushPiece(
        group,
        null,
        Vec2.sub(&offset, &Vec2.init(0.5 * dim.x(), 0)),
        offset_z,
        Vec2.splat(0),
        Vec2.init(thickness, dim.y()),
        color,
        entity_zc,
    );

    pushPiece(
        group,
        null,
        Vec2.add(&offset, &Vec2.init(0.5 * dim.x(), 0)),
        offset_z,
        Vec2.splat(0),
        Vec2.init(thickness, dim.y()),
        color,
        entity_zc,
    );
}

pub fn allocateRenderGroup(
    arena: *MemoryArena,
    max_push_buffer_size: usize,
    meters_to_pixels: f32,
) *RenderGroup {
    var result: *RenderGroup = game.pushStruct(arena, RenderGroup);

    result.push_buffer_base = game.pushSize(
        arena,
        @alignOf(@TypeOf(max_push_buffer_size)),
        max_push_buffer_size,
    );

    result.default_basis = game.pushStruct(arena, RenderBasis);
    result.default_basis.p = Vec3.splat(0);
    result.meters_to_pixels = meters_to_pixels;
    result.piece_count = 0;
    result.max_push_buffer_size = max_push_buffer_size;
    result.push_buffer_size = 0;

    return result;
}
