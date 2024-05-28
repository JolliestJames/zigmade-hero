const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const platform = @import("zigmade_platform");
const ety = @import("zigmade_entity.zig");

const lossyCast = std.math.lossyCast;

const Bitmap = game.Bitmap;
const MemoryArena = game.MemoryArena;
const Vec2 = math.Vec2;
const Vec3 = math.Vec3;
const Vec4 = math.Vec4;

pub const RenderBasis = struct {
    p: Vec3 = Vec3.splat(0),
};

const RenderEntityBasis = extern struct {
    basis: *RenderBasis,
    offset: Vec2,
    offset_z: f32,
    entity_zc: f32,
};

// NOTE: Is there a better approximation for what a
// "compact discriminated union" should look like in Zig?
pub const RenderGroupEntryType = enum(u8) {
    clear,
    bitmap,
    rectangle,
};

pub const RenderGroupEntryHeader = extern struct {
    type: RenderGroupEntryType,
};

pub const RenderEntryClear = extern struct {
    header: RenderGroupEntryHeader,
    r: f32,
    g: f32,
    b: f32,
    a: f32,
};

pub const RenderEntryBitmap = extern struct {
    header: RenderGroupEntryHeader,
    bitmap: ?*game.Bitmap = null,
    entity_basis: RenderEntityBasis,
    r: f32,
    g: f32,
    b: f32,
    a: f32,
};

pub const RenderEntryRectangle = extern struct {
    header: RenderGroupEntryHeader,
    entity_basis: RenderEntityBasis,
    dim: Vec2,
    r: f32,
    g: f32,
    b: f32,
    a: f32,
};

// TODO: This is dumb, this should just be part of
// the renderer pushbuffer. Add correction of coordinates
// in there and be done with it
pub const RenderGroup = struct {
    default_basis: *RenderBasis,
    meters_to_pixels: f32,
    max_push_buffer_size: usize,
    push_buffer_size: usize,
    push_buffer_base: [*]u8,
};

pub inline fn pushRenderElement(
    group: *RenderGroup,
    comptime T: type,
    comptime entry_type: RenderGroupEntryType,
) ?*T {
    var result: ?*T = null;

    const header = pushRenderElement_(
        group,
        @sizeOf(T),
        entry_type,
    );

    result = @alignCast(@ptrCast(header));

    return result;
}

pub inline fn pushRenderElement_(
    group: *RenderGroup,
    size: usize,
    comptime entry_type: RenderGroupEntryType,
) ?*RenderGroupEntryHeader {
    var result: ?*RenderGroupEntryHeader = null;

    if ((group.push_buffer_size + size) < group.max_push_buffer_size) {
        result = @ptrCast(group.push_buffer_base + group.push_buffer_size);
        result.?.type = entry_type;
        group.push_buffer_size += size;
    } else unreachable;

    return result;
}

pub inline fn pushPiece(
    group: *RenderGroup,
    bitmap: ?*Bitmap,
    offset: Vec2,
    offset_z: f32,
    alignment: Vec2,
    color: Vec4,
    entity_zc: f32,
) void {
    const maybe_piece = pushRenderElement(
        group,
        RenderEntryBitmap,
        .bitmap,
    );

    if (maybe_piece) |piece| {
        piece.entity_basis.basis = group.default_basis;
        piece.bitmap = bitmap;

        piece.entity_basis.offset = Vec2.sub(
            &Vec2.scale(
                &Vec2.init(offset.x(), -offset.y()),
                group.meters_to_pixels,
            ),
            &alignment,
        );

        piece.entity_basis.offset_z = offset_z;
        piece.entity_basis.entity_zc = entity_zc;
        piece.r = color.r();
        piece.g = color.g();
        piece.b = color.b();
        piece.a = color.a();
    }
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
    const maybe_piece = pushRenderElement(
        group,
        RenderEntryRectangle,
        .rectangle,
    );

    if (maybe_piece) |piece| {
        const half_dim = Vec2.scale(&dim, 0.5 * group.meters_to_pixels);

        piece.entity_basis.basis = group.default_basis;

        piece.entity_basis.offset = Vec2.sub(
            &Vec2.scale(
                &Vec2.init(offset.x(), -offset.y()),
                group.meters_to_pixels,
            ),
            &half_dim,
        );

        piece.entity_basis.offset_z = offset_z;
        piece.entity_basis.entity_zc = entity_zc;
        piece.r = color.r();
        piece.g = color.g();
        piece.b = color.b();
        piece.a = color.a();
        piece.dim = Vec2.scale(&dim, group.meters_to_pixels);
    }
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

inline fn getRenderEntityBasisP(
    render_group: *RenderGroup,
    entity_basis: *RenderEntityBasis,
    screen_center: Vec2,
) Vec2 {
    const entity_base_p = entity_basis.basis.p;
    const z_fudge = 1.0 + 0.1 * (entity_base_p.z() + entity_basis.offset_z);

    const eg_x = screen_center.x() + render_group.meters_to_pixels * z_fudge * entity_base_p.x();
    const eg_y = screen_center.y() - render_group.meters_to_pixels * z_fudge * entity_base_p.y();
    const entity_z = -render_group.meters_to_pixels * entity_base_p.z();

    const result = Vec2.init(
        eg_x + entity_basis.offset.x(),
        eg_y + entity_basis.offset.y() + entity_basis.entity_zc * entity_z,
    );

    return result;
}

pub fn renderGroupToOutput(
    render_group: *RenderGroup,
    output_target: *const Bitmap,
) void {
    const screen_center = Vec2.init(
        0.5 * @as(f32, @floatFromInt(output_target.width)),
        0.5 * @as(f32, @floatFromInt(output_target.height)),
    );

    var base: usize = 0;
    while (base < render_group.push_buffer_size) {
        const header: *RenderGroupEntryHeader = @ptrCast(render_group.push_buffer_base + base);

        switch (header.type) {
            .clear => {
                const entry = @as(*RenderEntryClear, @alignCast(@ptrCast(header)));

                base += @sizeOf(@TypeOf(entry.*));
            },
            .bitmap => {
                const entry = @as(*RenderEntryBitmap, @alignCast(@ptrCast(header)));
                const p = getRenderEntityBasisP(render_group, &entry.entity_basis, screen_center);

                if (entry.bitmap) |bitmap| {
                    // NOTE: With Casey's implementation, there will be one iteration of the game
                    // loop when a sword has transitioned from spatial to non_spatial during which
                    // a draw attempt will be made without this check for an invalid position in place.
                    // This makes it clear why avoiding use of a non spatial entity's position is
                    // important. An attempt to draw at that position in this case will cause an
                    // integer part of floating point value out of bounds panic.
                    if (!std.meta.eql(entry.entity_basis.basis.p.v, ety.invalidPos().v)) {
                        drawBitmap(output_target, bitmap, p.x(), p.y(), entry.a);
                    }
                } else unreachable;

                base += @sizeOf(@TypeOf(entry.*));
            },
            .rectangle => {
                const entry = @as(*RenderEntryRectangle, @alignCast(@ptrCast(header)));
                const p = getRenderEntityBasisP(render_group, &entry.entity_basis, screen_center);

                drawRectangle(
                    output_target,
                    p,
                    Vec2.add(&p, &entry.dim),
                    entry.r,
                    entry.g,
                    entry.b,
                );

                base += @sizeOf(@TypeOf(entry.*));
            },
        }
    }
}

pub fn allocateRenderGroup(
    arena: *MemoryArena,
    max_push_buffer_size: usize,
    meters_to_pixels: f32,
) *RenderGroup {
    var result: *RenderGroup = game.pushStruct(arena, RenderGroup);

    result.push_buffer_base = game.pushSize(
        arena,
        @alignOf(u8),
        max_push_buffer_size,
    );

    result.default_basis = game.pushStruct(arena, RenderBasis);
    result.default_basis.p = Vec3.splat(0);
    result.meters_to_pixels = meters_to_pixels;
    result.max_push_buffer_size = max_push_buffer_size;
    result.push_buffer_size = 0;

    return result;
}

pub fn drawRectangle(
    buffer: *const Bitmap,
    min: Vec2,
    max: Vec2,
    r: f32,
    g: f32,
    b: f32,
) void {
    var min_x: i32 = @intFromFloat(@round(min.x()));
    var min_y: i32 = @intFromFloat(@round(min.y()));
    var max_x: i32 = @intFromFloat(@round(max.x()));
    var max_y: i32 = @intFromFloat(@round(max.y()));

    if (min_x < 0) min_x = 0;
    if (min_y < 0) min_y = 0;
    if (max_x > buffer.width) max_x = buffer.width;
    if (max_y > buffer.height) max_y = buffer.height;
    if (min_x > max_x) max_x = min_x;
    if (min_y > max_y) max_y = min_y;

    const color: u32 =
        (@as(u32, (@intFromFloat(@round(r * 255.0)))) << 16) |
        (@as(u32, (@intFromFloat(@round(g * 255.0)))) << 8) |
        (@as(u32, (@intFromFloat(@round(b * 255.0)))) << 0);

    var row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
        (@as(u32, @intCast(min_x)) *
        @as(u32, @intCast(platform.BITMAP_BYTES_PER_PIXEL))) +
        @as(u32, @bitCast(min_y *% buffer.pitch));

    for (@intCast(min_y)..@intCast(max_y)) |_| {
        var pixel: [*]u32 = @alignCast(@ptrCast(row));

        for (@intCast(min_x)..@intCast(max_x)) |_| {
            pixel[0] = color;
            pixel += 1;
        }

        row += @as(u32, @intCast(buffer.pitch));
    }
}

fn drawRectOutline(buffer: *const Bitmap, min: Vec2, max: Vec2, color: Vec3, r: f32) void {
    // NOTE: Top and bottom
    drawRectangle(
        buffer,
        Vec2.init(min.x() - r, min.y() - r),
        Vec2.init(max.x() + r, min.y() + r),
        color.r(),
        color.g(),
        color.b(),
    );

    drawRectangle(
        buffer,
        Vec2.init(min.x() - r, max.y() - r),
        Vec2.init(max.x() + r, max.y() + r),
        color.r(),
        color.g(),
        color.b(),
    );

    // NOTE: Left and right
    drawRectangle(
        buffer,
        Vec2.init(min.x() - r, min.y() - r),
        Vec2.init(min.x() + r, max.y() + r),
        color.r(),
        color.g(),
        color.b(),
    );

    drawRectangle(
        buffer,
        Vec2.init(max.x() - r, min.y() - r),
        Vec2.init(max.x() + r, max.y() + r),
        color.r(),
        color.g(),
        color.b(),
    );
}

pub fn drawBitmap(
    buffer: *const Bitmap,
    bitmap: *Bitmap,
    real_x: f32,
    real_y: f32,
    c_alpha: f32,
) void {
    var min_x: i32 = @intFromFloat(@round(real_x));
    var min_y: i32 = @intFromFloat(@round(real_y));
    var max_x: i32 = min_x + bitmap.width;
    var max_y: i32 = min_y + bitmap.height;
    //var max_x: i32 = @intFromFloat(@round(real_x + @as(f32, @floatFromInt(bitmap.width))));
    //var max_y: i32 = @intFromFloat(@round(real_y + @as(f32, @floatFromInt(bitmap.height))));

    var source_offset_x: i32 = 0;
    if (min_x < 0) {
        source_offset_x = -min_x;
        min_x = 0;
    }

    var source_offset_y: i32 = 0;
    if (min_y < 0) {
        source_offset_y = -min_y;
        min_y = 0;
    }

    if (max_x > buffer.width) max_x = @intCast(buffer.width);
    if (max_y > buffer.height) max_y = @intCast(buffer.height);
    if (min_x > max_x) max_x = min_x;
    if (min_y > max_y) max_y = min_y;

    var source_row: [*]u8 = undefined;
    const bitmap_offset = source_offset_y * bitmap.pitch +
        platform.BITMAP_BYTES_PER_PIXEL * source_offset_x;

    if (bitmap_offset > 0) {
        source_row = @as([*]u8, @ptrCast(bitmap.memory)) +
            @as(usize, @intCast(bitmap_offset));
    } else {
        source_row = @as([*]u8, @ptrCast(bitmap.memory)) -
            @as(usize, @intCast(-bitmap_offset));
    }

    var dest_row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
        (@as(u32, @intCast(min_x)) *
        @as(u32, @intCast(platform.BITMAP_BYTES_PER_PIXEL))) +
        @as(u32, @bitCast(min_y *% buffer.pitch));

    for (@intCast(min_y)..@intCast(max_y)) |_| {
        var dest: [*]u32 = @alignCast(@ptrCast(dest_row));
        var source: [*]align(@alignOf(u8)) u32 = @alignCast(@ptrCast(source_row));

        for (@intCast(min_x)..@intCast(max_x)) |_| {
            const sa: f32 = @floatFromInt((source[0] >> 24) & 0xFF);
            const rsa = sa / 255 * c_alpha;
            const sr: f32 = c_alpha * @as(f32, @floatFromInt((source[0] >> 16) & 0xFF));
            const sg: f32 = c_alpha * @as(f32, @floatFromInt((source[0] >> 8) & 0xFF));
            const sb: f32 = c_alpha * @as(f32, @floatFromInt((source[0] >> 0) & 0xFF));

            const da: f32 = @floatFromInt((dest[0] >> 24) & 0xFF);
            const dr: f32 = @floatFromInt((dest[0] >> 16) & 0xFF);
            const dg: f32 = @floatFromInt((dest[0] >> 8) & 0xFF);
            const db: f32 = @floatFromInt((dest[0] >> 0) & 0xFF);
            const rda = da / 255;

            const inv_rsa: f32 = 1 - rsa;
            // TODO: Check this for math errors
            const a = 255 * (rsa + rda - rsa * rda);
            const r = inv_rsa * dr + sr;
            const g = inv_rsa * dg + sg;
            const b = inv_rsa * db + sb;

            dest[0] = (lossyCast(u32, a + 0.5) << 24) |
                (lossyCast(u32, r + 0.5) << 16) |
                (lossyCast(u32, g + 0.5) << 8) |
                (lossyCast(u32, b + 0.5) << 0);

            dest += 1;
            source += 1;
        }

        dest_row += @as(usize, @intCast(buffer.pitch));

        if (bitmap.pitch > 0) {
            source_row += @as(usize, @intCast(bitmap.pitch));
        } else {
            source_row -= @as(usize, @intCast(-bitmap.pitch));
        }
    }
}

fn drawMatte(
    buffer: *const Bitmap,
    bitmap: *Bitmap,
    real_x: f32,
    real_y: f32,
    c_alpha: f32,
) void {
    var min_x: i32 = @intFromFloat(@round(real_x));
    var min_y: i32 = @intFromFloat(@round(real_y));
    var max_x: i32 = min_x + bitmap.width;
    var max_y: i32 = min_y + bitmap.height;

    var source_offset_x: i32 = 0;
    if (min_x < 0) {
        source_offset_x = -min_x;
        min_x = 0;
    }

    var source_offset_y: i32 = 0;
    if (min_y < 0) {
        source_offset_y = -min_y;
        min_y = 0;
    }

    if (max_x > buffer.width) max_x = @intCast(buffer.width);
    if (max_y > buffer.height) max_y = @intCast(buffer.height);
    if (min_x > max_x) max_x = min_x;
    if (min_y > max_y) max_y = min_y;

    var source_row: [*]u8 = undefined;
    const bitmap_offset = source_offset_y * bitmap.pitch +
        platform.BITMAP_BYTES_PER_PIXEL * source_offset_x;

    if (bitmap_offset > 0) {
        source_row = @as([*]u8, @ptrCast(bitmap.memory)) +
            @as(usize, @intCast(bitmap_offset));
    } else {
        source_row = @as([*]u8, @ptrCast(bitmap.memory)) -
            @as(usize, @intCast(-bitmap_offset));
    }

    var dest_row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
        (@as(u32, @intCast(min_x)) *
        @as(u32, @intCast(platform.BITMAP_BYTES_PER_PIXEL))) +
        @as(u32, @bitCast(min_y *% buffer.pitch));

    for (@intCast(min_y)..@intCast(max_y)) |_| {
        var dest: [*]u32 = @alignCast(@ptrCast(dest_row));
        var source: [*]align(@alignOf(u8)) u32 = @alignCast(@ptrCast(source_row));

        for (@intCast(min_x)..@intCast(max_x)) |_| {
            const sa: f32 = @floatFromInt((source[0] >> 24) & 0xFF);
            const rsa = sa / 255 * c_alpha;

            const da: f32 = @floatFromInt((dest[0] >> 24) & 0xFF);
            const dr: f32 = @floatFromInt((dest[0] >> 16) & 0xFF);
            const dg: f32 = @floatFromInt((dest[0] >> 8) & 0xFF);
            const db: f32 = @floatFromInt((dest[0] >> 0) & 0xFF);

            const inv_rsa: f32 = 1 - rsa;
            // TODO: Check this for math errors
            const a = inv_rsa * da;
            const r = inv_rsa * dr;
            const g = inv_rsa * dg;
            const b = inv_rsa * db;

            dest[0] = (lossyCast(u32, a + 0.5) << 24) |
                (lossyCast(u32, r + 0.5) << 16) |
                (lossyCast(u32, g + 0.5) << 8) |
                (lossyCast(u32, b + 0.5) << 0);

            dest += 1;
            source += 1;
        }

        dest_row += @as(usize, @intCast(buffer.pitch));

        if (bitmap.pitch > 0) {
            source_row += @as(usize, @intCast(bitmap.pitch));
        } else {
            source_row -= @as(usize, @intCast(-bitmap.pitch));
        }
    }
}
