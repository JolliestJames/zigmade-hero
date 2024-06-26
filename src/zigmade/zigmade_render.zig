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

pub const EnvironmentMap = struct {
    lod: [4]Bitmap,
};

pub const RenderBasis = extern struct {
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
// TODO: Remove the header
pub const RenderGroupEntryType = enum(u8) {
    clear,
    bitmap,
    rectangle,
    coordinate_system,
    saturation,
};

pub const RenderGroupEntryHeader = extern struct {
    type: RenderGroupEntryType,
};

pub const RenderEntryClear = extern struct {
    color: Vec4,
};

pub const RenderEntrySaturation = extern struct {
    level: f32,
};

pub const RenderEntryCoordinateSystem = extern struct {
    origin: Vec2,
    x_axis: Vec2,
    y_axis: Vec2,
    color: Vec4,
    texture: *Bitmap,
    normal_map: ?*Bitmap,
    top: ?*EnvironmentMap,
    middle: ?*EnvironmentMap,
    bottom: ?*EnvironmentMap,
};

pub const RenderEntryBitmap = extern struct {
    bitmap: ?*Bitmap = null,
    entity_basis: RenderEntityBasis,
    color: Vec4,
};

pub const RenderEntryRectangle = extern struct {
    entity_basis: RenderEntityBasis,
    dim: Vec2,
    color: Vec4,
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
) ?*align(@alignOf(void)) T {
    var result: ?*align(@alignOf(void)) T = null;

    const header = pushRenderElement_(group, @sizeOf(T), entry_type);

    result = @ptrCast(header);

    return result;
}

pub inline fn pushRenderElement_(
    group: *RenderGroup,
    _size: usize,
    comptime entry_type: RenderGroupEntryType,
) ?*void {
    var result: ?*void = null;

    const size = _size + @sizeOf(RenderGroupEntryHeader);

    if ((group.push_buffer_size + size) < group.max_push_buffer_size) {
        var header: *RenderGroupEntryHeader = @ptrCast(group.push_buffer_base + group.push_buffer_size);
        header.type = entry_type;
        result = @ptrCast(@as([*]u8, @ptrCast(header)) + @sizeOf(@TypeOf(header.*)));
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
    const maybe_piece = pushRenderElement(group, RenderEntryBitmap, .bitmap);

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
        piece.color = color;
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
    const maybe_piece = pushRenderElement(group, RenderEntryRectangle, .rectangle);

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
        piece.color = color;
        piece.dim = Vec2.scale(&dim, group.meters_to_pixels);
    }
}

pub inline fn pushRectOutline(
    group: *RenderGroup,
    offset: Vec2,
    offset_z: f32,
    dim: Vec2,
    color: Vec4,
    entity_zc: f32,
) void {
    const thickness = 0.1;

    // NOTE: Top and bottom
    pushRect(
        group,
        Vec2.sub(&offset, &Vec2.init(0, 0.5 * dim.y())),
        offset_z,
        Vec2.init(dim.x(), thickness),
        color,
        entity_zc,
    );

    pushRect(
        group,
        Vec2.add(&offset, &Vec2.init(0, 0.5 * dim.y())),
        offset_z,
        Vec2.init(dim.x(), thickness),
        color,
        entity_zc,
    );

    // NOTE: Left and right
    pushRect(
        group,
        Vec2.sub(&offset, &Vec2.init(0.5 * dim.x(), 0)),
        offset_z,
        Vec2.init(thickness, dim.y()),
        color,
        entity_zc,
    );

    pushRect(
        group,
        Vec2.add(&offset, &Vec2.init(0.5 * dim.x(), 0)),
        offset_z,
        Vec2.init(thickness, dim.y()),
        color,
        entity_zc,
    );
}

pub inline fn clear(group: *RenderGroup, color: Vec4) void {
    const maybe_entry = pushRenderElement(group, RenderEntryClear, .clear);

    if (maybe_entry) |entry| {
        entry.color = color;
    }
}

pub inline fn saturation(group: *RenderGroup, level: f32) void {
    const maybe_entry = pushRenderElement(group, RenderEntrySaturation, .saturation);

    if (maybe_entry) |entry| {
        entry.level = level;
    }
}

pub inline fn coordinateSystem(
    group: *RenderGroup,
    origin: Vec2,
    x_axis: Vec2,
    y_axis: Vec2,
    color: Vec4,
    texture: *Bitmap,
    normal_map: ?*Bitmap,
    top: ?*EnvironmentMap,
    middle: ?*EnvironmentMap,
    bottom: ?*EnvironmentMap,
) *align(@alignOf(void)) RenderEntryCoordinateSystem {
    const maybe_entry = pushRenderElement(
        group,
        RenderEntryCoordinateSystem,
        .coordinate_system,
    );

    if (maybe_entry) |entry| {
        entry.origin = origin;
        entry.x_axis = x_axis;
        entry.y_axis = y_axis;
        entry.color = color;
        entry.texture = texture;
        entry.normal_map = normal_map;
        entry.top = top;
        entry.middle = middle;
        entry.bottom = bottom;
    }

    return maybe_entry.?;
}

inline fn getRenderEntityBasisP(
    render_group: *RenderGroup,
    entity_basis: *align(@alignOf(void)) RenderEntityBasis,
    screen_center: Vec2,
) Vec2 {
    const entity_base_p = entity_basis.basis.p;
    const z_fudge = 1.0 + 0.1 * (entity_base_p.z() + entity_basis.offset_z);

    const eg_x = screen_center.x() + render_group.meters_to_pixels * z_fudge * entity_base_p.x();
    const eg_y = screen_center.y() - render_group.meters_to_pixels * z_fudge * entity_base_p.y();
    const entity_z = -render_group.meters_to_pixels * entity_base_p.z();

    const result = Vec2.init(
        eg_x + entity_basis.offset.v[0],
        eg_y + entity_basis.offset.v[1] + entity_basis.entity_zc * entity_z,
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
    while (base < render_group.push_buffer_size) : (base += @sizeOf(RenderGroupEntryHeader)) {
        const header: *RenderGroupEntryHeader = @ptrCast(render_group.push_buffer_base + base);
        const data = @as([*]u8, @ptrCast(header)) + @sizeOf(@TypeOf(header.*));

        switch (header.type) {
            .clear => {
                const entry: *align(@alignOf(void)) RenderEntryClear = @alignCast(@ptrCast(data));

                drawRectangle(
                    output_target,
                    Vec2.splat(0),
                    Vec2.fromInt(output_target.width, output_target.height),
                    entry.color,
                );

                base += @sizeOf(@TypeOf(entry.*));
            },
            .saturation => {
                const entry: *align(@alignOf(void)) RenderEntrySaturation = @alignCast(@ptrCast(data));

                changeSaturation(output_target, entry.level);

                base += @sizeOf(@TypeOf(entry.*));
            },
            .bitmap => {
                const entry = @as(*align(@alignOf(void)) RenderEntryBitmap, @ptrCast(data));
                const p = getRenderEntityBasisP(render_group, &entry.entity_basis, screen_center);

                if (false)
                    if (entry.bitmap) |bitmap| {
                        // NOTE: With Casey's implementation, there will be one iteration of the game
                        // loop when a sword has transitioned from spatial to non_spatial during which
                        // a draw attempt will be made without this check for an invalid position in place.
                        // This makes it clear why avoiding use of a non spatial entity's position is
                        // important. An attempt to draw at that position in this case will cause an
                        // integer part of floating point value out of bounds panic.
                        if (!std.meta.eql(entry.entity_basis.basis.p.v, ety.invalidPos().v)) {
                            drawBitmap(output_target, bitmap, p.x(), p.y(), entry.color.v[3]);
                        }
                    } else unreachable;

                base += @sizeOf(@TypeOf(entry.*));
            },
            .rectangle => {
                const entry: *align(@alignOf(void)) RenderEntryRectangle = @ptrCast(data);
                const p = getRenderEntityBasisP(render_group, &entry.entity_basis, screen_center);
                const dim = entry.dim;

                drawRectangle(
                    output_target,
                    p,
                    Vec2.add(@alignCast(&p), @alignCast(&dim)),
                    entry.color,
                );

                base += @sizeOf(@TypeOf(entry.*));
            },
            .coordinate_system => {
                const entry: *align(@alignOf(void)) RenderEntryCoordinateSystem =
                    @alignCast(@ptrCast(data));
                const origin = entry.origin;
                const x_axis = entry.x_axis;
                const y_axis = entry.y_axis;

                var v_max = Vec2.add(&y_axis, &Vec2.add(&origin, &x_axis));

                if (true)
                    drawRectangleSlowly(
                        output_target,
                        entry.origin,
                        entry.x_axis,
                        entry.y_axis,
                        entry.color,
                        entry.texture,
                        entry.normal_map,
                        entry.top,
                        entry.middle,
                        entry.bottom,
                    );

                const color = Vec4.init(1, 1, 0, 1);
                const dim = Vec2.splat(2);
                var p = entry.origin;

                drawRectangle(output_target, Vec2.sub(&p, &dim), Vec2.add(&p, &dim), color);

                p = Vec2.add(&origin, &x_axis);

                drawRectangle(output_target, Vec2.sub(&p, &dim), Vec2.add(&p, &dim), color);

                p = Vec2.add(&origin, &y_axis);

                drawRectangle(output_target, Vec2.sub(&p, &dim), Vec2.add(&p, &dim), color);
                drawRectangle(output_target, Vec2.sub(&v_max, &dim), Vec2.add(&v_max, &dim), color);

                if (false)
                    for (0..entry.points.len) |p_index| {
                        p = entry.points[p_index];

                        p = Vec2.add(
                            &Vec2.add(
                                &entry.origin,
                                &Vec2.scale(&entry.x_axis, p.x()),
                            ),
                            &Vec2.scale(&entry.y_axis, p.y()),
                        );

                        drawRectangle(
                            output_target,
                            Vec2.sub(&p, &dim),
                            Vec2.add(&p, &dim),
                            entry.color.r(),
                            entry.color.g(),
                            entry.color.b(),
                            entry.color.a(),
                        );
                    };

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

pub inline fn unpack4x8(to_unpack: u32) Vec4 {
    const result = Vec4.init(
        @floatFromInt((to_unpack >> 16) & 0xFF),
        @floatFromInt((to_unpack >> 8) & 0xFF),
        @floatFromInt((to_unpack >> 0) & 0xFF),
        @floatFromInt((to_unpack >> 24) & 0xFF),
    );

    return result;
}

pub inline fn SRGB255ToLinear1(c: Vec4) Vec4 {
    var result: Vec4 = undefined;

    const inv_255 = 1.0 / 255.0;

    result.v[0] = math.square(inv_255 * c.r());
    result.v[1] = math.square(inv_255 * c.g());
    result.v[2] = math.square(inv_255 * c.b());
    result.v[3] = inv_255 * c.a();

    return result;
}

pub inline fn linear1ToSRGB255(c: Vec4) Vec4 {
    var result: Vec4 = undefined;

    const one_255 = 255.0;

    result.v[0] = one_255 * @sqrt(c.r());
    result.v[1] = one_255 * @sqrt(c.g());
    result.v[2] = one_255 * @sqrt(c.b());
    result.v[3] = one_255 * c.a();

    return result;
}

inline fn unscaleAndBiasNormal(normal: Vec4) Vec4 {
    var result: Vec4 = undefined;

    const inv_255 = 1.0 / 255.0;

    result.v[0] = -1.0 + 2.0 * (inv_255 * normal.x());
    result.v[1] = -1.0 + 2.0 * (inv_255 * normal.y());
    result.v[2] = -1.0 + 2.0 * (inv_255 * normal.z());
    result.v[3] = inv_255 * normal.w();

    return result;
}

pub fn drawRectangle(
    buffer: *const Bitmap,
    v_min: Vec2,
    v_max: Vec2,
    color: Vec4,
) void {
    const r = color.r();
    const g = color.g();
    const b = color.b();
    const a = color.a();

    var min_x: i32 = @intFromFloat(@round(v_min.x()));
    var min_y: i32 = @intFromFloat(@round(v_min.y()));
    var max_x: i32 = @intFromFloat(@round(v_max.x()));
    var max_y: i32 = @intFromFloat(@round(v_max.y()));

    if (min_x < 0) min_x = 0;
    if (min_y < 0) min_y = 0;
    if (max_x > buffer.width) max_x = buffer.width;
    if (max_y > buffer.height) max_y = buffer.height;
    if (min_x > max_x) max_x = min_x;
    if (min_y > max_y) max_y = min_y;

    const color32: u32 =
        (@as(u32, (@intFromFloat(@round(a * 255.0)))) << 24) |
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
            pixel[0] = color32;
            pixel += 1;
        }

        row += @as(u32, @intCast(buffer.pitch));
    }
}

inline fn SRGBBilinearBlend(texel_sample: BilinearSample, fx: f32, fy: f32) Vec4 {
    var texel_a = unpack4x8(texel_sample.a);
    var texel_b = unpack4x8(texel_sample.b);
    var texel_c = unpack4x8(texel_sample.c);
    var texel_d = unpack4x8(texel_sample.d);

    // NOTE: Go from srgb to "linear" brightness space
    texel_a = SRGB255ToLinear1(texel_a);
    texel_b = SRGB255ToLinear1(texel_b);
    texel_c = SRGB255ToLinear1(texel_c);
    texel_d = SRGB255ToLinear1(texel_d);

    const result = Vec4.lerp(
        &Vec4.lerp(&texel_a, fx, &texel_b),
        fy,
        &Vec4.lerp(&texel_c, fx, &texel_d),
    );

    return result;
}

pub inline fn sampleEnvironmentMap(
    screen_space_uv: Vec2,
    sample_direction: Vec3,
    roughness: f32,
    maybe_map: ?*EnvironmentMap,
    distance_from_map_in_z: f32,
) Vec3 {
    //
    // NOTE: screen_space_uv tells us where the ray is being cast from in
    // normalized screen coordinates
    //
    // sample_direction tells us what direction the cast is going -- it is
    // does not need to be normalized, but its y must be positive
    //
    // roughness says which lods of map we sample from
    //
    // distance_from_map_in_z says how far the map is from the sample point
    // in z, given in meters
    //

    var result: Vec3 = undefined;

    if (maybe_map) |map| {
        // NOTE: Pick which LOD to sample from
        const lod_index: u32 = @intFromFloat(roughness *
            @as(f32, @floatFromInt(map.lod.len - 1)) + 0.5);
        assert(lod_index < maybe_map.?.lod.len);

        const lod = &map.lod[lod_index];

        // NOTE: Compute the distance to the map and the scaling
        // factor for meters to uvs
        // TODO: Parameterize this, should be different for x and y
        // based on map
        const uvs_per_meter = 0.01;
        const c = (uvs_per_meter * distance_from_map_in_z) / sample_direction.y();

        // TODO: Make sure we know what direction z should go in y
        const offset = Vec2.scale(
            &Vec2.init(sample_direction.x(), sample_direction.z()),
            c,
        );

        // NOTE: Find the intersection point
        var uv = Vec2.add(&screen_space_uv, &offset);

        // NOTE: Clamp to the valid range
        uv.v[0] = math.clamp01(uv.x());
        uv.v[1] = math.clamp01(uv.y());

        // NOTE: Bilinear sample
        // TODO: Formalize texture boundaries
        const tx = uv.x() * @as(f32, @floatFromInt(lod.width - 2));
        const ty = uv.y() * @as(f32, @floatFromInt(lod.height - 2));

        const ix: i32 = @intFromFloat(tx);
        const iy: i32 = @intFromFloat(ty);

        const fx = tx - @as(f32, @floatFromInt(ix));
        const fy = ty - @as(f32, @floatFromInt(iy));

        assert(ix >= 0 and ix < lod.width);
        assert(iy >= 0 and iy < lod.height);

        const texel_ptr = if (lod.pitch > 0)
            @as([*]u8, @ptrCast(lod.memory)) +
                @as(usize, @intCast(iy * lod.pitch)) +
                @as(usize, @intCast(ix)) * @sizeOf(u32)
        else
            @as([*]u8, @ptrCast(lod.memory)) -
                @as(usize, @intCast(iy * -lod.pitch)) +
                @as(usize, @intCast(ix)) * @sizeOf(u32);

        @as([*]u32, @alignCast(@ptrCast(texel_ptr)))[0] = 0xFFFFFFFF;

        const sample = bilinearSample(lod, ix, iy);
        result = SRGBBilinearBlend(sample, fx, fy).xyz();
    }

    return result;
}

const BilinearSample = struct {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
};

inline fn bilinearSample(texture: *Bitmap, x: i32, y: i32) BilinearSample {
    var result: BilinearSample = undefined;

    const offset = y * texture.pitch + x * @sizeOf(u32);

    const texel_ptr = if (offset > 0)
        @as([*]u8, @ptrCast(texture.memory)) + @as(usize, @intCast(offset))
    else
        @as([*]u8, @ptrCast(texture.memory)) - @as(usize, @intCast(-offset));

    const c_offset = if (texture.pitch > 0)
        texel_ptr + @as(usize, @intCast(texture.pitch))
    else
        texel_ptr - @as(usize, @intCast(-texture.pitch));

    const b_offset = texel_ptr + @sizeOf(u32);
    const d_offset = c_offset + @sizeOf(u32);

    result.a = @as(*align(@alignOf(u8)) u32, @ptrCast(texel_ptr)).*;
    result.b = @as(*align(@alignOf(u8)) u32, @ptrCast(b_offset)).*;
    result.c = @as(*align(@alignOf(u8)) u32, @ptrCast(c_offset)).*;
    result.d = @as(*align(@alignOf(u8)) u32, @ptrCast(d_offset)).*;

    return result;
}

pub fn drawRectangleSlowly(
    buffer: *const Bitmap,
    origin: Vec2,
    x_axis: Vec2,
    y_axis: Vec2,
    _color: Vec4,
    texture: *Bitmap,
    maybe_normal_map: ?*Bitmap,
    maybe_top: ?*EnvironmentMap,
    maybe_middle: ?*EnvironmentMap,
    maybe_bottom: ?*EnvironmentMap,
) void {
    //@setFloatMode(.Optimized);

    // NOTE: Premultiply color up front
    const color = _color.premultipliedAlpha(_color.a());

    const x_axis_len = x_axis.length();
    const y_axis_len = y_axis.length();
    const nx_axis = Vec2.scale(&x_axis, y_axis_len / x_axis_len);
    const ny_axis = Vec2.scale(&y_axis, x_axis_len / y_axis_len);

    // NOTE: nz_scale could be a parameter if we want to have
    // control over the amount of scaling in the z direction
    // that the normals appear to have
    const nz_scale = 0.5 * (x_axis_len + y_axis_len);

    const inv_x_axis_length_sq = 1 / Vec2.lengthSquared(&x_axis);
    const inv_y_axis_length_sq = 1 / Vec2.lengthSquared(&y_axis);

    const color32: u32 =
        (@as(u32, (@intFromFloat(@round(color.a() * 255.0)))) << 24) |
        (@as(u32, (@intFromFloat(@round(color.r() * 255.0)))) << 16) |
        (@as(u32, (@intFromFloat(@round(color.g() * 255.0)))) << 8) |
        (@as(u32, (@intFromFloat(@round(color.b() * 255.0)))) << 0);

    const width_max = buffer.width - 1;
    const height_max = buffer.height - 1;

    const inv_width_max = 1.0 / @as(f32, @floatFromInt(width_max));
    const inv_height_max = 1.0 / @as(f32, @floatFromInt(height_max));

    var x_min: i32 = width_max;
    var x_max: i32 = 0;
    var y_min: i32 = height_max;
    var y_max: i32 = 0;

    const p: [4]Vec2 = .{
        origin,
        Vec2.add(&origin, &x_axis),
        Vec2.add(&origin, &Vec2.add(&x_axis, &y_axis)),
        Vec2.add(&origin, &y_axis),
    };

    for (0..p.len) |p_index| {
        const test_p = p[p_index];
        const floor_x: i32 = @intFromFloat(@floor(test_p.x()));
        const ceil_x: i32 = @intFromFloat(@ceil(test_p.x()));
        const floor_y: i32 = @intFromFloat(@floor(test_p.y()));
        const ceil_y: i32 = @intFromFloat(@ceil(test_p.y()));

        if (x_min > floor_x) x_min = floor_x;
        if (y_min > floor_y) y_min = floor_y;
        if (x_max < ceil_x) x_max = ceil_x;
        if (y_max < ceil_y) y_max = ceil_y;
    }

    if (x_min < 0) x_min = 0;
    if (y_min < 0) y_min = 0;
    if (x_max > width_max) x_max = width_max;
    if (y_max > height_max) y_max = height_max;

    var row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
        (@as(u32, @intCast(x_min)) *
        @as(u32, @intCast(platform.BITMAP_BYTES_PER_PIXEL))) +
        @as(u32, @bitCast(y_min *% buffer.pitch));

    for (@intCast(y_min)..@intCast(y_max)) |y| {
        var pixel: [*]u32 = @alignCast(@ptrCast(row));

        for (@intCast(x_min)..@intCast(x_max)) |x| {
            if (true) {
                const pixel_p = Vec2.fromInt(x, y);
                const d = Vec2.sub(&pixel_p, &origin);

                // TODO: Perp inner
                // TODO: Simpler origin
                const edge_0: f32 = Vec2.inner(&d, &Vec2.negate(&Vec2.perp(&x_axis)));
                const edge_1: f32 = Vec2.inner(
                    &Vec2.sub(&d, &x_axis),
                    &Vec2.negate(&Vec2.perp(&y_axis)),
                );
                const edge_2: f32 = Vec2.inner(
                    &Vec2.sub(&Vec2.sub(&d, &x_axis), &y_axis),
                    &Vec2.perp(&x_axis),
                );
                const edge_3: f32 = Vec2.inner(&Vec2.sub(&d, &y_axis), &Vec2.perp(&y_axis));

                if (edge_0 < 0 and edge_1 < 0 and edge_2 < 0 and edge_3 < 0) {
                    const screen_space_uv = Vec2.init(
                        @as(f32, @floatFromInt(x)) * inv_width_max,
                        @as(f32, @floatFromInt(y)) * inv_height_max,
                    );

                    const u = inv_x_axis_length_sq * Vec2.inner(&d, &x_axis);
                    const v = inv_y_axis_length_sq * Vec2.inner(&d, &y_axis);

                    // TODO: SSE clamping
                    if (false) {
                        assert(u >= 0 and u <= 1);
                        assert(v >= 0 and v <= 1);
                    }

                    // TODO: Formalize texture boundaries
                    const tx = u * @as(f32, @floatFromInt(texture.width - 2));
                    const ty = v * @as(f32, @floatFromInt(texture.height - 2));

                    const ix: i32 = @intFromFloat(tx);
                    const iy: i32 = @intFromFloat(ty);

                    const fx = tx - @as(f32, @floatFromInt(ix));
                    const fy = ty - @as(f32, @floatFromInt(iy));

                    assert(ix >= 0 and ix < texture.width);
                    assert(iy >= 0 and iy < texture.height);

                    const texel_sample = bilinearSample(texture, ix, iy);
                    var texel = SRGBBilinearBlend(texel_sample, fx, fy);

                    if (maybe_normal_map) |normal_map| {
                        const normal_sample = bilinearSample(normal_map, ix, iy);

                        var normal_a = unpack4x8(normal_sample.a);
                        var normal_b = unpack4x8(normal_sample.b);
                        var normal_c = unpack4x8(normal_sample.c);
                        var normal_d = unpack4x8(normal_sample.d);

                        var normal = Vec4.lerp(
                            &Vec4.lerp(&normal_a, fx, &normal_b),
                            fy,
                            &Vec4.lerp(&normal_c, fx, &normal_d),
                        );

                        normal = unscaleAndBiasNormal(normal);
                        // TODO: Do we really need to do this?

                        // TODO: Rotate normals based on x/y axis
                        normal = normal.setXY(
                            Vec2.add(
                                &Vec2.scale(&nx_axis, normal.x()),
                                &Vec2.scale(&ny_axis, normal.y()),
                            ),
                        );
                        normal.v[2] *= nz_scale;
                        normal = normal.setXYZ(Vec3.normalize(&normal.xyz()));

                        // NOTE: The eye vector is always assumed to be [0, 0, 1]
                        // This is just the simplified version of -e + 2e^T N N
                        var bounce_direction = Vec3.scale(&normal.xyz(), 2 * normal.z());
                        bounce_direction.v[2] -= 1.0;

                        // TODO: Eventually we need to support two mappings, one for
                        // top-down view (which we don't do now) and one for sideways, which
                        // is what's happening here
                        bounce_direction.v[2] = -bounce_direction.z();

                        var maybe_far_map: ?*EnvironmentMap = null;
                        var distance_from_map_in_z: f32 = 2.0;
                        const t_env_map = bounce_direction.y();
                        var t_far_map: f32 = 0;

                        if (t_env_map < -0.5) {
                            // TODO: This path seems particularly broken
                            maybe_far_map = maybe_bottom;
                            t_far_map = -1.0 - 2 * t_env_map;
                            distance_from_map_in_z = -distance_from_map_in_z;
                        } else if (t_env_map > 0.5) {
                            maybe_far_map = maybe_top;
                            t_far_map = 2 * (t_env_map - 0.5);
                        }

                        // TODO: How do we sample from the middle map?
                        _ = maybe_middle;
                        var light_color = Vec3.splat(0);

                        if (maybe_far_map != null) {
                            var far_map_color = sampleEnvironmentMap(
                                screen_space_uv,
                                bounce_direction,
                                normal.w(),
                                maybe_far_map,
                                distance_from_map_in_z,
                            );

                            light_color = Vec3.lerp(&light_color, t_far_map, &far_map_color);

                            // TODO: Actually do a lighting model computation here

                            texel = texel.setRGB(Vec3.add(
                                &texel.rgb(),
                                &Vec3.scale(&light_color, texel.a()),
                            ));
                        }
                    }

                    texel = Vec4.hadamard(&texel, &color);
                    texel.v[0] = math.clamp01(texel.r());
                    texel.v[1] = math.clamp01(texel.g());
                    texel.v[2] = math.clamp01(texel.b());

                    var dest = Vec4.init(
                        @floatFromInt((pixel[0] >> 16) & 0xFF),
                        @floatFromInt((pixel[0] >> 8) & 0xFF),
                        @floatFromInt((pixel[0] >> 0) & 0xFF),
                        @floatFromInt((pixel[0] >> 24) & 0xFF),
                    );

                    // NOTE Go from srgb to "linear" brightness space
                    dest = SRGB255ToLinear1(dest);

                    const blended = Vec4.add(
                        &Vec4.scale(&dest, 1 - texel.a()),
                        &texel,
                    );

                    // NOTE: Go from "linear" brightness space to srgb
                    var blended_255 = linear1ToSRGB255(blended);

                    pixel[0] = (lossyCast(u32, blended_255.a() + 0.5) << 24) |
                        (lossyCast(u32, blended_255.r() + 0.5) << 16) |
                        (lossyCast(u32, blended_255.g() + 0.5) << 8) |
                        (lossyCast(u32, blended_255.b() + 0.5) << 0);
                }
            } else {
                pixel[0] = color32;
            }

            pixel += 1;
        }

        row += @as(u32, @intCast(buffer.pitch));
    }
}

pub fn drawRectOutline(buffer: *const Bitmap, min: Vec2, max: Vec2, color: Vec3, r: f32) void {
    // NOTE: Top and bottom
    drawRectangle(
        buffer,
        Vec2.init(min.x() - r, min.y() - r),
        Vec2.init(max.x() + r, min.y() + r),
        color.toVec4(1),
    );

    drawRectangle(
        buffer,
        Vec2.init(min.x() - r, max.y() - r),
        Vec2.init(max.x() + r, max.y() + r),
        color.toVec4(1),
    );

    // NOTE: Left and right
    drawRectangle(
        buffer,
        Vec2.init(min.x() - r, min.y() - r),
        Vec2.init(min.x() + r, max.y() + r),
        color.toVec4(1),
    );

    drawRectangle(
        buffer,
        Vec2.init(max.x() - r, min.y() - r),
        Vec2.init(max.x() + r, max.y() + r),
        color.toVec4(1),
    );
}

pub fn drawBitmap(
    buffer: *const Bitmap,
    bitmap: *Bitmap,
    real_x: f32,
    real_y: f32,
    c_alpha: f32,
) void {
    //@setFloatMode(.Optimized);

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
        var source: [*]align(@alignOf(u8)) u32 = @ptrCast(source_row);

        for (@intCast(min_x)..@intCast(max_x)) |_| {
            var texel = Vec4.init(
                @as(f32, @floatFromInt((source[0] >> 16) & 0xFF)),
                @as(f32, @floatFromInt((source[0] >> 8) & 0xFF)),
                @as(f32, @floatFromInt((source[0] >> 0) & 0xFF)),
                @floatFromInt((source[0] >> 24) & 0xFF),
            );

            texel = SRGB255ToLinear1(texel);
            texel = Vec4.scale(&texel, c_alpha);

            var d = Vec4.init(
                @floatFromInt((dest[0] >> 16) & 0xFF),
                @floatFromInt((dest[0] >> 8) & 0xFF),
                @floatFromInt((dest[0] >> 0) & 0xFF),
                @floatFromInt((dest[0] >> 24) & 0xFF),
            );

            d = SRGB255ToLinear1(d);

            var result = Vec4.add(
                &Vec4.scale(&d, 1 - texel.a()),
                &texel,
            );

            result = linear1ToSRGB255(result);

            dest[0] = (lossyCast(u32, result.a() + 0.5) << 24) |
                (lossyCast(u32, result.r() + 0.5) << 16) |
                (lossyCast(u32, result.g() + 0.5) << 8) |
                (lossyCast(u32, result.b() + 0.5) << 0);

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

pub fn changeSaturation(
    buffer: *const Bitmap,
    level: f32,
) void {
    var dest_row: [*]u8 = @alignCast(@ptrCast(buffer.memory));

    for (0..@intCast(buffer.height)) |_| {
        var dest: [*]u32 = @alignCast(@ptrCast(dest_row));

        for (0..@intCast(buffer.width)) |_| {
            var d = Vec4.init(
                @floatFromInt((dest[0] >> 16) & 0xFF),
                @floatFromInt((dest[0] >> 8) & 0xFF),
                @floatFromInt((dest[0] >> 0) & 0xFF),
                @floatFromInt((dest[0] >> 24) & 0xFF),
            );

            d = SRGB255ToLinear1(d);

            const avg = 1.0 / 3.0 * (d.r() + d.g() + d.b());
            const delta = Vec3.init(d.r() - avg, d.g() - avg, d.b() - avg);

            var result = Vec3.add(
                &Vec3.init(avg, avg, avg),
                &Vec3.scale(&delta, level),
            ).toVec4(d.a());

            result = linear1ToSRGB255(result);

            dest[0] = (lossyCast(u32, result.a() + 0.5) << 24) |
                (lossyCast(u32, result.r() + 0.5) << 16) |
                (lossyCast(u32, result.g() + 0.5) << 8) |
                (lossyCast(u32, result.b() + 0.5) << 0);

            dest += 1;
        }

        dest_row += @as(usize, @intCast(buffer.pitch));
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
        var source: [*]align(@alignOf(u8)) u32 = @ptrCast(source_row);

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
