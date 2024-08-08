// NOTE
//
// 1. Everywhere outside the renderer, y always goes upward, x to the right
//
// 2. All bitmaps including the render target are assumed to be bottom-up
// (meaning that the first row pointer points to the bottom-most row when
// viewed on screen)
//
// 3. It is mandatory that all inputs to the renderer are in world
// coordinates (meters), not pixels. If something absolutely has to be
// specified in pixels, that will be marked explicitly in the API, but
// this should be rare
//
// 4. z is a special coordinate because it is broken up into discrete slices,
// and the renderer actually understands these slices. z slices are what control
// the scaling of things, whereas z offsets inside a slice are what control y
// offsetting
//
// TODO: ZHANDLING
//
// 5. All color values specified to the renderer as Vec4s are in non-premultiplied
// alpha
//

const std = @import("std");
const assert = std.debug.assert;
const game = @import("zigmade.zig");
const math = @import("zigmade_math.zig");
const platform = @import("zigmade_platform");
const ety = @import("zigmade_entity.zig");

const lossyCast = std.math.lossyCast;

const MemoryArena = game.MemoryArena;
const Vec2 = math.Vec2;
const Vec3 = math.Vec3;
const Vec4 = math.Vec4;
const Rectangle2 = math.Rectangle2;
const vec2 = math.vec2;
const vec3 = math.vec3;
const vec4 = math.vec4;

pub const Bitmap = struct {
    align_percentage: Vec2 = Vec2.splat(0),
    width_over_height: f32 = 0,
    width: i32,
    height: i32,
    pitch: i32,
    memory: ?[*]void,
};

pub const EnvironmentMap = struct {
    lod: [4]Bitmap,
    pz: f32,
};

pub const RenderBasis = extern struct {
    p: Vec3 = Vec3.splat(0),
};

const RenderEntityBasis = extern struct {
    basis: *RenderBasis,
    offset: Vec3,
    scale: f32,
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

pub const RenderEntryBitmap = extern struct {
    bitmap: ?*Bitmap = null,
    entity_basis: RenderEntityBasis,
    size: Vec2,
    color: Vec4,
};

pub const RenderEntryRectangle = extern struct {
    entity_basis: RenderEntityBasis,
    dim: Vec2,
    color: Vec4,
};

// NOTE: This is only for test
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

pub const RenderGroupCamera = struct {
    // NOTE: Camera parameters
    focal_length: f32,
    distance_above_target: f32,
};

// TODO: This is dumb, this should just be part of
// the renderer pushbuffer. Add correction of coordinates
// in there and be done with it
pub const RenderGroup = struct {
    game_camera: RenderGroupCamera,
    render_camera: RenderGroupCamera,
    // NOTE: Translates meters on the monitor into pixels on the monitor
    meters_to_pixels: f32,
    monitor_half_dim_in_meters: Vec2,
    global_alpha: f32,
    default_basis: *RenderBasis,
    max_push_buffer_size: usize,
    push_buffer_size: usize,
    push_buffer_base: [*]u8,
};

const BilinearSample = struct {
    a: u32,
    b: u32,
    c: u32,
    d: u32,
};

const EntityBasisPResult = struct {
    p: Vec2 = Vec2.splat(0),
    scale: f32 = 0,
    valid: bool = false,
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

pub inline fn pushBitmap(
    group: *RenderGroup,
    bitmap: *Bitmap,
    height: f32,
    offset: Vec3,
    color: Vec4,
) void {
    const maybe_entry = pushRenderElement(group, RenderEntryBitmap, .bitmap);

    if (maybe_entry) |entry| {
        entry.entity_basis.basis = group.default_basis;
        entry.bitmap = bitmap;

        const size = vec2(height * bitmap.width_over_height, height);
        const alignment = Vec2.hadamard(&bitmap.align_percentage, &size);
        const new_offset = Vec3.sub(&offset, &vec3(alignment.x(), alignment.y(), 0));

        entry.entity_basis.offset = new_offset;
        entry.color = Vec4.scale(&color, group.global_alpha);
        entry.size = size;
    }
}

pub inline fn pushRect(
    group: *RenderGroup,
    offset: Vec3,
    dim: Vec2,
    color: Vec4,
) void {
    const maybe_piece = pushRenderElement(group, RenderEntryRectangle, .rectangle);

    if (maybe_piece) |piece| {
        piece.entity_basis.basis = group.default_basis;
        piece.entity_basis.offset = Vec3.sub(&offset, &vec3(dim.x() * 0.5, dim.y() * 0.5, 0));
        piece.color = color;
        piece.dim = dim;
    }
}

pub inline fn pushRectOutline(
    group: *RenderGroup,
    offset: Vec3,
    dim: Vec2,
    color: Vec4,
) void {
    const thickness = 0.1;

    // NOTE: Top and bottom
    pushRect(group, Vec3.sub(&offset, &vec3(0, 0.5 * dim.y(), 0)), vec2(dim.x(), thickness), color);
    pushRect(group, Vec3.add(&offset, &vec3(0, 0.5 * dim.y(), 0)), vec2(dim.x(), thickness), color);

    // NOTE: Left and right
    pushRect(group, Vec3.sub(&offset, &vec3(0.5 * dim.x(), 0, 0)), vec2(thickness, dim.y()), color);
    pushRect(group, Vec3.add(&offset, &vec3(0.5 * dim.x(), 0, 0)), vec2(thickness, dim.y()), color);
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

inline fn unproject(
    group: *const RenderGroup,
    projected_xy: Vec2,
    at_distance_from_camera: f32,
) Vec2 {
    const world_xy = Vec2.scale(&projected_xy, at_distance_from_camera / group.game_camera.focal_length);

    return world_xy;
}

inline fn getCameraRectangleAtDistance(
    group: *RenderGroup,
    distance_from_camera: f32,
) Rectangle2 {
    const raw_xy = unproject(group, group.monitor_half_dim_in_meters, distance_from_camera);

    const result = Rectangle2.centerHalfDim(&vec2(0, 0), &raw_xy);

    return result;
}

pub inline fn getCameraRectangleAtTarget(group: *RenderGroup) Rectangle2 {
    const result = getCameraRectangleAtDistance(group, group.game_camera.distance_above_target);

    return result;
}

inline fn getRenderEntityBasisP(
    render_group: *RenderGroup,
    entity_basis: *align(@alignOf(void)) RenderEntityBasis,
    screen_dim: Vec2,
) EntityBasisPResult {
    var result: EntityBasisPResult = .{};

    const screen_center = Vec2.scale(&screen_dim, 0.5);
    const entity_base_p = entity_basis.basis.p;
    const entity_base_offset = entity_basis.offset;

    const distance_to_pz = render_group.render_camera.distance_above_target - entity_base_p.z();
    const near_clip_plane = 0.2;

    const raw_xy = Vec2.add(&entity_base_p.xy(), &entity_base_offset.xy());
    const raw_xyz = vec3(raw_xy.x(), raw_xy.y(), 1.0);

    if (distance_to_pz > near_clip_plane) {
        const projected_xy = Vec3.scale(&Vec3.scale(&raw_xyz, render_group.render_camera.focal_length), 1.0 / distance_to_pz);

        result = .{
            .p = Vec2.add(&screen_center, &Vec2.scale(&projected_xy.xy(), render_group.meters_to_pixels)),
            .scale = render_group.meters_to_pixels * projected_xy.z(),
            .valid = true,
        };
    }

    return result;
}

pub fn renderGroupToOutput(
    render_group: *RenderGroup,
    output_target: *const Bitmap,
) void {
    platform.beginTimedBlock(.render_group_to_output);
    defer platform.endTimedBlock(.render_group_to_output);

    const screen_dim = vec2(@as(f32, @floatFromInt(output_target.width)), @as(f32, @floatFromInt(output_target.height)));

    const pixels_to_meters = 1.0 / render_group.meters_to_pixels;

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
                const basis = getRenderEntityBasisP(render_group, &entry.entity_basis, screen_dim);

                if (true) {
                    if (entry.bitmap) |bitmap| {
                        // NOTE: With Casey's implementation, there will be one iteration of the game
                        // loop when a sword has transitioned from spatial to non_spatial during which
                        // a draw attempt will be made without this check for an invalid position in place.
                        // This makes it clear why avoiding use of a non spatial entity's position is
                        // important. An attempt to draw at that position in this case will cause an
                        // integer part of floating point value out of bounds panic.
                        if (!std.meta.eql(entry.entity_basis.basis.p.v, ety.invalidPos().v)) {
                            if (false) {
                                drawBitmap(output_target, bitmap, basis.p.x(), basis.p.y(), entry.color.v[3]);
                            } else {
                                drawRectangleHopefullyQuickly(
                                    output_target,
                                    basis.p,
                                    Vec2.scale(&vec2(entry.size.v[0], 0), basis.scale),
                                    Vec2.scale(&vec2(0, entry.size.v[1]), basis.scale),
                                    entry.color,
                                    bitmap,
                                    pixels_to_meters,
                                );
                            }
                        }
                    } else unreachable;
                }

                base += @sizeOf(@TypeOf(entry.*));
            },
            .rectangle => {
                const entry: *align(@alignOf(void)) RenderEntryRectangle = @ptrCast(data);
                const basis = getRenderEntityBasisP(render_group, &entry.entity_basis, screen_dim);
                const dim = entry.dim;

                if (true)
                    drawRectangle(
                        output_target,
                        basis.p,
                        Vec2.add(@alignCast(&basis.p), @alignCast(&Vec2.scale(&dim, basis.scale))),
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
                        pixels_to_meters,
                    );

                const color = vec4(1, 1, 0, 1);
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
    resolution_pixels_x: usize,
    resolution_pixels_y: usize,
) *RenderGroup {
    var result: *RenderGroup = game.pushStruct(arena, RenderGroup);

    result.push_buffer_base = game.pushSize(
        arena,
        @alignOf(u8),
        max_push_buffer_size,
    );

    result.default_basis = game.pushStruct(arena, RenderBasis);
    result.default_basis.p = Vec3.splat(0);
    result.max_push_buffer_size = max_push_buffer_size;
    result.push_buffer_size = 0;

    // NOTE: Horizontal measurement of monitor in meters
    const monitor_width = 0.635;
    // NOTE: Length in meters the person is sitting from their monitor
    result.game_camera.focal_length = 0.6;
    result.game_camera.distance_above_target = 9.0;
    result.render_camera = result.game_camera;
    //result.render_camera.distance_above_target = 50.0;

    result.global_alpha = 1.0;

    // TODO: Adjust based on buffer size
    result.meters_to_pixels = @as(f32, @floatFromInt(resolution_pixels_x)) * monitor_width;

    const pixels_to_meters = 1.0 / result.meters_to_pixels;
    result.monitor_half_dim_in_meters = vec2(
        0.5 * @as(f32, @floatFromInt(resolution_pixels_x)) * pixels_to_meters,
        0.5 * @as(f32, @floatFromInt(resolution_pixels_y)) * pixels_to_meters,
    );

    return result;
}

pub inline fn unpack4x8(to_unpack: u32) Vec4 {
    const result = vec4(
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
        const uvs_per_meter = 0.1;
        const c = (uvs_per_meter * distance_from_map_in_z) / sample_direction.y();

        // TODO: Make sure we know what direction z should go in y
        const offset = Vec2.scale(
            &vec2(sample_direction.x(), sample_direction.z()),
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

        // NOTE: Turn this on to see where in the map you're sampling
        if (false) {
            const texel_ptr = if (lod.pitch > 0)
                @as([*]u8, @ptrCast(lod.memory)) +
                    @as(usize, @intCast(iy * lod.pitch)) +
                    @as(usize, @intCast(ix)) * @sizeOf(u32)
            else
                @as([*]u8, @ptrCast(lod.memory)) -
                    @as(usize, @intCast(iy * -lod.pitch)) +
                    @as(usize, @intCast(ix)) * @sizeOf(u32);

            @as([*]u32, @alignCast(@ptrCast(texel_ptr)))[0] = 0xFFFFFFFF;
        }

        const sample = bilinearSample(lod, ix, iy);
        result = SRGBBilinearBlend(sample, fx, fy).xyz();
    }

    return result;
}

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
    pixels_to_meters: f32,
) void {
    platform.beginTimedBlock(.draw_rectangle_slowly);
    defer platform.endTimedBlock(.draw_rectangle_slowly);
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

    // TODO: This will need to be specified separately
    const origin_z = 0.0;
    const origin_y = Vec2.add(
        &Vec2.add(
            &origin,
            &Vec2.scale(&x_axis, 0.5),
        ),
        &Vec2.scale(&y_axis, 0.5),
    ).y();
    const fixed_cast_y = inv_height_max * origin_y;

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
            platform.beginTimedBlock(.test_pixel);
            defer platform.endTimedBlock(.test_pixel);

            // NOTE: Test and fill pixels
            if (true) {
                const pixel_p = Vec2.fromInt(x, y);
                const d = Vec2.sub(&pixel_p, &origin);

                // TODO: Perp inner
                // TODO: Simpler origin
                const edge_0: f32 = Vec2.inner(&d, &Vec2.negate(&Vec2.perp(&x_axis)));
                const edge_1: f32 = Vec2.inner(&Vec2.sub(&d, &x_axis), &Vec2.negate(&Vec2.perp(&y_axis)));
                const edge_2: f32 = Vec2.inner(&Vec2.sub(&Vec2.sub(&d, &x_axis), &y_axis), &Vec2.perp(&x_axis));
                const edge_3: f32 = Vec2.inner(&Vec2.sub(&d, &y_axis), &Vec2.perp(&y_axis));

                if (edge_0 < 0 and edge_1 < 0 and edge_2 < 0 and edge_3 < 0) {
                    platform.beginTimedBlock(.fill_pixel);
                    defer platform.endTimedBlock(.fill_pixel);

                    var screen_space_uv: Vec2 = undefined;
                    var z_diff: f32 = undefined;

                    if (true) {
                        screen_space_uv = vec2(
                            @as(f32, @floatFromInt(x)) * inv_width_max,
                            fixed_cast_y,
                        );

                        z_diff = pixels_to_meters * (@as(f32, @floatFromInt(y)) - origin_y);
                    } else {
                        screen_space_uv = vec2(
                            @as(f32, @floatFromInt(x)) * inv_width_max,
                            @as(f32, @floatFromInt(y)) * inv_height_max,
                        );

                        z_diff = 0;
                    }

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

                    // NOTE: Normal map compositing
                    if (false) {
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
                            var bounce_direction = Vec3.scale(&normal.xyz(), 2.0 * normal.z());
                            bounce_direction.v[2] -= 1.0;

                            // TODO: Eventually we need to support two mappings, one for
                            // top-down view (which we don't do now) and one for sideways, which
                            // is what's happening here
                            bounce_direction.v[2] = -bounce_direction.z();

                            var maybe_far_map: ?*EnvironmentMap = null;
                            const pz = origin_z + z_diff;
                            //var map_z: f32 = 2.0;
                            const t_env_map = bounce_direction.y();
                            var t_far_map: f32 = 0;

                            if (t_env_map < -0.5) {
                                // TODO: This path seems particularly broken
                                maybe_far_map = maybe_bottom;
                                t_far_map = -1.0 - 2.0 * t_env_map;
                            } else if (t_env_map > 0.5) {
                                maybe_far_map = maybe_top;
                                t_far_map = 2.0 * (t_env_map - 0.5);
                            }

                            // TODO: How do we sample from the middle map?
                            _ = maybe_middle;
                            var light_color = Vec3.splat(0);

                            t_far_map *= t_far_map;
                            t_far_map *= t_far_map;

                            if (maybe_far_map) |far_map| {
                                const distance_from_map_in_z = far_map.pz - pz;

                                var far_map_color = sampleEnvironmentMap(
                                    screen_space_uv,
                                    bounce_direction,
                                    normal.w(),
                                    maybe_far_map,
                                    distance_from_map_in_z,
                                );

                                light_color = Vec3.lerp(&light_color, t_far_map, &far_map_color);
                            }

                            // TODO: Actually do a lighting model computation here

                            texel = texel.setRGB(Vec3.add(
                                &texel.rgb(),
                                &Vec3.scale(&light_color, texel.a()),
                            ));

                            // NOTE: Draws the bounce direction
                            if (false) {
                                texel = texel.setRGB(Vec3.add(
                                    &Vec3.splat(0.5),
                                    &Vec3.scale(&bounce_direction, 0.5),
                                ));
                                texel = texel.setRGB(Vec3.scale(
                                    &texel.rgb(),
                                    texel.a(),
                                ));
                            }
                        }
                    }

                    texel = Vec4.hadamard(&texel, &color);
                    texel.v[0] = math.clamp01(texel.r());
                    texel.v[1] = math.clamp01(texel.g());
                    texel.v[2] = math.clamp01(texel.b());

                    var dest = vec4(
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

pub fn drawRectangleHopefullyQuickly(
    buffer: *const Bitmap,
    origin: Vec2,
    x_axis: Vec2,
    y_axis: Vec2,
    _color: Vec4,
    texture: *Bitmap,
    _: f32,
) void {
    platform.beginTimedBlock(.draw_rectangle_hopefully_quickly);
    defer platform.endTimedBlock(.draw_rectangle_hopefully_quickly);
    //@setFloatMode(.Optimized);

    // NOTE: Premultiply color up front
    const color = _color.premultipliedAlpha(_color.a());

    //const x_axis_len = x_axis.length();
    //const y_axis_len = y_axis.length();
    //const nx_axis = Vec2.scale(&x_axis, y_axis_len / x_axis_len);
    //const ny_axis = Vec2.scale(&y_axis, x_axis_len / y_axis_len);

    // NOTE: nz_scale could be a parameter if we want to have
    // control over the amount of scaling in the z direction
    // that the normals appear to have
    //const nz_scale = 0.5 * (x_axis_len + y_axis_len);

    const inv_x_axis_length_sq = 1 / Vec2.lengthSquared(&x_axis);
    const inv_y_axis_length_sq = 1 / Vec2.lengthSquared(&y_axis);

    const color32: u32 =
        (@as(u32, (@intFromFloat(@round(color.a() * 255.0)))) << 24) |
        (@as(u32, (@intFromFloat(@round(color.r() * 255.0)))) << 16) |
        (@as(u32, (@intFromFloat(@round(color.g() * 255.0)))) << 8) |
        (@as(u32, (@intFromFloat(@round(color.b() * 255.0)))) << 0);

    const width_max = buffer.width - 1;
    const height_max = buffer.height - 1;

    //const inv_width_max = 1.0 / @as(f32, @floatFromInt(width_max));
    //const inv_height_max = 1.0 / @as(f32, @floatFromInt(height_max));

    // TODO: This will need to be specified separately
    //const origin_z = 0.0;
    //const origin_y = Vec2.add(
    //    &Vec2.add(
    //        &origin,
    //        &Vec2.scale(&x_axis, 0.5),
    //    ),
    //    &Vec2.scale(&y_axis, 0.5),
    //).y();
    //const fixed_cast_y = inv_height_max * origin_y;

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

    const nx_axis = Vec2.scale(&x_axis, inv_x_axis_length_sq);
    const ny_axis = Vec2.scale(&y_axis, inv_y_axis_length_sq);

    const inv_255 = 1.0 / 255.0;
    const one_255 = 255.0;

    var row: [*]u8 = @as([*]u8, @alignCast(@ptrCast(buffer.memory))) +
        (@as(u32, @intCast(x_min)) *
        @as(u32, @intCast(platform.BITMAP_BYTES_PER_PIXEL))) +
        @as(u32, @bitCast(y_min *% buffer.pitch));

    for (@intCast(y_min)..@intCast(y_max)) |y| {
        var pixel: [*]u32 = @alignCast(@ptrCast(row));

        for (@intCast(x_min)..@intCast(x_max)) |x| {
            platform.beginTimedBlock(.test_pixel);
            defer platform.endTimedBlock(.test_pixel);

            // NOTE: Test and fill pixels
            if (true) {
                const pixel_p = Vec2.fromInt(x, y);
                const d = Vec2.sub(&pixel_p, &origin);

                const u = Vec2.inner(&d, &nx_axis);
                const v = Vec2.inner(&d, &ny_axis);

                if (u >= 0 and u <= 1 and v >= 0 and v <= 1) {
                    platform.beginTimedBlock(.fill_pixel);
                    defer platform.endTimedBlock(.fill_pixel);

                    // TODO: Formalize texture boundaries
                    const tx = u * @as(f32, @floatFromInt(texture.width - 2));
                    const ty = v * @as(f32, @floatFromInt(texture.height - 2));

                    const ix: i32 = @intFromFloat(tx);
                    const iy: i32 = @intFromFloat(ty);

                    const fx = tx - @as(f32, @floatFromInt(ix));
                    const fy = ty - @as(f32, @floatFromInt(iy));

                    assert(ix >= 0 and ix < texture.width);
                    assert(iy >= 0 and iy < texture.height);

                    const offset = iy * texture.pitch + ix * @sizeOf(u32);

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

                    const sample_a = @as(*align(@alignOf(u8)) u32, @ptrCast(texel_ptr)).*;
                    const sample_b = @as(*align(@alignOf(u8)) u32, @ptrCast(b_offset)).*;
                    const sample_c = @as(*align(@alignOf(u8)) u32, @ptrCast(c_offset)).*;
                    const sample_d = @as(*align(@alignOf(u8)) u32, @ptrCast(d_offset)).*;

                    // NOTE: Unpack texel samples
                    var texel_A_r: f32 = @floatFromInt((sample_a >> 16) & 0xFF);
                    var texel_A_g: f32 = @floatFromInt((sample_a >> 8) & 0xFF);
                    var texel_A_b: f32 = @floatFromInt((sample_a >> 0) & 0xFF);
                    var texel_A_a: f32 = @floatFromInt((sample_a >> 24) & 0xFF);

                    var texel_B_r: f32 = @floatFromInt((sample_b >> 16) & 0xFF);
                    var texel_B_g: f32 = @floatFromInt((sample_b >> 8) & 0xFF);
                    var texel_B_b: f32 = @floatFromInt((sample_b >> 0) & 0xFF);
                    var texel_B_a: f32 = @floatFromInt((sample_b >> 24) & 0xFF);

                    var texel_C_r: f32 = @floatFromInt((sample_c >> 16) & 0xFF);
                    var texel_C_g: f32 = @floatFromInt((sample_c >> 8) & 0xFF);
                    var texel_C_b: f32 = @floatFromInt((sample_c >> 0) & 0xFF);
                    var texel_C_a: f32 = @floatFromInt((sample_c >> 24) & 0xFF);

                    var texel_D_r: f32 = @floatFromInt((sample_d >> 16) & 0xFF);
                    var texel_D_g: f32 = @floatFromInt((sample_d >> 8) & 0xFF);
                    var texel_D_b: f32 = @floatFromInt((sample_d >> 0) & 0xFF);
                    var texel_D_a: f32 = @floatFromInt((sample_d >> 24) & 0xFF);

                    // NOTE: Convert texture from srgb to "linear" brightness space
                    texel_A_r = math.square(inv_255 * texel_A_r);
                    texel_A_g = math.square(inv_255 * texel_A_g);
                    texel_A_b = math.square(inv_255 * texel_A_b);
                    texel_A_a = inv_255 * texel_A_a;

                    texel_B_r = math.square(inv_255 * texel_B_r);
                    texel_B_g = math.square(inv_255 * texel_B_g);
                    texel_B_b = math.square(inv_255 * texel_B_b);
                    texel_B_a = inv_255 * texel_B_a;

                    texel_C_r = math.square(inv_255 * texel_C_r);
                    texel_C_g = math.square(inv_255 * texel_C_g);
                    texel_C_b = math.square(inv_255 * texel_C_b);
                    texel_C_a = inv_255 * texel_C_a;

                    texel_D_r = math.square(inv_255 * texel_D_r);
                    texel_D_g = math.square(inv_255 * texel_D_g);
                    texel_D_b = math.square(inv_255 * texel_D_b);
                    texel_D_a = inv_255 * texel_D_a;

                    // NOTE: Bilinear texture blend
                    const i_fx = 1.0 - fx;
                    const i_fy = 1.0 - fy;

                    const l0 = i_fy * i_fx;
                    const l1 = i_fy * fx;
                    const l2 = fy * i_fx;
                    const l3 = fy * fx;

                    var texel_r = l0 * texel_A_r + l1 * texel_B_r + l2 * texel_C_r + l3 * texel_D_r;
                    var texel_g = l0 * texel_A_g + l1 * texel_B_g + l2 * texel_C_g + l3 * texel_D_g;
                    var texel_b = l0 * texel_A_b + l1 * texel_B_b + l2 * texel_C_b + l3 * texel_D_b;
                    var texel_a = l0 * texel_A_a + l1 * texel_B_a + l2 * texel_C_a + l3 * texel_D_a;

                    // NOTE: Modulate by incoming color
                    texel_r = texel_r * color.r();
                    texel_g = texel_g * color.g();
                    texel_b = texel_b * color.b();
                    texel_a = texel_a * color.a();

                    // NOTE: Clamp colors to valid range
                    texel_r = math.clamp01(texel_r);
                    texel_g = math.clamp01(texel_g);
                    texel_b = math.clamp01(texel_b);

                    // NOTE: Load destination
                    var dest_r: f32 = @floatFromInt((pixel[0] >> 16) & 0xFF);
                    var dest_g: f32 = @floatFromInt((pixel[0] >> 8) & 0xFF);
                    var dest_b: f32 = @floatFromInt((pixel[0] >> 0) & 0xFF);
                    var dest_a: f32 = @floatFromInt((pixel[0] >> 24) & 0xFF);

                    // NOTE: Go from srgb to "linear" brightness space
                    dest_r = math.square(inv_255 * dest_r);
                    dest_g = math.square(inv_255 * dest_g);
                    dest_b = math.square(inv_255 * dest_b);
                    dest_a = inv_255 * dest_a;

                    // NOTE: Destination blend
                    const inv_texel_a = 1.0 - texel_a;
                    var blended_r = inv_texel_a * dest_r + texel_r;
                    var blended_g = inv_texel_a * dest_g + texel_g;
                    var blended_b = inv_texel_a * dest_b + texel_b;
                    var blended_a = inv_texel_a * dest_a + texel_a;

                    // NOTE: Go from "linear" brightness space to srgb
                    blended_r = one_255 * @sqrt(blended_r);
                    blended_g = one_255 * @sqrt(blended_g);
                    blended_b = one_255 * @sqrt(blended_b);
                    blended_a = one_255 * blended_a;

                    // NOTE: Repack
                    pixel[0] = (lossyCast(u32, blended_a + 0.5) << 24) |
                        (lossyCast(u32, blended_r + 0.5) << 16) |
                        (lossyCast(u32, blended_g + 0.5) << 8) |
                        (lossyCast(u32, blended_b + 0.5) << 0);
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
        vec2(min.x() - r, min.y() - r),
        vec2(max.x() + r, min.y() + r),
        color.toVec4(1),
    );

    drawRectangle(
        buffer,
        vec2(min.x() - r, max.y() - r),
        vec2(max.x() + r, max.y() + r),
        color.toVec4(1),
    );

    // NOTE: Left and right
    drawRectangle(
        buffer,
        vec2(min.x() - r, min.y() - r),
        vec2(min.x() + r, max.y() + r),
        color.toVec4(1),
    );

    drawRectangle(
        buffer,
        vec2(max.x() - r, min.y() - r),
        vec2(max.x() + r, max.y() + r),
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
            var texel = vec4(
                @as(f32, @floatFromInt((source[0] >> 16) & 0xFF)),
                @as(f32, @floatFromInt((source[0] >> 8) & 0xFF)),
                @as(f32, @floatFromInt((source[0] >> 0) & 0xFF)),
                @floatFromInt((source[0] >> 24) & 0xFF),
            );

            texel = SRGB255ToLinear1(texel);
            texel = Vec4.scale(&texel, c_alpha);

            var d = vec4(
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
            var d = vec4(
                @floatFromInt((dest[0] >> 16) & 0xFF),
                @floatFromInt((dest[0] >> 8) & 0xFF),
                @floatFromInt((dest[0] >> 0) & 0xFF),
                @floatFromInt((dest[0] >> 24) & 0xFF),
            );

            d = SRGB255ToLinear1(d);

            const avg = 1.0 / 3.0 * (d.r() + d.g() + d.b());
            const delta = vec3(d.r() - avg, d.g() - avg, d.b() - avg);

            var result = Vec3.add(
                &vec3(avg, avg, avg),
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
