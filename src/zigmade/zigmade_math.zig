// TODO: Should this be extern union instead?
//pub const vec2 = extern union {
//    array: [2]f32,
//    map: extern struct {
//        x: f32,
//        y: f32,
//    },
//};
pub const Vec2 = struct {
    x: f32 = 0.0,
    y: f32 = 0.0,
};

pub const Vec3 = struct {
    x: f32 = 0.0,
    y: f32 = 0.0,
    z: f32 = 0.0,
};

pub const Vec4 = struct {
    x: f32 = 0.0,
    y: f32 = 0.0,
    z: f32 = 0.0,
    w: f32 = 0.0,
};

// TODO: Consider using comptime to make dynamic Vec types similar
// to Mach's VecN
//pub fn VecN(comptime n: usize, comptime t: type) type {
//    return extern struct {
//        v: @Vector(n, t),
//    };
//}

pub inline fn scale(v: Vec2, f: f32) Vec2 {
    var result: Vec2 = undefined;

    result.x = v.x * f;
    result.y = v.y * f;

    return result;
}

pub inline fn negate(v: Vec2) Vec2 {
    var result: Vec2 = undefined;

    result.x = -v.x;
    result.y = -v.y;

    return result;
}

pub inline fn add(v1: Vec2, v2: Vec2) Vec2 {
    var result: Vec2 = undefined;

    result.x = v1.x + v2.x;
    result.y = v1.y + v2.y;

    return result;
}

pub inline fn sub(v1: Vec2, v2: Vec2) Vec2 {
    var result: Vec2 = undefined;

    result.x = v1.x - v2.x;
    result.y = v1.y - v2.y;

    return result;
}

pub inline fn square(f: f32) f32 {
    const result = f * f;

    return result;
}

pub inline fn inner(v1: Vec2, v2: Vec2) f32 {
    const result = v1.x * v2.x + v1.y * v2.y;

    return result;
}

pub inline fn lengthSquared(vec: Vec2) f32 {
    const result = inner(vec, vec);

    return result;
}

pub const Rectangle2 = struct {
    min: Vec2,
    max: Vec2,
};

pub inline fn getMinCorner(rect: Rectangle2) Vec2 {
    const result = rect.min;
    return result;
}

pub inline fn getMaxCorner(rect: Rectangle2) Vec2 {
    const result = rect.max;
    return result;
}

pub inline fn getCenter(rect: Rectangle2) Vec2 {
    const result = scale(add(rect.min, rect.max), 0.5);
    return result;
}

pub inline fn rectMinMax(min: Vec2, max: Vec2) Rectangle2 {
    var result: Rectangle2 = undefined;

    result.min = min;
    result.max = max;

    return result;
}

pub inline fn rectMinDim(min: Vec2, dim: Vec2) Rectangle2 {
    var result: Rectangle2 = undefined;

    result.min = min;
    result.max = add(min, dim);

    return result;
}

pub inline fn rectCenterDim(center: Vec2, dim: Vec2) Rectangle2 {
    const result = rectCenterHalfDim(center, scale(dim, 0.5));

    return result;
}

pub inline fn rectCenterHalfDim(center: Vec2, half_dim: Vec2) Rectangle2 {
    var result: Rectangle2 = undefined;

    result.min = sub(center, half_dim);
    result.max = add(center, half_dim);

    return result;
}

pub inline fn isInRectangle(rectangle: Rectangle2, vec: Vec2) bool {
    const result = (vec.x >= rectangle.min.x and
        vec.y >= rectangle.min.y and
        vec.x < rectangle.max.x and
        vec.y < rectangle.max.y);

    return result;
}
