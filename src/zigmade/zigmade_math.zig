// TODO: Should this be extern union instead?
//pub const vec2 = extern union {
//    array: [2]f64,
//    map: extern struct {
//        x: f64,
//        y: f64,
//    },
//};
pub const Vec2 = struct {
    x: f64 = 0.0,
    y: f64 = 0.0,
};

pub inline fn scale(v: Vec2, f: f64) Vec2 {
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

pub inline fn square(f: f64) f64 {
    const result = f * f;

    return result;
}
