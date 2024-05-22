const std = @import("std");
const math = @import("zigmade_math.zig");
const RandomSeries = std.rand.DefaultPrng;

pub fn seed(n: u64) RandomSeries {
    const result = RandomSeries.init(n);

    return result;
}

pub inline fn nextInt(series: *RandomSeries, t: type) t {
    const result = series.random().int(t);

    return result;
}

pub inline fn choice(series: *RandomSeries, n: usize) usize {
    const result = nextInt(series, usize) % n;

    return result;
}

pub inline fn unilateral(series: *RandomSeries) f32 {
    const divisor: f32 = 1.0 / 8192.0;
    const result = divisor * @as(f32, @floatFromInt(series
        .random()
        .intRangeAtMost(u32, 0, 8192)));

    return result;
}

pub inline fn bilateral(series: *RandomSeries) f32 {
    const result = 2 * unilateral(series) - 1;

    return result;
}

pub inline fn f32Between(
    series: *RandomSeries,
    min: f32,
    max: f32,
) f32 {
    const result = math.lerp(min, unilateral(series), max);

    return result;
}

pub inline fn i32Between(
    series: *RandomSeries,
    min: i32,
    max: i32,
) i32 {
    const result = series.random().intRangeAtMost(i32, min, max);

    return result;
}
