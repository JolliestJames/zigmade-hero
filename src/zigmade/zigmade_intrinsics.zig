const std = @import("std");
const math = std.math;

const BitScanResult = struct {
    found: bool = false,
    index: u32 = 0,
};

pub fn findLeastSigSetBit(value: u32) BitScanResult {
    var result = BitScanResult{};

    result.index = asm ("bsf %[value], %[index]"
        : [index] "={rax}" (-> u32),
        : [value] "{rax}" (value),
    );

    result.found = true;

    return result;
}

pub fn signOf(value: i64) i64 {
    var result = std.math.sign(value);

    if (result == 0) result = 1;

    return result;
}
