const std = @import("std");

const BitScanResult = struct {
    found: bool = false,
    index: u32 = 0,
};

pub fn find_least_sig_set_bit(value: u32) BitScanResult {
    var result = BitScanResult{};

    result.index = asm ("bsf %[value], %[index]"
        : [index] "={rax}" (-> u32),
        : [value] "{rax}" (value),
    );

    result.found = true;

    return result;
}
