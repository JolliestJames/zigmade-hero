const std = @import("std");

// Although this function looks imperative, note that its job is to
// declaratively construct a build graph that will be executed by an external
// runner.
pub fn build(b: *std.Build) void {
    // Standard target options allows the person running `zig build` to choose
    // what target to build for. Here we do not override the defaults, which
    // means any target is allowed, and the default is native. Other options
    // for restricting supported target set are available.
    const target = b.standardTargetOptions(.{});

    // Standard optimization options allow the person running `zig build` to select
    // between Debug, ReleaseSafe, ReleaseFast, and ReleaseSmall. Here we do not
    // set a preferred release mode, allowing the user to decide how to optimize.
    const optimize = b.standardOptimizeOption(.{});

    const debug = b.option(bool, "debug_wall_clock", "Debug platform layer concerns, such as wall clock time") orelse false;

    const options = b.addOptions();
    options.addOption(bool, "DEBUG_WALL_CLOCK", debug);

    const win32 = b.createModule(.{
        .source_file = .{ .path = "./src/zigwin32/win32.zig" },
    });

    const platform = b.createModule(.{
        .source_file = .{ .path = "./src/zigmade_platform.zig" },
    });

    const zigmade = b.addSharedLibrary(.{
        .name = "zigmade",
        .root_source_file = .{ .path = "./src/zigmade/zigmade.zig" },
        .version = .{ .major = 0, .minor = 1, .patch = 0 },
        .target = target,
        .optimize = optimize,
    });

    zigmade.addModule("zigmade_platform", platform);

    // This declares intent for the library to be installed into the standard
    // location when the user invokes the "install" step (the default step when
    // running `zig build`).
    //b.installArtifact(lib);

    const exe = b.addExecutable(.{
        .name = "win32_zigmade",
        .root_source_file = .{ .path = "src/win32_zigmade.zig" },
        .target = target,
        .optimize = optimize,
    });

    exe.addModule("win32", win32);
    exe.addModule("zigmade_platform", platform);
    exe.addOptions("options", options);

    // This declares intent for the executable to be installed into the
    // standard location when the user invokes the "install" step (the default
    // step when running `zig build`).
    const build_step = b.step("zigmade", "Build the zigmade lib");
    build_step.dependOn(&zigmade.step);

    const exe_install_step = b.addInstallArtifact(exe, .{
        .dest_dir = .{ .override = .{ .custom = "../build" } },
        .pdb_dir = .{ .override = .{ .custom = "../build" } },
    });

    const zigmade_install_step = b.addInstallArtifact(zigmade, .{
        .dest_dir = .{ .override = .{ .custom = "../build" } },
        .pdb_dir = .{ .override = .{ .custom = "../build" } },
    });

    b.getInstallStep().dependOn(&exe_install_step.step);
    b.getInstallStep().dependOn(&zigmade_install_step.step);

    // This *creates* a Run step in the build graph, to be executed when another
    // step is evaluated that depends on it. The next line below will establish
    // such a dependency.
    const run_cmd = b.addRunArtifact(exe);

    // By making the run step depend on the install step, it will be run from the
    // installation directory rather than directly from within the cache directory.
    // This is not necessary, however, if the application depends on other installed
    // files, this ensures they will be present and in the expected location.
    run_cmd.step.dependOn(b.getInstallStep());

    // This allows the user to pass arguments to the application in the build
    // command itself, like this: `zig build run -- arg1 arg2 etc`
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    // This creates a build step. It will be visible in the `zig build --help` menu,
    // and can be selected like this: `zig build run`
    // This will evaluate the `run` step rather than the default, which is "install".
    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    // Creates a step for unit testing. This only builds the test executable
    // but does not run it.
    const lib_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/root.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_lib_unit_tests = b.addRunArtifact(lib_unit_tests);

    const exe_unit_tests = b.addTest(.{
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const run_exe_unit_tests = b.addRunArtifact(exe_unit_tests);

    // Similar to creating the run step earlier, this exposes a `test` step to
    // the `zig build --help` menu, providing a way for the user to request
    // running the unit tests.
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_unit_tests.step);
    test_step.dependOn(&run_exe_unit_tests.step);
}
