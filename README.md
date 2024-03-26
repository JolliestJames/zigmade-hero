# Zigmade Hero

Inspired by [MzaxnaV](https://github.com/MzaxnaV)'s Handmade Hero project in Zig and Casey Muratori's [Handmade Hero](https://www.youtube.com/watch?v=Ee3EtYb8d1o) series.

# Setup

* Install Zig. This project runs with Zig 2024.3.0-mach.
I try to keep this project's version in sync with whatever the Mach project's currently [nominated Zig version](https://machengine.org/about/nominated-zig/) happens to be.
* Clone the repo
* `cd` into the repo: `cd handmade-zig`
* Clone the `zigwin32` repo into the `src/` directory.
This project is currently using [this](https://github.com/marlersoft/zigwin32/tree/aec51b5b77764bd474a3a8e074d011492eb7e5ed) commit.
* `zig build run`
