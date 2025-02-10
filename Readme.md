# mineconfig (WIP)

A CLI tool that manipulates Minecraft-related artifacts.

Target: Run those tedious tasks required for setting up Minecraft modding environment,
which are currently tightly coupled with Gradle in the existing toolchains.
Examples: mcpconfig, forge userdev
([NeoFormRuntime](https://github.com/neoforged/NeoFormRuntime) can do these
two tasks for Minecraft version ≥ 1.17), parchment, etc.

Non-target: Functions that are already available and easy to use
in existing independent CLI tools like AutoRenamingTool, tiny-remapper, etc.

## 0.1.0 roadmap

- [ ] Implement CLI
- [x] Download official game and libraries
- [ ] Run mcpconfig
- [ ] Run forge userdev
