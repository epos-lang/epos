# lua_llvm
_A LLVM compiler for Lua written Go_

## Notes:
- This is WIP
- I'm not a fan of starting counting at 1 so this starts at 0
- Currently, it does not have for loops (only while)
- It has nested multi-line comments by default
- The `function` keyword has been shortened to `fn`
- The `local` keyword is not supported
- I added a `match` statement (example:
```lua
match num then
  1, 2, 3, 4, 5 -> print("1-5")
  6, 7, 8, 9, 10 -> print("6-10")
  default -> print("default")
end
```

## Setup
- Run `nix build`

## Command to run
- Run `./result/bin/lua_llvm ./examples/hello.lua`
- Run `./build/hello`
