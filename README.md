# Epos
_The programming language of epic poetry_

Epos is a functional statically-typed programming language that compiles to LLVM IR.

Epos draws inspiration from [Lua](https://www.lua.org/), [Nim](https://nim-lang.org/), [Elixir](https://elixir-lang.org/), and [Gleam](https://gleam.run/) (in no particular order).

Example code
```epos
# Single line comment

(#
  (#
    Multi line comment (can be nested)
  #)
#)

print("Hello, World!")

```

## Getting Started
- Run `nix build github:epos-lang/epos`. The compiler will now located in `./result/bin/epos`
Create a file ending in `.epos` with `print([[Hello world!]])` and run `epos <filename>`

## Notes:
- This is in active development
