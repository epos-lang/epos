# Epos
_The programming language of epic poetry_

Epos draws inspiration from [Lua](https://www.lua.org/), [Nim](https://nim-lang.org/), [Elixir](https://elixir-lang.org/), [Gleam](https://gleam.run/), [Go](https://golang.org/), and [Rust](https://www.rust-lang.org/).

Epos compiles to LLVM IR and is written in Go.

> NOTE: The Epos compiler requires Nix to build.

Example code
```epos
# Single line comment

#[
  #[
    Multi line comment (can be nested)
  ]#
]#

print("Hello, World!")

# Names can be use "_" and "-"

fn add(a: int, b: int): int
  a + b
end
print(add(1, 2))

fn factorial(n: int): int
  match n == 0
    true -> 1
    _ -> n * factorial(n - 1)
  end
end
print(factorial(5))

regular_string: string = "Hello, World!"
multi_line_string: string = [[
  Hello, World!
]]

boolean: bool = true

num: int = 42

array: list(int) = {1, 2, 3}

record Person
  name: string
  age: int
  favorite_color?: string
end

person: Person = @{
  name: "John Doe",
  age: 42
}

```

## Setup
- Run `nix build`

## Command to run
- Run `./result/bin/epos ./examples/hello.epos`
- Run `./build/hello`

## Notes/TODO:
- This is WIP
- Add static typing
- Eventually add macros
- Add stdlib
