# Epos
_The programming language of epic poetry_

Epos is a functional statically-typed programming language that compiles to LLVM IR.

Epos draws inspiration from [Lua](https://www.lua.org/), [Nim](https://nim-lang.org/), [Elixir](https://elixir-lang.org/), and [Gleam](https://gleam.run/) (in no particular order).

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

## Getting Started
- Run `nix build github:silent-brad/epos`. The compiler will now located in `./result/bin/epos`
Create a file ending in `.epos` with `print([[Hello world!]])` and run `epos <filename>`

## Notes:
- This is in active development
