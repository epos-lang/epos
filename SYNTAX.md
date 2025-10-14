# Epos Language Syntax Features/Rules

- Implicit return for functions
- Kabab case
- All variables are immutable
- Functions/variables are public by default (use `priv` for private)
- No `for`, `while`, `if/else` statements
- String concatenation with `++`/`<>` ?? or ("#{...}" ?)
- List prepend (`..`)
- Pattern matching
- Array first/last pattern matching (`{first, ..last}`)
- Lists (`seq`?) (`arr: list(int) = {1, 2, 3}`)
- Records
```epos
# Records in Epos
type person_type = @{
  name: string
  age: int
  favorite_color?: string # optional
}
person: person_type = @{
  name => "John",
  age => 30
}
```
- Discard block ?
- Multiline raw string (`[[`/`]]`)
- `assert` keyword
- `import` keyword for files, stdlib or urls (no package manager)
- Allow multiple arguments for functions that are of the same type to share types (`fn add(a, b: int): int`)
- Named return values (`fn add(a: int, b: int): (sum: int, diff: int)`) ??
- All types be Capital letter? (`Bool`, `List(String)`, `Int`)
- Define types with a symbol $Person @Person???
- Define public functions (or maybe private) with `*` placed in front of the function name (`fn *add(a, b: int): int`)
- Function types (`fn higher_order_fn(caller_fn: (a: int, b: int) -> int): int`)
- `string` type as `text` ?
- `expr` for express (like print)?
- Fancy case syntax (..)
- Discard patterns (`_ = 5` is unused, `_num = 5`)
- `todo` keyword
- Pipe Operator
- Uniform Function Call Syntax ?
- Static typing
- Macros
- Concurrency ?
- Use Dr. Yarvin's Kelvin Versioning

### Example code
```epos
# Single line comment
#[
  #[
    Multiline comment
  ]#
]#

fn factorial(n: int): int
  match n == 0 then
    true -> 1
    false -> n * fact(n - 1)
  end
end
print(factorial(5))

fn fib(n: int): int
  match n == 0 then
    true -> 0
    false -> match n == 1 then
      true -> 1
      false -> fib(n - 1) + fib(n - 2)
    end
  end
end
print(fib(10))

# Arrays
arr: list(int) = {1, 2, 3, 4, 5}
# Objects
type Person_Type
  name: string
  age: int
end
person: Person_Type = {
  name => "John",
  age => 30
}
```
