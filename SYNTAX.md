# Epos Language Syntax Features/Rules

- Implicit return for functions
- Kabab case
- All variables are immutable
- Functions/variables are public by default (use `priv` for private)
- No `for`, `while`, `if/else` statements
- String concatenation with `++`/`<>` ?? or ("#{...}" ?)
- List prepend (`..`)
- Pattern matching
- Array first/last pattern matching (`[first, ..last]`)
- Discard block ?
- Multiline raw string (`[[`/`]]`)
- `assert` keyword
- `import` keyword for files, stdlib or urls (no package manager)
- Allow multiple arguments for functions that are of the same type to share types (`fn add(a, b: int): int`)
- Named return values (`fn add(a: int, b: int): (sum: int, diff: int)`) ??
- All types be Capital letter? (`Bool`, `List(String)`, `Int`)
- Fancy case syntax (..)
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
# Instead of print use expr (for express)
expr(factorial(5))
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
arr: int[] = [1, 2, 3, 4, 5]
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
