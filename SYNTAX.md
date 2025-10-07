# Language Syntax Features/Rules

- Implicit return for functions
- Kabab case
- All variables are immutable
- Functions/variables are public by default (use `priv` for private)
- No `for`, `while`, `if/else` statements
- Pattern matching
- Array first/last pattern matching (`[first, ..last]`)
- Discard block ?
- Multiline raw string (`[[`/`]]`)
- `assert` keyword
- `import` keyword for files, stdlib or urls (no package manager)
- Fancy case syntax (..)
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
(#
  Multiline comment
#)

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
type Person
  name: string
  age: int
end
obj: Person = {
  name => "John",
  age => 30
}
```
