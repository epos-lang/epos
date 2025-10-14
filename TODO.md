# TODO

- [x] Add kebab case support
- [x] Add multiple strings (`[[]]`)
- [x] Add lists (`{1, 2, 3}`)
- [x] Allow typing of variables
- [x] Make sure you can make lists of lists
- [x] Fix multiline comments
- [x] Add records
- [x] Add function type (example: `add_type: type = fn(int, int) -> int`)
- [x] Add `len` function to get length of lists (`len({1, 2, 3}) # => 2`)
- [x] Add list spread operator (`{1, ..{2, 3}}` or `{..{1, 2}, 3}`)
- [x] Add compare function (`compare("a", "a")`/`compare({1, 2}, {1, 2})`). Note: does not work on records
- [x] Make functions able to not have a return argument
- [x] Make `match` expressions passable as implicit returns
- [x] Add string interpolation (example: `"Hello #{name}!"` or `[[Hello #{name}!]]`)
- [x] Add `assert` keyword (example: `assert a + b == 3`)
- [x] Add generics
  - [x] Fix generics (for real this time)
- [x] Add `import` (examples: `import "./other-file.epos"`, `import {function, function2} from "./local-module"`)
  - [.] Fix import error where private functions have to be imported
- [x] Make `type` a keyword (example: `type new_type = int`)
- [x] Remove `if`/`while`/`return` statements
- [x] Make variables immutable
- [x] Add piping operator (`|>`)
- [x] Add float type
- [x] Add function param default value (`fn test-fn(a: int = 1)`)
- [x] Add generic record types (example: `record value(t)\n\t result: t\n end\n value-1: value(int) = @{result => 5}`)
- [?] Change multi-line comments to use `(#`/`#)`
- [!] Add builtin functions for file reading/writing
  - Example (write file): `file: file-type = open-file("text.txt")
    write-file(file, "Hello World!")
    close-file(file)`
  - Example (read file): `file: file-type = open-file("text.txt")
    text: string = read-file(file)
    close-file(file)`
- [!] Add builtin functions for getting command line arguments (example: `arg_list: list(string) = args()`)
- [x] Add ranges (example: `1..5 #=> {1, 2, 3, 4, 5}`)
- [.] Add `and`/`or` keywords
- [x] Add `error` builtin function
- [.] Add lambda expressions (example: `fn(x: int): int => x + 1`)
- [ ] Add `_` as a discard variable name (example: `_: int = 1 #=> _ is discarded` or `_list_of_nums: list(int) = {1, 2}`)
- [ ] Add Uniform function call syntax to replace pipes???
- [ ] Add union types
- [ ] Add `result`/`option`/`some`/`none`/`ok`/`err`/.. types
- [ ] Array first/last pattern matching (`{first, ..last}`
- [ ] Add function overloading (example: `fn add(x: int, y: int): int\n\t x + y\n end\n fn add(x: int, y: int, z: int): int\n\t x + y + z\n end\n add(1, 2)\n add(1, 2, 3)`)
