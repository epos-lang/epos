# TODO
- [x] Add kebab case support
- [x] Add multiple strings (`[[]]`)
- [x] Add lists (? what to do about `[[1, 2], [2]]` not turning into a mutliline string? -> `@[@[1, 2, 3], @[4, 5, 6]]`?)
- [x] Allow typing of variables
- [ ] Make sure you can make lists of lists
- [ ] Fix multiline comments
- [ ] Make `match` expressions passable as implicit returns
- [ ] Add objects (
```epos
struct Person
  name: string,
  age: int,
  value: string,
end
person: Person = {name => "John", age => 30, [value] => key}
```
)
- [ ] Add function type
- [ ] Add `_` as a discard variable name
- [ ] Array first/last pattern matching (`[first, ..last]` or `[first | last]` ??) (`@[first | last]`)
- [ ] Add piping operator (`|>`)
- [ ] Remove `if`/`while` statements
- [ ] Make variables immutable
- [x] Add static typing
