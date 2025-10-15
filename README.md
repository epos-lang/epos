# Epos
_The programming language of epic poetry_

Epos is a functional statically-typed programming language that compiles to LLVM IR.

Epos draws inspiration from [Lua](https://www.lua.org/), [Nim](https://nim-lang.org/), [Elixir](https://elixir-lang.org/), and [Gleam](https://gleam.run/) (in no particular order).

Example code
```epos
(#
  Simple program in Epos
#)

fn each(lst: list(t), fun: fn(t), index: int = 0)
  lst.elem(index).fun()
  match index < len(lst) - 1 then
    true => each(lst, fun, index + 1)
  end
end

record Book
  title: string
  author: string
  date: string
end

books: list(Book) = {
  @{
    title => "Fear and Trembling",
    author => "Johannas de Silentio",
    date => "1843"
  }, @{
    title => "Either/Or",
    author => "Victor Eremita",
    date => "1843"
  }, @{
    title => "Concluding Unscientific Postscript",
    author => "Johannas Climacus",
    date => "1846"
  }
}

books.each(fn(book: Book) => print([[
Title: #{book.title}
Author: #{book.author}
Date: #{book.date}
]]))

(#
Running `epos -r simple-program.epos` returns this result:
Executable created: build/simple-program
Running build/simple-program...

Title: Fear and Trembling
Author: Johannas de Silentio
Date: 1843


Title: Either/Or
Author: Victor Eremita
Date: 1843


Title: Concluding Unscientific Postscript
Author: Johannas Climacus
Date: 1846

#)
```

## Getting Started
- Run `nix build github:epos-lang/epos`. The compiler will now located in `./result/bin/epos`
Create a file ending in `.epos` with `print([[Hello world!]])` and run `epos <filename>`

## Notes:
- This is in active development
