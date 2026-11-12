# Rhodium
A work in progress, functional programming language that is finally being implemented after a long while of thinking about implementation details. I've read many white papers and tried many programming languages to the point where I feel comfortable making my own. I have had previous attempts at this but these occoured in the form of practices in the field of programming language implementation.

Rhodium is a member of the platinum group metals which are unique for their rarity and thus exceeding price on the market. Whilst not a statement about the quality of the language or it's appeal, it is admittedly a cool enough name for it.

As far as this project goes my main concern when writing it is to adhere to the shape I have for it in my mind which is:
- Compiled
- Supports macros
- Functional
- FBIP memory management
- Algebraic datatypes
- concise syntax that makes sense to me
- Integrated tooling (i.e.: The compiler and language server share a codebase)

## Example
This is a minimal example of how the language is going to look like.

```rh
main = fn() never -> do
  @print("Hello, World!")
end
```

```rh
type Result[P, E] =
| Success(P)
| Failure(E)

type Maybe[P] =
| Something(P)
| Nothing
where {
  inner_or_default = fn[P](m Maybe[P], default P) P -> match m
  | Maybe::Something(inner) -> inner
  | _ -> default
}

type List[I] = {
  capacity u32
  length   u32
  head     *I
} where {
  head = fn[I](list *List[I]) Maybe[*I] -> match list.length
  | 0 -> Maybe::Nothing # Assuming prelude doesn't import all of Maybe's members
  | _ -> Maybe::Something(list.head)
}
```
