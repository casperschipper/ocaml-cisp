```elm
bagToList : Bag c x -> List (DeadEnd c x) -> List (DeadEnd c x)
bagToList bag list =
  case bag of
    Empty ->
      list

    AddRight bag1 x ->
      bagToList bag1 (x :: list)

    Append bag1 bag2 ->
      bagToList bag1 (bagToList bag2 list)
```

Bag is a tree it is easy to add to the right or left to such a thing.

Parser `>>` is for ignoring the valu
Parser `<*>` is for keeping a value, building a value from a bunch of consequitive stuff.
Parser `<$>` is for transforming a value within a parser, so for example:
```
let foo : Parser String 
(toUpperCase <$> foo)
```
`>>=` or `andThen` 
allows you to parse something, then continue parsing, but you can:
- throw a custom error
- access the stuff that was parsed
- control how the parsing continues.



