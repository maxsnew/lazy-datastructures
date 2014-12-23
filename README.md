lazy-datastructures
-------------------

Data structures using laziness as provided by the [lazy](https://github.com/maxsnew/lazy) package.

Lazy Lists
==========
`Lazy.List.List` is either empty or a cons with a lazy tail.
`Lazy.List.LList` is a `Lazy (Lazy.List.List)`.

Since the tail of the list is lazily evaluated, you can make circular/infinite lists.

