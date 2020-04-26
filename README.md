# existentials

A type-level DSL for placing existential constraints on some arguments of a type constructor.

You might be familiar with the [some](https://hackage.haskell.org/package/some) package, which defines a `Some` datatype which allows you to express `exists a. Maybe a` as `Some Maybe`:

```haskell
data Some f where
    Some :: f a -> Some f
```

This package defines a similar `Some`, except our is a specialization of the `SomeT` transformer which allows you to existentially quantify over multiple type parameters, e.g. `exists a b. Either a b` as `SomeT Some Either`.

But wait, there's more! What if you only want to existentially quantify over the first of two type parameters? This is possible too, by combining `SomeT` with the `ThisT` transformer, e.g. `exists a. Either a Int` is `SomeT (This Int) Either`.
