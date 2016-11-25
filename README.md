# Coalpit

Coalpit is a library for building command-line program
interfaces. They are like command-line user interfaces, but for
programs.

Given a type, it derives instances to print and parse it as
command-line arguments.

The goal is to
faciliate
[the KISS principle](https://en.wikipedia.org/wiki/KISS_principle)
preservation for interfaces between system components in certain
(rather [unixy](https://en.wikipedia.org/wiki/Unix_philosophy))
architectures. Described in more detail in
the
[command-line program interface](https://defanor.uberspace.net/notes/command-line-program-interface.html) note.

Not production-ready yet, merely a prototype.


## Example

There is an example in `Example.hs`, but here are some bits:

```haskell
data Y = Foo Bool Int
       | Bar Int
       | Baz
data X = X String (Maybe Int) (Maybe [Int]) Y Y String
$(deriveArgs ''Y)
$(deriveArgs ''X)
```

`toArgs` serializes data into arguments, and `fromArgs` deserializes
it: `X "test" Nothing (Just [1,2,3]) (Foo True 1) Baz "end"` ↔ `x
"test" n j 1,2,3 foo t 1 baz "end"`.


## TODO

What it currently lacks, but what should be done, roughly in that
order:

* Proper parsing: use optparse-applicative, Parsec, or a custom
  parser, but something with error handling and more flexible.
* Named arguments (via records), not just positional ones.
* Optional arguments: once the named ones will be there, `Maybe a`
  could be handled nicer.
* Help messages.
* Documentation.
