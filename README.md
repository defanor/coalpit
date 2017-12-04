# Coalpit

Coalpit is a library for
building
[command-line program interfaces](https://defanor.uberspace.net/notes/command-line-program-interface.html):
the goal is to get interfaces between programs quickly and easily,
while keeping them language-agnostic and more user- and shell
scripting-friendly than JSON and similar formats.

Given a type, it derives instances to print and parse it as
command-line arguments. The resulting deserialization wouldn't be as
nice as that of
e.g.
[optparse-generic](https://hackage.haskell.org/package/optparse-generic),
but the aim here is to handle more or less arbitrary types.

Warning: it is possible to run into ambiguity by defining a recursive
structure with optional named elements while using default options.
Unit type can be used to avoid that, or `omitNamedOptions` can be
disabled.

Not production-ready yet, merely a prototype.

## Example

An example is available in `Example.hs`. Given the following Haskell
value:

```haskell
Input { something = Nothing
      , fooBar = Just (Foo (FooArgs { arg1 = 1
                                    , arg2 = "a string"}))
      , fooBar2 = Bar}
```

With the default options, its serialized version should look like
this:

```haskell
["--foobar","foo","1","a string","bar"]
```

What would look like this in a shell:

```sh
--foobar foo 1 'a string' bar
```

More verbose versions can be produced and parsed with
`alwaysUseSelName = True` and/or `omitNamedOptions = False`:

```sh
--foobar foo --arg1 1 --arg2 'a string' --foobar2 bar
nothing just foo 1 'a string' bar
--something nothing --foobar just foo --arg1 1 --arg2 'a string' --foobar2 bar
```
