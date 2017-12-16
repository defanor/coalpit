# Coalpit

Coalpit is a library for
building
[command-line program interfaces](https://defanor.uberspace.net/notes/command-line-program-interface.html):
the goal is to get interfaces between programs quickly and easily,
while keeping them language-agnostic and more user- and shell
scripting-friendly than JSON and similar formats.

Given a type, it derives instances to print and parse it as
command-line arguments, as well as to compose usage instructions. The
resulting deserialization wouldn't be as nice as that of
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

And its usage string -- like this:

```
[--something STRING] [--foobar (foo INT STRING | bar)] (foo INT STRING | bar)
```

More verbose versions can be produced and parsed with
`alwaysUseSelName = True` and/or `omitNamedOptions = False`:

```sh
--foobar foo --arg1 1 --arg2 'a string' --foobar2 bar
nothing just foo 1 'a string' bar
--something nothing --foobar just foo --arg1 1 --arg2 'a string' --foobar2 bar
```

And here is output of the `help` function from the same file, with all
the (alwaysUseSelName, omitNamedOptions) combinations:

```
(True,True)
["--foo","[]","--bar","a string"]
--foo ([] | : INT ([] | :...)) [--bar STRING]
(True,True)
["--foo","[]"]
--foo ([] | : INT ([] | :...)) [--bar STRING]
(True,False)
["--foo","[]","--bar","just","a string"]
--foo ([] | : INT ([] | :...)) --bar (nothing | just STRING)
(True,False)
["--foo","[]","--bar","nothing"]
--foo ([] | : INT ([] | :...)) --bar (nothing | just STRING)
(False,True)
["[]","--bar","a string"]
([] | : INT ([] | :...)) [--bar STRING]
(False,True)
["[]"]
([] | : INT ([] | :...)) [--bar STRING]
(False,False)
["[]","just","a string"]
([] | : INT ([] | :...)) (nothing | just STRING)
(False,False)
["[]","nothing"]
([] | : INT ([] | :...)) (nothing | just STRING)
```
