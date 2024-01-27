# Coalpit

Coalpit is a library for building command-line interfaces: the goal is
to get interfaces quickly and easily, while keeping them
language-agnostic and more user- and shell scripting-friendly than
JSON and similar formats.

Given a type, it derives instances to print and parse it as
command-line arguments or DSVs, as well as to compose usage
instructions. The resulting deserialization wouldn't be as nice as
that of
e.g.
[optparse-generic](https://hackage.haskell.org/package/optparse-generic),
but the aim here is to handle more or less arbitrary types.


## Example

An example is available in `examples/Basic.hs`. Given the following
Haskell value:

```haskell
Input { something = Nothing
      , fooBar = Just (Foo (FooArgs { arg1 = 1
                                    , arg2 = "a string"}))
      , fooBar2 = Bar}
```

Its serialized version with the default options is:

```haskell
input nothing just foo fooargs 1 "a string" bar
```

And its usage string:

```
input [--something] (nothing | just STRING) [--foobar] (nothing | just (foo fooargs [--arg1] INT [--arg2] STRING | bar)) [--foobar2] (foo fooargs [--arg1] INT [--arg2] STRING | bar)
```

Other versions can be produced by varying selector name policy. Below
are triples of a policy, a corresponding example serialization, and an
example usage string (output of the `help` function from the example):

```
SNDisable
test : 1 : 2 : 3 [] just "a string"
test ([] | : INT ([] | :...)) (nothing | just STRING)
SNDisable
test : 1 : 2 : 3 [] nothing
test ([] | : INT ([] | :...)) (nothing | just STRING)
SNAvoid
test : 1 : 2 : 3 [] just "a string"
test [--foo] ([] | : INT ([] | :...)) [--bar] (nothing | just STRING)
SNAvoid
test : 1 : 2 : 3 [] nothing
test [--foo] ([] | : INT ([] | :...)) [--bar] (nothing | just STRING)
SNPrefer
test --foo : 1 : 2 : 3 [] --bar just "a string"
test [--foo] ([] | : INT ([] | :...)) [--bar] (nothing | just STRING)
SNPrefer
test --foo : 1 : 2 : 3 [] --bar nothing
test [--foo] ([] | : INT ([] | :...)) [--bar] (nothing | just STRING)
SNRequire
test --foo : 1 : 2 : 3 [] --bar just "a string"
test --foo ([] | : INT ([] | :...)) --bar (nothing | just STRING)
SNRequire
test --foo : 1 : 2 : 3 [] --bar nothing
test --foo ([] | : INT ([] | :...)) --bar (nothing | just STRING)
```
