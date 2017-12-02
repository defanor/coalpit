# Coalpit

Coalpit is a library for building command-line program
interfaces. They are like command-line user interfaces, but for
programs.

Given a type, it derives instances to print and parse it as
command-line arguments. The resulting serialization wouldn't be as
nice as that of
e.g.
[optparse-generic](https://hackage.haskell.org/package/optparse-generic),
but the aim here is to handle arbitrary types.

The goal is to
faciliate
[the KISS principle](https://en.wikipedia.org/wiki/KISS_principle)
preservation for interfaces between system components in certain
(rather [unixy](https://en.wikipedia.org/wiki/Unix_philosophy))
architectures. Described in more detail in
the
[command-line program interface](https://defanor.uberspace.net/notes/command-line-program-interface.html) note.

Far from production-ready yet, merely a prototype.
