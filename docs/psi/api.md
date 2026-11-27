# Psi: loading

How a program loads [Psi](README.md), reads what it loaded or what went wrong, and builds a loader of its own. Adding
directives, markers and formatters is [Extending](extending.md).

```scala
libraryDependencies += "io.h8" %% "cfg-psi" % "@VERSION@"
```

## Loading

`Psi(urls*)` loads the sources at URLs and merges them in order, and `Psi.parse(name, text)` loads a source held as a
string, `name` naming it in locations. Either gives the tree, an `h8io.cfg.Node.IMap`, read [as any other
loader's](../tree.md), or `PsiErrors`, every error found: nothing is thrown for anything a source holds. A URL, given
or included, that takes more than 10 seconds to connect or 30 to send its next bytes is unreadable, an error; a load
reads at most 10 000 000 bytes, every read counted, for at most 60 seconds.

```scala mdoc:silent
import h8io.cfg.Node
import h8io.cfg.impl.psi.*

val loaded = Psi.parse("app.psi", "server { host: localhost, port: 8080 }")
```

### From the class path

A program that names no source loads its configuration as Typesafe Config's `ConfigFactory.load()` does, from the
class path, with Psi's extensions. `Psi.standard()` reads every `reference` on the class path, the defaults each
library ships, then over them `application`, the program's own; `Psi.withReferences(urls*)` reads the references, then
the sources at `urls` over them, for a program whose configuration is a file. `Psi()` with no URL reads nothing.

- `application` is looked for as `#include(resource: application)` does: `.Ψ`, `.ψ` and `.psi`, the first found
  read. With none, the references are loaded alone.
- `reference` is every copy on the class path, of every extension: each `reference.Ψ` over each `reference.ψ`, and
  those over each `reference.psi`, the copies of one name in the order of the class path.
- They are the sources of one load, merged in that order and resolved together: a reference refers to what the
  application sets, as the application to what a reference sets.
- No system property chooses the file, as `config.file` does in Typesafe Config: a program that takes one reads it
  itself, and passes the URL to `withReferences`.
- A reference is read by the loader that loads, with its directives and its limits.

### Locations

A node's `location` is a `PsiLocation`: a source, line and column, or for a value a substitution put somewhere, both
where it stands and where it came from.

```scala mdoc
for (root <- loaded)
  root("server") match {
    case server: Node.IMap[?] =>
      server("port") match {
        case port: Node.IScalar[?] => println(port.value + " at " + port.location.description)
        case other => println(other)
      }
    case other => println(other)
  }
```

## Errors

`PsiErrors.errors` holds the errors, each a case class of the sealed `PsiError` with a `location`, a `message` saying
what is wrong and naming what was found, and a `hint` saying what to do, when there is one. A program reads an error
by its type and fields; the texts are for people and may change in any release. `PsiErrors.message`, and its
`toString`, is the report, the errors grouped by source:

```scala mdoc
Psi.parse("app.psi", "a = 1").left.map(_.errors)
Psi.parse("app.psi", "a = 1").left.foreach(errors => println(errors.message))
```

### Checking a source

`Psi.check(name, text)` gives the errors in the syntax of a source held as a string, what lexing and parsing find in
it, in the order of their lines and columns, and nothing when there are none: a check of a source as it is being
written, for an editor or a linter. Nothing is read, no directive runs and no substitution is resolved, so it is cheap
and safe on any input, and the errors those would find, a missing include or a substitution that finds nothing, are
left to `parse`. A loader's own `check` knows its markers and formatters; `Psi`'s reports them as `UnknownMarker` and
`UnknownFormatter`, which a tool that does not know the loader may pass over.

```scala mdoc
Psi.check("app.psi", "a: 1\nb:c\nd: ${nowhere}")
```

## Building a loader

`Psi` is the loader with nothing added. A builder makes another:

| Method | What it does |
|---|---|
| `withSystemProperties(map)` | gives `#prop` the properties of `map` rather than `sys.props` |
| `withEnvironment(map)` | gives `#env` the variables of `map` rather than `sys.env` |
| `disableDirective(name)` | turns a directive off for good, as a loader for input it does not trust turns off `include` and `env` |
| `withDepthLimit(n)` | limits depth to `n` rather than 128: lower for a thread with a smaller stack than the JVM's default |
| `withReadLimit(bytes, time)` | limits reading the sources of a load, every read counted, rather than to 10 000 000 bytes and 60 seconds |
| `withSizeLimit(nodes, characters)` | limits what a load adds to its sources, the nodes grafts copy and directives' fragments bring and the characters concatenations build, rather than to 1 000 000 and 10 000 000: about 350 000 nodes fill a 64 MB heap |
| `withDirective(name, directive)` | adds a directive of its own, called `#name` ([Extending](extending.md#directives)) |
| `withMarker(name, marker)` | adds a marker of its own, written `name` ([Extending](extending.md#markers)) |
| `withFormatter(name, formatter)` | adds a formatter of its own, called `${path; name(…)}` ([Extending](extending.md#formatters)) |

`build` gives the `PsiLoader`, with `apply(urls*)`, `parse(name, text)` and `check(name, text)` as `Psi` has, or every error of its
configuration at once: a name registered twice, the built-in directives `include`, `prop` and `env`, the built-in
markers `<`, `>`, `width` and `+` and the built-in formatter `join` included,
a disabled name registered, a parameter declared twice, a marker named as no marker can be written, a depth limit
below 1, a size or read limit below 0.

```scala mdoc:silent
val untrusted = Psi.builder.disableDirective("include").disableDirective("env").withSystemProperties(Map.empty).build
```

```scala mdoc
untrusted.map(_.parse("input", "#include(file: \"/etc/passwd\")*").left.map(_.message))
```
