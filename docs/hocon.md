# HOCON

`cfg-hocon` loads configuration with [Lightbend's config](https://github.com/lightbend/config), the reference library
for HOCON: HOCON, JSON and `.properties`, and YAML through
[typesafe-config-yaml](https://github.com/h8io/typesafe-config-yaml), with everything HOCON does — includes,
substitutions, the environment. It is the loader for a project that already writes HOCON, or wants Typesafe Config's
behaviour exactly.

```scala
libraryDependencies += "io.h8" %% "cfg-hocon" % "@VERSION@"
```

```scala
import h8io.cfg.impl.hocon.HOCON

val cfg = HOCON(defaults, local) // a Node.IMap[Id.Root]
val standard = HOCON()           // application.conf, reference.conf and the system properties
```

## Loading

`HOCON(urls*)` parses the sources, merges them in order, the later winning, and resolves substitutions over the
result, as `ConfigFactory.load` does. A URL passed that cannot be read is an error, whatever its syntax; an `include`
inside one stays optional unless written `include required(…)`.

With no URLs it loads the standard application configuration instead: the system properties, `application.*` and
`reference.conf` from the class path.

An `include` of a YAML file works, and so does a YAML source passed directly: `typesafe-config-yaml` reads it into
Typesafe Config's model, every scalar a string.

`HOCON.isolated(url)` loads one file on its own: its substitutions resolved inside it and the files it includes, with
no system property, no environment variable and no `reference.conf` reaching it. It is what Psi's `#hocon` imports
([Directives](psi/directives.md#importing-yaml-and-hocon)).

## What is lost

- **Text.** Typesafe Config types HOCON and JSON values as it parses them, and a scalar is the value written back:
  `1e5` comes back as `100000.0`. YAML is not typed, and keeps its text.
- **Tags.** HOCON has none, and Typesafe Config's model has no place for YAML's: `tag` is always `None`.
- **Locations** are what Typesafe Config's origins say.

## Trusted input only

Typesafe Config parses and resolves by recursion with no limit: a source nested a few thousand levels deep, or a chain
of a few thousand substitutions, overflows the stack, and the `StackOverflowError` reaches the caller as it is. YAML
read through `cfg-hocon` is no exception. Load input that is not trusted with [`cfg-yaml`](yaml.md) or
[`cfg-psi`](psi/README.md), which limit how deep it goes.

Errors are Typesafe Config's: a `ConfigException`, thrown, the first one found.
