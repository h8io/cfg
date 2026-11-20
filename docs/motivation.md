# Why cfg

## The object model

It started with HOCON and YAML, and at first it was not the formats I disliked but the object models their libraries
give a program. So I wrote one of my own, in classes and traits: a sealed tree of maps, sequences, scalars and nulls in
which every node knows its path and its place in the source, and a key that is not there is a node too, rather than an
exception or a `null` (see [The tree](tree.md)). Then came the loaders that build it from
[HOCON](hocon.md) and from [YAML](yaml.md).

## Two loaders, each missing something

That did not satisfy me. HOCON has no tags, and YAML has no includes. A configuration split into files, some of which
say what kind of value they hold, could not be written in either.

So I wrote [typesafe-config-yaml](https://github.com/h8io/typesafe-config-yaml), which lets a HOCON `include` read
YAML. It brought the includes, but lost the tags: YAML read that way becomes Typesafe Config's model, which has no
place for them. The project may still be of use to someone, as it keeps [Lightbend's
config](https://github.com/lightbend/config), HOCON's reference library, and only teaches it YAML; `cfg-hocon` loads
YAML through it.

HOCON's parser had one more cost: it types values before anyone sees them, so `1e5` written in a file comes back as
`100000.0`, and a scalar is no longer the text that was written.

## One format for both

Then I tried HOCON's own path. HOCON made every JSON document a valid HOCON one; I set out to make a format in which
HOCON and YAML documents were both valid. It soon turned out that this would tie the format's hands: every spelling
either of them gives a meaning to was taken, whether or not that meaning was any good. `#` alone, a comment in both,
could not have started a [directive](psi/directives.md), which is how Psi includes a file or reads the environment.

So I dropped the compatibility and kept the ideas: merging, substitutions and includes from HOCON, tags from YAML,
indentation as Scala 3 has it, and from neither the typing of values. The result is [Psi](psi/README.md). As always:

[![14 competing standards: one universal standard that covers everyone's use cases, and now there are 15 competing
standards](https://imgs.xkcd.com/comics/standards.png)](https://xkcd.com/927/)

*[Standards](https://xkcd.com/927/), xkcd by Randall Munroe, [CC BY-NC 2.5](https://creativecommons.org/licenses/by-nc/2.5/).*

## What stays

The HOCON and YAML loaders stay, alongside Psi. All three give the same tree, so a program reads its configuration the
same way whichever format it chose, and can change its mind later.
