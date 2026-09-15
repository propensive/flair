# Consequent

**Consequent Style** is a syntax and formatting standard for Scala 3, and
**Consequent** is also the compiler plugin that enforces it.

Most formatting standards describe an output: run the formatter, accept what it
produces. Consequent Style instead states eight *principles* about how code is
read, and derives its rules from them. The rules are checked, not applied —
there is no reformatter — because several of them (where to break a long
signature, which of two shapes fits a definition) have more than one correct
answer, and the choice belongs to the author.

- [The standard](doc/consequent-style.md)
- [Rule reference](doc/rules/)
- [Releasing](doc/releasing.md)

## The eight principles

| | Principle | Concern |
| --- | --- | --- |
| **F** | The Frame | the fixed shape every file sits in |
| **A** | Anchoring | where a continuation line begins |
| **D** | Density | how much to fit on one line |
| **C** | Continuation Marking | showing that a line is unfinished |
| **B** | Balance | symmetry of spacing around a token |
| **P** | Proximity | blank lines and gaps as grouping |
| **T** | Tabulation | vertical alignment across a run of lines |
| **L** | Locatability | finding a definition from its name |

Every rule carries an identifier of the form `F1`, `A2.3` — the principle it
derives from, then its number within that principle. Each has a page under
[`doc/rules/`](doc/rules/).

## Using the plugin

Consequent is published for each supported Scala compiler version, because a
compiler plugin links against compiler internals:

```
dev.propensive:consequent_3.8.4
dev.propensive:consequent_3.9.0-RC4
```

The suffix is the *full* compiler version, not the binary version — the
`CrossVersion.full` convention every compiler plugin uses. Add it as a compiler
plugin; with Mill, the triple colon selects that suffix:

```scala
def scalacPluginMvnDeps = Seq(mvn"dev.propensive:::consequent:$version")
```

By default violations are reported as warnings; `-P:consequent:errors` makes
them errors, and `-P:consequent:strict=S1` (or `strict=S`, naming a whole
principle) makes just those errors, for a project adopting the standard a rule
at a time.

### Options

| Option | Default | Meaning |
| --- | --- | --- |
| `errors` | off | report violations as errors rather than warnings |
| `header=<n>` | `32` | number of lines in the licence header (`0` disables `F1`) |
| `columns=<n>` | `100` | maximum line length |
| `umbrella=<pkg>` | unset | umbrella re-export package; unset disables `L4` and `L5` |
| `moduleRoot=<seg>` | `lib` | path segment below which each module has its own directory |
| `language=<f,…>` | per compiler | `-language` features to enable when parsing |
| `interpolators=<i,…>` | `s,f,raw` | interpolators whose interior whitespace is significant |
| `unsafeToken=<T>` | unset | the project's unsafe token; unset disables `S1` |
| `strict=<r,…>` | none | rules or principles reported as errors regardless of `errors` |
| `metrics=<path>` | unset | write the per-file census to this table |
| `count=<n,…>` | none | extra identifiers the census counts by name |

### The census

`-P:consequent:metrics=<path>` writes a tab-separated table of `file`,
`indicator`, `count` covering the constructs a project wants to watch the size
of: `while`, `var`, `null`, `throw`, `catch-all`, every `unsafe`-prefixed name
under its own name, every definition gated by the unsafe token
(`unsafe-gate`), and any further identifiers named by `count`.

None of this is a violation, and nothing fails because of it. The question it
answers is how much code sits behind a project's escape hatches and whether
that is growing, which no per-file diagnostic can answer.

The write merges: only the records of the files just compiled are replaced, so
an incremental build leaves the rest of the table intact. After a clean build
the table covers the whole corpus. Counting is by name and tree shape on the
untyped tree, so it is a gauge and not a semantic census — a `get` is counted
wherever it appears, without asking what it selects from.

## Building

Requires JDK 21+. Everything else is fetched by the `./mill` bootstrap.

```
make build     # compile both cross-versions
make test      # run the test suite
```

## License

Consequent is made available under the [Apache 2.0 License](LICENSE).
