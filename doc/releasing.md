# Releasing

A release is one GitHub release carrying the two library jars, one `flair` executable per
platform, the polyglot bootstrap script and the installer:

```
dev.propensive:flair-plugin:X.Y.Z
dev.propensive:flair-client:X.Y.Z
flair-{linux,macos}-{x64,arm64}, flair-windows-x64.exe, flair, install.sh
```

Publishing runs locally rather than in CI, through the shared scripts in
[propensive/.github](https://github.com/propensive/.github), pinned by commit in
`etc/github-ref` and run via `etc/shared`.

## Releasing

Bump `flairVersion` in `build.mill` to the version being released and commit it: the script
refuses to run when the pin disagrees with its argument, because the launcher resolves
`flair-client` at that version and Burdock externalizes a library only when the released jar's
bytes are the ones on the launcher's classpath. Then:

```sh
make release VERSION=X.Y.Z
```

This publishes in two ordered steps: first the two library jars, exactly as `publishLocal`
produces them, so that GitHub records the digests Burdock hashed at compile time; then — once
those digests are indexed — the launcher is assembled and repackaged against them, the script
verifies that both libraries externalized to this release's URLs, and it uploads one executable
per platform (cross-built from one machine with the pinned `xeq` builder), the `flair`
bootstrap script (ziggurat's `Xeq.dispatcher`), and the generated `install.sh` that
`https://flair.propensive.dev/` redirects to.

## Dependencies

Soundness and Pyrocosm arrive as per-component jars in `~/.ivy2/local`, installed from their
GitHub Releases: `make sync-releases` installs the pinned Pyrocosm release (the shared CI
workflow does the same through its `extra_releases` input), and `make sync-releases
VERSION=X.Y.Z` in a Soundness checkout installs a Soundness release. Bumping either pin means
bumping `soundnessVersion` or `pyrocosmVersion` in `build.mill` and, for Pyrocosm,
`extra_releases` in `.github/workflows/ci.yml`.

The compiler is the proscala fork at `settings.scalaVersion`, kept in step with the Soundness
build; the plugin links against its internals, so a compiler bump is where the plugin is most
likely to need attention (`dotty.tools.dotc.ast.untpd` tree shapes are the largest surface
area), and the test suite is the regression gate.

## Consumers

A build uses the plugin as `dev.propensive:flair-plugin:X.Y.Z` (from `~/.ivy2/local` or the
release) with the options `flair options` prints from the project's configuration; see the
README.
