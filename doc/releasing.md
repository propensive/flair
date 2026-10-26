# Releasing

A release is one GitHub release carrying the two library jars, one `flare` executable per
platform, the polyglot bootstrap script and the installer:

```
dev.propensive:flare-plugin:X.Y.Z
dev.propensive:flare-client:X.Y.Z
flare-{linux,macos}-{x64,arm64}, flare-windows-x64.exe, flare, install.sh
```

Publishing runs in GitHub Actions, through the shared scripts in
[propensive/.github](https://github.com/propensive/.github), pinned by commit in
`etc/github-ref` and run via `etc/shared`.

## Releasing

The tag is the only place the version is declared. The release builds the libraries at the
tag's version, and the launcher resolves `flare-client` at that same version, since Burdock
externalizes a library only when the released jar's bytes are the ones on the launcher's
classpath. Once CI is green on the commit to release:

```sh
git tag -s X.Y.Z && git push --tags
```

The tag is the trigger. It fires `.github/workflows/release.yml`, which calls the shared
`scala-release.yml` and runs `release.sh` — the same script every repository in the ecosystem
releases with. What flare needs beyond the common path is the five lines in `etc/release`.

Nothing is published until every gate has passed: the tag must be signed and verified by
GitHub; CI must *already* be green on that exact commit, so the release does not re-run the
suite; and `deps.py check` must find every pin, transitively,
a published release. If a later step fails, the release **and** the tag are deleted from origin,
so a retry is `git tag -d X.Y.Z && git tag -s X.Y.Z && git push --tags`.

Then it publishes in two ordered steps: first the two library jars, exactly as `publishLocal`
produces them, so that GitHub records the digests Burdock hashed at compile time; then — once
those digests are indexed — the launcher is assembled and repackaged against them, the script
verifies that both libraries externalized to this release's URLs, and it uploads one executable
per platform (cross-built from one machine with the pinned `xek` builder), the `flare`
bootstrap script, and the generated `install.sh` that `https://propensive.dev/flare` redirects
to. A draft is not used, and cannot be: a draft's asset URLs live under an `untagged-…` path
that changes on publication, which would bake dead URLs into the executables.

## The notes

The release notes are generated the same way for every repository, by `release_notes.py`. The
**Changes** section is built from the body of each pull request merged since the previous tag —
the summary paragraph and the user-facing notes that `pull_request_template.md` asks for — so
what goes into a PR body is what users read on the release. A hand-written overview can be
added as `doc/notes/<version>.md`; it is optional, and nothing gates on it.

## Dependencies

Soundness and Pyrocosm arrive as per-component jars in `~/.ivy2/local`, installed from their
GitHub Releases. Both are pinned in `etc/refs`, the one place a pin lives: the build reads
it, `make sync-deps` installs what it names (transitively), and the shared CI workflow does the
same. A pin is a release, `X.Y.Z`, or a snapshot, `X.Y.Z-<12 hex>`, of an unreleased upstream
build published there by `make snapshot`; a release refuses to run while any pin is a
snapshot. The flow is described in [propensive/.github](https://github.com/propensive/.github).

`make snapshot` here publishes flare's own libraries the same way, which is how Soundness pins
an unreleased `flare-plugin`.

The compiler is the proscala fork at `settings.scalaVersion`, kept in step with the Soundness
build; the plugin links against its internals, so a compiler bump is where the plugin is most
likely to need attention (`dotty.tools.dotc.ast.untpd` tree shapes are the largest surface
area), and the test suite is the regression gate.

## Consumers

A build uses the plugin as `dev.propensive:flare-plugin:X.Y.Z` (from `~/.ivy2/local` or the
release) with the options `flare options` prints from the project's configuration; see the
README.
