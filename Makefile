# Build the invocation-point `launcher` module as a plain (clean, no shell-preamble) assembly JAR.
# `launcher` depends on flair-client (and, through it, flair-plugin) as PUBLISHED coordinates
# resolved from ~/.ivy2/local, so the libraries are published there FIRST — otherwise the launcher
# silently builds against whatever was last published (a release's jars, say, whose bytes then
# externalize to that release's download, and local changes never reach the executable).
# `clean flair.launcher` for the same reason as in `run`: the coordinate is fixed, so Mill's cached
# resolution would not notice the fresh publish.
assembly: publishLocal
	./mill clean flair.launcher
	./mill flair.launcher.assembly

# Publish flair to GitHub Releases: the two library jars first, then — once their digests are
# indexed — the repackaged `flair` executables, added to the same release. See release-launcher.sh
# in propensive/.github (run through etc/shared) for the two-step ordering and its verification.
release:
	./etc/shared release-launcher.sh flair "flair-plugin flair-client" $(VERSION)

# Publish the libraries to the local ~/.ivy2 (the launcher resolves them from there; burdock will
# NOT externalize a locally-published copy unless its bytes match a release asset). This is also
# how a Soundness checkout picks up a local build of the plugin.
publishLocal:
	./mill flair.plugin.publishLocal + flair.client.publishLocal

# Repackage the launcher assembly into a self-fetching launcher with Burdock. The
# `burdock.externalize` macro wrapping `flair.flair` (in src/launcher/flair_launcher.scala) has
# already embedded `META-INF/burdock.deps` at compile time; running the repackager rewrites the JAR
# in place so published dependencies become on-demand `Burdock-Require` URLs and unpublished ones
# are inlined from `~/.cache/burdock`.
#
# Four publication homes are consulted: Maven Central (hashes resolved via deps.dev) for the
# third-party dependencies, and — via the `--github` hints — the release assets of the flair,
# Pyrocosm, Soundness and proscala repositories, whose per-jar SHA-256 digests the repackager
# matches against the classpath. Set GITHUB_TOKEN to lift the API rate limit.
flair.jar: assembly
	cp out/flair/launcher/assembly.dest/out.jar flair.jar
	java -cp flair.jar soundness.repackage --github propensive/flair,propensive/pyrocosm,propensive/soundness,propensive/proscala

# Package the repackaged JAR as a native executable for this machine with the pinned `xeq` builder
# script (fetched into dist/xeq and verified against etc/xeq.tsv).
flair: flair.jar xeq-fetch
	dist/xeq build --jar flair.jar --out flair

# Fetch the pinned `xeq` builder script into dist/xeq.
xeq-fetch:
	./etc/shared xeq-fetch.sh

install: flair
	cp flair ${HOME}/.local/bin/

# Run flair locally WITHOUT a release: publish the libraries, assemble the launcher, and run it
# directly (no burdock repackage, so the local library jars are simply bundled). Arguments go in
# ARGS, e.g. `make run ARGS='metrics --dry-run'`.
run: assembly
	java -jar out/flair/launcher/assembly.dest/out.jar $(ARGS)

# Compile and run the test suite with fume, which discovers the suite from the assembly named in
# .pyrocosm/fume/config.tel (relative to this directory). Extra selection terms go in TESTS.
# CI runs the same command (the shared workflow installs the fume pinned in etc/tools).
# `make test-plain` is a fume-less fallback: `flair.runTests` (src/test/flair_test_main.scala)
# drives `Tests.invoke` in-process.
test:
	./mill flair.test.assembly
	fume run -c out/flair/test/assembly.dest/out.jar $(TESTS)

test-plain:
	./mill flair.test.assembly
	java -cp out/flair/test/assembly.dest/out.jar flair.runTests

# Install every library pinned in etc/refs — releases and snapshots alike, transitively —
# into the local ivy repository, as CI does, so the build resolves exactly the pinned jars rather
# than whatever a sibling checkout's `publishLocal` last installed under the same version. A
# snapshot not yet on GitHub is built from the sibling checkout named by the pin's commit.
sync-deps:
	./etc/shared sync-deps.sh

# Check every source against Consequent Style and the project's own rules with the RELEASED flair
# (pinned in etc/tools; `make tools` installs it — the tool rule: what a repository runs is a
# release, never the build under test), as configured in
# .pyrocosm/flair/config.tel. Findings are warnings and the count is not yet zero, so CI does
# not run this; PATHS restricts the check to files beneath them.
check:
	flair check $(PATHS)

# Install the commands pinned in etc/tools (fume) through their releases' installers.
tools:
	./etc/shared tools.sh

# Publish HEAD's libraries as a snapshot — a `snapshot-<hex>` pre-release named by the filtered
# tree of the commit, at version `<flairVersion>-<hex>` — for a dependent repository to pin in
# its etc/refs before the next release. `LOCAL=1` stages and installs without publishing.
# The last line printed is the pin. See snapshot.sh in propensive/.github.
snapshot:
	./etc/shared snapshot.sh flair "$$(sed -n 's/.*val flairVersion = "\(.*\)".*/\1/p' build.mill)"

# Delete snapshot pre-releases older than DAYS (default 60) days.
snapshot-prune:
	./etc/shared snapshot-prune.sh flair $(DAYS)

dev:
	./mill -w flair.client.compile

.PHONY: check xeq-fetch sync-deps tools snapshot snapshot-prune assembly release publishLocal run test test-plain dev install
