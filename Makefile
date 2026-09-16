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
# `make test-plain` is the fume-less fallback the shared CI workflow uses: `flair.runTests`
# (src/test/flair_test_main.scala) drives `Tests.invoke` in-process.
test:
	./mill flair.test.assembly
	fume run $(TESTS)

test-plain:
	./mill flair.test.assembly
	java -cp out/flair/test/assembly.dest/out.jar flair.runTests

# Install the pinned pyrocosm release into the local ivy repository, as CI does, so a local build
# resolves the released jars rather than whatever a pyrocosm checkout's `publishLocal` last
# installed: the pinned version, or `VERSION=X.Y.Z`. Soundness itself is left alone.
sync-releases:
	./etc/shared sync-releases.sh propensive/pyrocosm pyrocosmVersion $(VERSION)

dev:
	./mill -w flair.client.compile

.PHONY: xeq-fetch sync-releases assembly release publishLocal run test test-plain dev install
