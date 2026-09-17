                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                            F L A I R                                             ┃
┃                                                                                                  ┃
┃                                       a linter for Scala 3                                       ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃   Flair, version 0.2.0.                                                                          ┃
┃   © Copyright 2025-26 Jon Pretty, Propensive OÜ.                                                 ┃
┃                                                                                                  ┃
┃   The primary distribution site is:                                                              ┃
┃                                                                                                  ┃
┃       https://propensive.dev/flair/                                                              ┃
┃                                                                                                  ┃
┃   Licensed under the Apache License, Version 2.0 (the "License"); you may not use this           ┃
┃   file except in compliance with the License. You may obtain a copy of the License at            ┃
┃                                                                                                  ┃
┃       https://www.apache.org/licenses/LICENSE-2.0                                                ┃
┃                                                                                                  ┃
┃   Unless required by applicable law or agreed to in writing, software distributed under          ┃
┃   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF            ┃
┃   ANY KIND, either express or implied. See the License for the specific language                 ┃
┃   governing permissions and limitations under the License.                                       ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package flair

import scala.caps
import soundness.*
import dysasymptotics.linearSize

import backstops.silentBackstop
import pyrocosm.{Block, Board, Inline, Status as Gauge}
import pyrocosm.Boards.show
import textMetrics.uniformMetric
import tableStyles.thickTableStyle
import palettes.solarizedDarkGaugePalette
import probates.cancelProbate
import charEncoders.utf8Encoder
import executives.completionsExecutive
import interpreters.posixInterpreter
import logging.silentLogging
import threading.platformThreading

// The exit statuses flair can terminate with, declared as objects (a `Status` must be an
// `object`, not a `val` — soundness#1811) so that the precise union of an `execute` block's
// result type documents them: `Status.Admissible` reifies the union, and the manpage's EXIT
// STATUS section is generated from it.
object Violations extends Status(1, t"one or more error-severity violations were reported")
object UsageError extends Status(2, t"the command line was not understood")
object NoConfig extends Status(3, t"no .pyrocosm/flair/config.tel was found")
object ConfigError extends Status(4, t"the configuration file could not be used")
object ParseFailure extends Status(5, t"a source file could not be parsed")
object NoRepository extends Status(6, t"the project is not in a git repository")
object NotesFailed extends Status(7, t"the git notes could not be written or read")
object InstallFailed extends Status(8, t"the tab-completions or manpage could not be installed")

// Flair's user interface, in one namespace: its subcommands and flags. The object exists so each
// can carry its natural name without a package-level `val` shadowing a Soundness export of the
// same name for the whole `flair` package.
object ui:
  val Check   = Subcommand("check", "check a profile's sources against its style and rules")
  val Metrics = Subcommand("metrics", "count a profile's rules and gates, and record the census in git notes")
  val Options = Subcommand("options", "print the -P:flair: options equivalent to a profile, for the compiler plugin")
  val Rules   = Subcommand("rules", "list the rules a profile checks")
  val Install = Subcommand("install", "install tab-completions and the manpage into the shell")

  val Terse  = Flag[Unit]("terse", false, List('t'), "one plain line per finding, for logs and CI")
  val Force  = Flag[Unit]("force", false, List('f'), "overwrite an existing note, or installed files")
  val DryRun = Flag[Unit]("dry-run", false, List('n'), "print the census without writing any note")
  val Show   = Flag[Text]("show", false, List('s'), "print the census recorded for a commit or input tree")

// Every command body takes `Stdio`, `Environment` and `WorkingDirectory` as pure `using`
// parameters; `execute` provides them through an `Invocation` whose derived capabilities are
// tracked, so each arm seals them once with `unsafeAssumePure` — they outlive the command they
// drive — before calling the body.
private inline def ambient[result]
   (inline body: (Stdio, Console, Environment, WorkingDirectory) ?=> result)
   (using invocation: Invocation)
:   result =
  given Stdio = caps.unsafe.unsafeAssumePure(invocation.stdio)
  given Console = caps.unsafe.unsafeAssumePure(summon[Console])
  given Environment = caps.unsafe.unsafeAssumePure(invocation.environment)
  given WorkingDirectory = caps.unsafe.unsafeAssumePure(invocation.workingDirectory)
  body

// Plain output is the default under CI: no terminal reads the colours, and a line per finding
// is what a log wants.
private def terseByDefault(using Environment): Boolean =
  safely(Environment.githubActions[Text]) == t"true"

private def width(using Environment): Int =
  safely(Environment.columns[Text].s.toInt).lay(120)(_.min(160))

// One grep-able line per finding.
private def terse(root: Text, findings: List[Report.Finding])(using Stdio): Unit =
  findings.each: (finding: Report.Finding) =>
    val level = if finding.error then t"error" else t"warning"
    val where = t"${Report.relative(root, finding.path)}:${finding.line}:${finding.column}"
    Out.println(t"$where: $level: [${finding.rule}] ${finding.message}")

// The words on the command line that are not flags or their operands.
private def words(arguments: List[Argument]): List[Text] =
  def recur(rest: List[Argument], acc: List[Text]): List[Text] = rest match
    case head :: tail =>
      val text = head()
      if text == t"--show" || text == t"-s" then recur(tail match { case _ :: rest => rest; case _ => tail }, acc)
      else if text.starts(t"-") then recur(tail, acc)
      else recur(tail, acc + List(text))
    case _ => acc

  recur(arguments, Nil)

// Which profile the words name, and which paths: the first word is a profile if the configuration
// defines one of that name; otherwise every word is a path. With one profile it needs no naming;
// with several, an unnamed profile is a usage error the command reports.
private def select(config: Workspace.Config, words: List[Text])
:   (Optional[Workspace.Profile], List[Text], Optional[Text]) =
  words match
    case head :: tail if config.profile(head).present => (config.profile(head), tail, Unset)
    case _ if config.profiles.size == 1               => (config.profiles.prim, words, Unset)
    case _ =>
      val names = config.profiles.map(_.name).join(t", ")
      (Unset, words, t"the configuration defines several profiles ($names); name one")

// The client's command dispatch. The `@main` entry point and burdock's `externalize` wrapper live
// alone in the `launcher` module (`src/launcher/flair_launcher.scala`), which depends on this
// module (and `plugin`) as PUBLISHED artifacts — so `externalize` records their jar hashes and the
// repackager turns them into on-demand `Burdock-Require` downloads instead of inlining them.
def runClient(): Unit =
  cli:
    val directory: Text = summon[Cli].workingDirectory.directory()
    val workspace: Workspace.Outcome = Workspace.load(directory)

    // Whether the client is a terminal, which is what decides whether a run shows its progress.
    val tty: Boolean = summon[DaemonService[?]].cliInput == ethereal.Stdin.Terminal

    arguments match
      case ui.Install() :: _ =>
        val force = ui.Force().present
        execute(install(force))

      case ui.Options() :: rest =>
        execute(ambient(options(workspace, words(rest))))

      case ui.Rules() :: rest =>
        execute(ambient(rules(workspace, words(rest))))

      case ui.Metrics() :: rest =>
        val force  = ui.Force().present
        val dryRun = ui.DryRun().present
        val show   = ui.Show().value
        execute(ambient(metrics(workspace, words(rest), force, dryRun, show, tty)))

      case ui.Check() :: rest =>
        val terse = ui.Terse().present
        execute(ambient(check(workspace, directory, words(rest), terse, tty)))

      // `flair`, `flair <profile>`, `flair <path>…` and `flair --terse`: a check.
      case rest =>
        val terse = ui.Terse().present
        execute(ambient(check(workspace, directory, words(rest), terse, tty)))

// The configuration, or the status to exit with when there is none to use.
private def loaded(workspace: Workspace.Outcome)(using Stdio)
:   Workspace.Config | NoConfig.type | ConfigError.type =
  workspace match
    case Workspace.Outcome.Loaded(config) => config

    case Workspace.Outcome.Missing =>
      Out.println(t"flair: no .pyrocosm/flair/config.tel was found here or above")
      NoConfig

    case Workspace.Outcome.Invalid(file, errors) =>
      Out.println(t"flair: $file could not be used:")
      errors.each { (error: Text) => Out.println(t"  $error") }
      ConfigError

private def check
   ( workspace: Workspace.Outcome, directory: Text, words: List[Text], terse: Boolean, tty: Boolean )
   (using Stdio, Console, Environment, WorkingDirectory, Monitor)
:   Exit | Violations.type | UsageError.type | NoConfig.type | ConfigError.type | ParseFailure.type =
  loaded(workspace) match
    case NoConfig => NoConfig
    case ConfigError => ConfigError
    case config: Workspace.Config => select(config, words) match
      case (_, _, message: Text) =>
        Out.println(t"flair: $message")
        UsageError

      case (profile: Workspace.Profile, paths, _) =>
        val plugin = config.pluginConfig(profile)
        val pluginConfig = plugin.config

        if !plugin.errors.nil then
          Out.println(t"flair: profile `${profile.name}` could not be used:")
          plugin.errors.each { (error: Text) => Out.println(t"  $error") }
          ConfigError
        else
          val restrict: List[Text] = paths.map { (path: Text) => Sources.resolve(directory, path) }
          val files = safely(Sources.expand(config.root, profile, restrict)).or(Nil)
          val enabled = Rules.enabled(pluginConfig)
          val features = profile.languages.map(_.s).stdlib
          val plain = terse || terseByDefault

          // The board: findings scroll into its content panel as each file is checked, under
          // a gauge of files parsed and then checked; the report follows once it has closed.
          val board = Board(t"flair: ${profile.name}")

          def work(): (Frontend.Result, List[Report.Finding]) =
            val parsed = java.util.concurrent.atomic.AtomicInteger(0)
            val checked = java.util.concurrent.atomic.AtomicInteger(0)

            val result =
              Frontend.run(files, profile.languages, Rules.lastPhase(enabled).tt): (phase: String) =>
                if phase == "parser" then
                  board.gauge(Gauge.Reckoning(parsed.incrementAndGet().toLong, files.size.toLong), t"Parsing")

            val texts: Map[Text, Text] = Map.from(result.units.map { (unit: Frontend.Parsed) => (unit.path, unit.text) }.stdlib)

            val findings: List[Report.Finding] =
              result.units.bind[List[Report.Finding], Report.Finding, List[Report.Finding]]: (unit: Frontend.Parsed) =>
                if board.aborted then Nil else
                  val report = Linter.check(pluginConfig, unit.path.s, unit.text.s, unit.tree, unit.source, features)

                  val found: List[Report.Finding] =
                    List.from:
                      report.violations.map: (violation: Violation, severity: Linter.Severity) =>
                        Report.Finding(violation.file.tt, violation.line, violation.column, violation.rule.tt,
                            violation.message.tt, severity == Linter.Severity.Error)

                  found.each { (finding: Report.Finding) => board.append(Report.excerpt(config.root, finding, unit.text)) }
                  board.gauge(Gauge.Reckoning(checked.incrementAndGet().toLong, result.units.size.toLong), t"Checking")
                  found

            (result, findings)

          // A terminal that cannot be initialised costs the board, not the check.
          val (result, findings) =
            if !(tty && !plain) then work() else
              recover:
                case Terminal.Error() => work()
              . protect(board.show()(work()))

          if board.aborted then Out.println(t"flair: the check was abandoned")

          val parseErrors = result.diagnostics.filter(_.error)

          val all: List[Report.Finding] =
            parseErrors.map { (d: Frontend.Diagnostic) => Report.Finding(d.path, d.line, d.column, t"parse", d.message, true) } + findings

          if plain then flair.terse(config.root, all)
          else
            val texts: Map[Text, Text] = Map.from(result.units.map { (unit: Frontend.Parsed) => (unit.path, unit.text) }.stdlib)
            Report.rich(config.root, all, texts, files.size, width)

          if !parseErrors.nil then ParseFailure
          else if findings.exists(_.error) then Violations
          else Exit.Ok

      case _ =>
        UsageError

private def options(workspace: Workspace.Outcome, words: List[Text])(using Stdio)
:   Exit | UsageError.type | NoConfig.type | ConfigError.type =
  loaded(workspace) match
    case NoConfig => NoConfig
    case ConfigError => ConfigError
    case config: Workspace.Config => select(config, words) match
      case (profile: Workspace.Profile, _, _) =>
        config.pluginOptions(profile).each { (option: Text) => Out.println(t"-P:flair:$option") }
        Exit.Ok

      case (_, _, message: Text) =>
        Out.println(t"flair: $message")
        UsageError

      case _ => UsageError

private def rules(workspace: Workspace.Outcome, words: List[Text])(using Stdio)
:   Exit | UsageError.type | NoConfig.type | ConfigError.type =
  loaded(workspace) match
    case NoConfig => NoConfig
    case ConfigError => ConfigError
    case config: Workspace.Config => select(config, words) match
      case (profile: Workspace.Profile, _, _) =>
        val pluginConfig = config.pluginConfig(profile).config

        List.from(Rules.style).each: (rule: flair.Rule) =>
          val state =
            if pluginConfig.omits(rule.id) then t"omitted"
            else if pluginConfig.strictly(rule.id) || pluginConfig.errors then t"error"
            else t"warn"
          Out.println(t"${rule.id.tt}\t${rule.principle.toString.tt}\t${rule.phase.tt}\t$state")

        config.rules.each: (rule: Workspace.Rule) =>
          val enforcement = profile.enforced.filter(_.rule == rule.name).prim
          val state = enforcement.lay(t"measured"): (e: Workspace.Enforcement) =>
            e.severity.or(rule.severity).or(t"warn")
          val predicates = rule.predicates.map { (p: Workspace.Predicate) => t"${p.kind} ${p.target}" }.join(t"; ")
          Out.println(t"${rule.name}\tProject\tparser\t$state\t$predicates")

        Exit.Ok

      case (_, _, message: Text) =>
        Out.println(t"flair: $message")
        UsageError

      case _ => UsageError

private def metrics
   ( workspace: Workspace.Outcome, words: List[Text], force: Boolean, dryRun: Boolean,
     show: Optional[Text], tty: Boolean )
   (using Stdio, Console, Environment, WorkingDirectory, Monitor)
:   Exit | UsageError.type | NoConfig.type | ConfigError.type | ParseFailure.type
      | NoRepository.type | NotesFailed.type =
  loaded(workspace) match
    case NoConfig => NoConfig
    case ConfigError => ConfigError
    case config: Workspace.Config => select(config, words) match
      case (_, _, message: Text) =>
        Out.println(t"flair: $message")
        UsageError

      case (profile: Workspace.Profile, _, _) =>
        val namespace = profile.notes.or(t"flair")

        Repository.locate(config.root) match
          case repository: Repository => show match
            case target: Text => showCensus(repository, namespace, profile.name, target)
            case _            => measure(config, profile, repository, namespace, force, dryRun, tty)

          case _ =>
            Out.println(t"flair: ${config.root} is not inside a git repository, so nothing can be recorded")
            NoRepository

      case _ => UsageError

private def showCensus(repository: Repository, namespace: Text, profile: Text, target: Text)
   (using Stdio, WorkingDirectory)
:   Exit | NotesFailed.type =
  repository.resolve(target) match
    case hash: Text =>
      // A commit's pointer note names the tree measured for this profile; a tree is its own key.
      val tree: Optional[Text] =
        if repository.objectType(hash) == t"commit" then
          repository.note(namespace, hash).let: (pointer: Text) =>
            safely(pointer.read[Tel]).let: (tel: Tel) =>
              tel.fields(t"measurement").to[List]
              . filter { (m: Tel) => m.field(t"profile").let(_.primaryAtom) == profile }
              . bind[List[Text], Text, List[Text]] { (m: Tel) => m.field(t"tree").lay(Nil: List[Text]) { (tree: Tel) => List(tree.primaryAtom) } }
              . prim
        else hash

      tree.let(repository.storedNote(namespace, _)) match
        case body: Text =>
          Out.print(body)
          Exit.Ok

        case _ =>
          Out.println(t"flair: no census is recorded for `$target` (profile `$profile`) under refs/notes/$namespace")
          NotesFailed

    case _ =>
      Out.println(t"flair: `$target` is not a commit or tree in this repository")
      NotesFailed

private def measure
   ( config: Workspace.Config, profile: Workspace.Profile, repository: Repository, namespace: Text,
     force: Boolean, dryRun: Boolean, tty: Boolean )
   (using Stdio, Console, Environment, WorkingDirectory, Monitor)
:   Exit | ConfigError.type | ParseFailure.type | NotesFailed.type =
  val plugin = config.pluginConfig(profile, census = true)
  val pluginConfig = plugin.config

  if !plugin.errors.nil then
    Out.println(t"flair: profile `${profile.name}` could not be used:")
    plugin.errors.each { (error: Text) => Out.println(t"  $error") }
    ConfigError
  else
    val files = safely(Sources.expand(config.root, profile, Nil)).or(Nil)
    val features = profile.languages.map(_.s).stdlib

    val board = Board(t"flair metrics: ${profile.name}")

    def work(): (Frontend.Result, List[(Text, List[(Text, Int)])]) =
      val parsed = java.util.concurrent.atomic.AtomicInteger(0)
      val counted = java.util.concurrent.atomic.AtomicInteger(0)

      val result =
        Frontend.run(files, profile.languages, t"parser"): (phase: String) =>
          if phase == "parser" then
            board.gauge(Gauge.Reckoning(parsed.incrementAndGet().toLong, files.size.toLong), t"Parsing")

      val perFile: List[(Text, List[(Text, Int)])] =
        result.units.map: (unit: Frontend.Parsed) =>
          val report = Linter.check(pluginConfig, unit.path.s, unit.text.s, unit.tree, unit.source, features)
          board.gauge(Gauge.Reckoning(counted.incrementAndGet().toLong, result.units.size.toLong), t"Counting")
          val relative = if unit.path.starts(t"${config.root}/") then unit.path.skip(config.root.length + 1) else unit.path
          val counts: List[(Text, Int)] =
            List.from(report.census.map { (indicator: String, count: Int) => (Text(indicator), count) })
          if !counts.nil then
            board.append(Block.Paragraph(Inline.text(t"$relative: ${counts.map { (i: Text, n: Int) => t"$i $n" }.join(t", ")}")))
          (relative, counts)

      (result, perFile)

    val (result, perFile) =
      if !(tty && !terseByDefault) then work() else
        recover:
          case Terminal.Error() => work()
        . protect(board.show()(work()))

    val totals = Census.totals(perFile)

    if result.diagnostics.exists(_.error) then
      result.diagnostics.filter(_.error).each: (d: Frontend.Diagnostic) =>
        Out.println(t"${d.path}:${d.line}:${d.column}: error: ${d.message}")
      Out.println(t"flair: a source did not parse, so nothing was recorded")
      ParseFailure
    else
      repository.inputTree(files) match
        case tree: Text =>
          Report.census(totals, files.size, tree, width)

          if dryRun then Exit.Ok else
            val head = repository.head()
            val measurement =
              Census.Measurement
                ( profile.name, tree, head.or(t""), repository.dirty(),
                  java.time.Instant.now().nn.toString.tt, files.size, totals,
                  perFile.filter(!_(1).nil) )

            val existing = repository.note(namespace, tree)

            // The tree note is written once per distinct input tree: the same inputs give the
            // same census, so a second run only adds a pointer from a new commit.
            val treeOutcome: Optional[Boolean] =
              if existing.present && !force then Unset
              else repository.addNote(namespace, tree, Census.body(measurement), force)

            val pointerOutcome: Optional[Boolean] = head match
              case commit: Text =>
                val already =
                  repository.note(namespace, commit).lay(false): (pointer: Text) =>
                    pointer.contains(t"tree $tree") && pointer.contains(t"profile ${profile.name}")

                if already then Unset
                else repository.appendNote(namespace, commit, Census.pointer(measurement))

              case _ =>
                Out.println(t"flair: the repository has no HEAD commit, so no pointer note was written")
                Unset

            if treeOutcome == false || pointerOutcome == false then
              Out.println(t"flair: the git notes could not be written")
              NotesFailed
            else
              val where = t"refs/notes/$namespace"
              val commit = head.lay(t"") { (c: Text) => t", commit $c" }

              (treeOutcome, pointerOutcome) match
                case (Unset, Unset) =>
                  Out.println(t"flair: already recorded under $where (tree $tree$commit); use --force to replace the census")
                case (Unset, _) =>
                  Out.println(t"flair: census for tree $tree already recorded under $where; added the pointer from commit ${head.or(t"")}")
                case _ =>
                  Out.println(t"flair: recorded under $where (tree $tree$commit)")

              Exit.Ok

        case _ =>
          Out.println(t"flair: the input tree could not be written; is `git` on the PATH?")
          NotesFailed

// Installs flair's shell tab-completions and its manpage, exactly as fume does. `Completions.ensure`
// writes the zsh/bash/fish completion script; it needs an `Entrypoint`, which the ambient
// Ethereal `DaemonService` supplies (it extends `Entrypoint`). The manpage's structure comes from
// `service.help()` — the same subcommand/flag tree the completions register — so `man flair` can
// never disagree with the CLI, and the EXIT STATUS section is populated from the `Status` unions
// of the `execute` blocks. `force = true` for the completions installs even when `flair` is not
// yet on the `PATH`, so a freshly-built binary can set itself up before being installed.
private def install(force: Boolean)
   (using invocation: Invocation, service: DaemonService[?])
   (using erased Effectful)
:   InstallFailed.type | Exit =

  import errorDiagnostics.stackTracesDiagnostics

  given Stdio = invocation.stdio

  // The `DaemonService` extends `Entrypoint`, and `Completions.ensure` accepts a TRACKED
  // `Entrypoint^`, so the service is passed on with its capture intact — no purity laundering.
  given entrypoint: (Entrypoint^{service}) = service

  given manual: Manual =
    Manual
      ( prose = t"Flair checks Scala 3 sources against Consequent Style and a project's own "
              + t"rules, defined in .pyrocosm/flair/config.tel, and records a census of those "
              + t"rules in git notes so that the counts can be followed commit by commit." )

  recover:
    // `exoskeleton.Install`, qualified: the bare name `Install` is this file's subcommand.
    case error: exoskeleton.Install.Error =>
      Out.println(t"Could not install the tab-completions or manpage")
      InstallFailed

  . protect:
      Completions.ensure(force = true).each(Out.println(_))

      Manpages.install(service.help().roff, force) match
        case Manpages.InstallResult.Installed(path) =>
          Out.println(t"Installed the manpage to $path")

        case Manpages.InstallResult.AlreadyInstalled(path) =>
          Out.println(t"A manpage is already installed at $path; use --force to overwrite it")

        case Manpages.InstallResult.NoWritableLocation =>
          Out.println(t"No writable location was found for the manpage")

      Exit.Ok
