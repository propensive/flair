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
┃   Flair, version 0.1.0.                                                                          ┃
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

import soundness.*
import dysasymptotics.linearSize

import filesystemBackends.javaBaseFilesystem

// The per-project configuration: a `.pyrocosm/flair/config.tel` file in the invocation's working
// directory or the nearest ancestor holding one — resolved upwards exactly like `.git` (and exactly
// as flame resolves its own `.pyrocosm/flame/config.tel`), so `flair` can be launched from anywhere
// inside a project. `.pyrocosm` is the project's shared directory, one subdirectory per tool.
//
// The file defines the project's own rules and the profiles that check them:
//
//   tel 1.0
//
//   rule cast                       # a named detection, from predicates over the parse tree
//     invokes asInstanceOf
//     message  Casts bypass the type checker; prefer a pattern match
//     severity error
//
//   profile default                 # what `flair` checks; `flair <name>` when there are several
//     source src/**/*.scala         # globs relative to the project root (the `.pyrocosm` parent)
//     exclude src/gen/**
//     language experimental.modularity
//     notes flair                   # the git notes namespace `flair metrics` writes to
//     style consequent              # the formatting standard, with its refinements beneath
//       header etc/header.txt
//       columns 100
//       umbrella soundness
//       module-root lib
//       interpolator m
//       omit L4
//       strict F4
//       severity warn
//       gate Unsafe
//         prefix unsafe
//     enforce cast                  # a rule this profile checks, optionally at its own severity
//       severity warn
//
// The plugin has no TEL parser, so a profile is rendered as `-P:flair:` options (`flair options`)
// and read back through the plugin's own `Config.parse` — one parser, one meaning, for both the
// command and the plugin.
object Workspace:
  case class Predicate(kind: Text, target: Text)
  case class Rule(name: Text, predicates: List[Predicate], message: Optional[Text], severity: Optional[Text])
  case class Gate(token: Text, prefix: Optional[Text])

  case class Style
    ( name:          Text,
      header:        Optional[Text],
      columns:       Optional[Int],
      umbrella:      Optional[Text],
      moduleRoot:    Optional[Text],
      interpolators: List[Text],
      omitted:       List[Text],
      strict:        List[Text],
      severity:      Optional[Text],
      gates:         List[Gate] )

  case class Enforcement(rule: Text, severity: Optional[Text])

  case class Profile
    ( name:      Text,
      sources:   List[Text],
      excludes:  List[Text],
      languages: List[Text],
      notes:     Optional[Text],
      style:     Optional[Style],
      enforced:  List[Enforcement] )

  case class Config(file: Text, root: Text, rules: List[Rule], profiles: List[Profile]):
    def profile(name: Text): Optional[Profile] = profiles.filter(_.name == name).prim

    // A relative path in the file resolves against the project root.
    def absolute(entry: Text): Text = if entry.starts(t"/") then entry else t"$root/$entry"

    // The `-P:flair:` options equivalent to a profile, without the `-P:flair:` prefix, in the
    // plugin's own syntax: `;` separates a list's items (the compiler splits `-P` on commas
    // before the plugin sees them) and `:` separates a value's fields.
    def pluginOptions(profile: Profile): List[Text] =
      val style: Optional[Style] = profile.style

      def option(keyword: Text, value: Optional[Text]): List[Text] =
        value.lay(Nil: List[Text]) { (text: Text) => List(t"$keyword=$text") }

      def listed(keyword: Text, values: List[Text]): List[Text] =
        if values.nil then Nil else List(t"$keyword=${values.join(t";")}")

      val errors: List[Text] =
        if style.let(_.severity) == t"error" then List(t"errors") else Nil

      val header: List[Text] = option(t"header", style.let(_.header).let(absolute(_)))
      val columns: List[Text] = option(t"columns", style.let(_.columns).let(_.show))
      val umbrella: List[Text] = option(t"umbrella", style.let(_.umbrella))
      val moduleRoot: List[Text] = option(t"moduleRoot", style.let(_.moduleRoot))
      val language: List[Text] = listed(t"language", profile.languages)
      val interpolators: List[Text] = listed(t"interpolators", style.let(_.interpolators).or(Nil))
      val strict: List[Text] = listed(t"strict", style.let(_.strict).or(Nil))
      val omitted: List[Text] = listed(t"omit", style.let(_.omitted).or(Nil))

      val gates: List[Text] =
        style.let(_.gates).or(Nil).map: (gate: Gate) =>
          val token: Text = gate.token
          gate.prefix.lay(t"gate=$token") { (prefix: Text) => t"gate=$token:$prefix" }

      val definitions: List[Text] =
        rules.bind[List[Text], Text, List[Text]]: (rule: Rule) =>
          val name: Text = rule.name
          val predicates: List[Text] = rule.predicates.map { (p: Predicate) => t"rule=$name:${p.kind}:${p.target}" }
          val message: List[Text] = option(t"rule", rule.message.let { (m: Text) => t"$name:message:$m" })
          val severity: List[Text] = option(t"rule", rule.severity.let { (level: Text) => t"$name:severity:$level" })
          predicates + message + severity

      val enforcements: List[Text] =
        profile.enforced.map: (e: Enforcement) =>
          val rule: Text = e.rule
          e.severity.lay(t"enforce=$rule") { (level: Text) => t"enforce=$rule:$level" }

      errors + header + columns + umbrella + moduleRoot + language + interpolators + strict
        + omitted + gates + definitions + enforcements

    // The plugin's configuration for a profile, and the errors its options raise — read back
    // through the plugin's own parser, so the command and the plugin can never disagree.
    def pluginConfig(profile: Profile, census: Boolean = false): Plugin =
      val extra: List[Text] = if census then List(t"census") else Nil
      val options: List[Text] = pluginOptions(profile) + extra
      val parsed = flair.Config.parse(options.map(_.s).stdlib)
      val config: flair.Config = parsed(0)
      val messages: scala.List[Text] = parsed(1).map { (error: String) => Text(error) }
      Plugin(config, List.from(messages))

  // A profile as the plugin sees it: its configuration, and what the options failed to say.
  case class Plugin(config: flair.Config, errors: List[Text])

  // What loading the configuration found: no file at all, a file that could not be read or
  // did not say what it must, or a configuration.
  enum Outcome:
    case Missing
    case Invalid(file: Text, errors: List[Text])
    case Loaded(config: Config)

  // The nearest `.pyrocosm/flair/config.tel` at or above `directory`, or `Unset` if no ancestor
  // has one. The FILE is what is sought: a `.pyrocosm` holding only other tools' directories does
  // not end the search.
  def locate(directory: Text): Optional[Path on Linux] =
    safely:
      def recur(dir: Path on Linux): Optional[Path on Linux] =
        val candidate =
          dir / Name[Linux](t".pyrocosm") / Name[Linux](t"flair") / Name[Linux](t"config.tel")
        if candidate.existent() then candidate else dir.parent.let(recur(_))

      recur(directory.as[Path on Linux])

  // Two separately-scoped `safely` regions (the filesystem read, then the TEL parse), as flame's
  // reader does: one region's tactic would be captured by both the path reader and the TEL
  // aggregator, which separation checking rejects.
  private def parse(file: Path on Linux): Optional[Tel] =
    safely(file.read[Data]).let { data => safely(data.read[Tel]) }

  def load(directory: Text): Outcome =
    locate(directory).lay(Outcome.Missing): file =>
      parse(file).lay(Outcome.Invalid(file.encode, List(t"the file could not be read as TEL"))): tel =>
        val root: Text = file.parent.let(_.parent).let(_.parent).let(_.encode).or(directory)
        read(file.encode, root, tel)

  // The atoms of every child compound with this keyword, one per occurrence.
  private def atoms(node: Tel, keyword: Text): List[Text] =
    node.fields(keyword).to[List].map(_.primaryAtom).filter(_ != t"")

  // A compound's atoms joined by single spaces: a `message` or a `matches` pattern may hold
  // several words.
  private def phrase(node: Tel): Text = node.atomTexts.to[List].filter(_ != t"").join(t" ")

  private def int(node: Tel, keyword: Text): Optional[Int] =
    atoms(node, keyword).prim.let { (text: Text) => safely(text.as[Int]) }

  private def read(file: Text, root: Text, tel: Tel): Outcome =
    var errors: List[Text] = Nil
    def fail(message: Text): Unit = errors = errors + List(message)

    val rules: List[Rule] =
      tel.fields(t"rule").to[List].map: (node: Tel) =>
        val name = node.primaryAtom

        val kinds: List[Text] = List.from(Predicates.kinds.map(_.tt))

        node.childCompounds.to[List].map(_.keyword).each: (keyword: Text) =>
          if keyword != t"message" && keyword != t"severity" && !kinds.has(keyword) then
            fail(t"rule `$name`: `$keyword` is not a predicate; expected one of ${kinds.join(t", ")}")

        val predicates: List[Predicate] =
          kinds.bind[List[Predicate], Predicate, List[Predicate]]: kind =>
            node.fields(kind).to[List].map: (child: Tel) =>
              val predicate = Predicate(kind, phrase(child))
              Predicates.validate(flair.Predicate(kind.s, predicate.target.s)).foreach: (error: String) =>
                fail(t"rule `$name`: ${error.tt}")
              predicate

        if predicates.nil then fail(t"rule `$name` has no predicates")

        Rule
          ( name, predicates,
            node.field(t"message").let(phrase(_)),
            atoms(node, t"severity").prim )

    rules.map(_.name).each: (name: Text) =>
      if rules.filter(_.name == name).size > 1 then fail(t"rule `$name` is defined more than once")

    val profiles: List[Profile] =
      tel.fields(t"profile").to[List].map: (node: Tel) =>
        val name = node.primaryAtom
        if name == t"" then fail(t"a profile has no name")
        if reserved.has(name) then fail(t"`$name` is a subcommand, so it cannot name a profile")

        val sources = atoms(node, t"source")
        if sources.nil then fail(t"profile `$name` names no sources")

        val style: Optional[Style] =
          node.field(t"style").let: (styleNode: Tel) =>
            val styleName = styleNode.primaryAtom
            if styleName != t"consequent" then fail(t"profile `$name`: `$styleName` is not a style; only `consequent` is")

            Style
              ( styleName,
                header        = atoms(styleNode, t"header").prim,
                columns       = int(styleNode, t"columns"),
                umbrella      = atoms(styleNode, t"umbrella").prim,
                moduleRoot    = atoms(styleNode, t"module-root").prim,
                interpolators = atoms(styleNode, t"interpolator"),
                omitted       = atoms(styleNode, t"omit"),
                strict        = atoms(styleNode, t"strict"),
                severity      = atoms(styleNode, t"severity").prim,
                gates         =
                  styleNode.fields(t"gate").to[List].map: (gate: Tel) =>
                    Gate(gate.primaryAtom, gate.field(t"prefix").let(_.primaryAtom)) )

        val enforced: List[Enforcement] =
          node.fields(t"enforce").to[List].map: (e: Tel) =>
            val rule = e.primaryAtom
            if rules.filter(_.name == rule).nil then fail(t"profile `$name` enforces `$rule`, which is not defined")
            Enforcement(rule, atoms(e, t"severity").prim)

        Profile
          ( name, sources, atoms(node, t"exclude"), atoms(node, t"language"),
            atoms(node, t"notes").prim, style, enforced )

    profiles.map(_.name).each: (name: Text) =>
      if profiles.filter(_.name == name).size > 1 then fail(t"profile `$name` is defined more than once")

    if profiles.nil then fail(t"no profile is defined")

    if errors.nil then Outcome.Loaded(Config(file, root, rules, profiles))
    else Outcome.Invalid(file, errors)

  // Subcommand names, which a profile may not take: `flair <word>` must be unambiguous.
  val reserved: Set[Text] = Set(t"check", t"metrics", t"options", t"rules", t"install")
