                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                       C O N S E Q U E N T                                        ┃
┃                                                                                                  ┃
┃                           a syntax and formatting standard for Scala 3                           ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃                                                                                                  ┃
┃   © Copyright 2025-26 Jon Pretty, Propensive OÜ.                                                 ┃
┃                                                                                                  ┃
┃   The primary distribution site is:                                                              ┃
┃                                                                                                  ┃
┃       https://consequent.style/                                                                  ┃
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
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package consequent

import scala.collection.mutable

import dotty.tools.dotc.*, ast.tpd, core.Contexts.*, plugins.*, util.{SourceFile, SourcePosition}
import dotty.tools.dotc.util.Spans.Span

class ConsequentPhase(options: List[String]) extends PluginPhase:
  val phaseName: String                = "consequent"
  override val runsAfter: Set[String]  = Set("typer")
  override val runsBefore: Set[String] = Set("pickler")

  private val (config, optionErrors) = Config.parse(options)
  private val errors: Boolean   = config.errors

  // Is this rule one the project holds at error severity? A `strict` entry
  // names either a rule — `S1`, which also covers its sub-rules `S1.1` and
  // `S1.2` — or a principle letter, `S`, which covers every rule derived from
  // it. Neither form matches a longer identifier that merely begins with it,
  // so `S1` does not select `S12`.
  private def strict(rule: String): Boolean =
    config.strict.exists: prefix =>
      if prefix.forall(_.isLetter)
      then rule.startsWith(prefix) && rule.drop(prefix.length).headOption.exists(_.isDigit)
      else rule == prefix || rule.startsWith(prefix+".")

  private val seen: mutable.Set[String] = mutable.Set.empty
  private var reportedOptions: Boolean = false

  private val esc: Char = 27.toChar
  private val bel: Char = 7.toChar
  private val gray   = s"$esc[38;2;128;128;128m"
  private val orange = s"$esc[38;2;255;165;0m"
  private val cyan   = s"$esc[38;2;0;200;255m"
  private val reset  = s"$esc[0m"

  private def colourPrefix(rule: String, useColor: Boolean): String =
    if useColor then
      val hyperlink = false
      val rendered  = rule.replace(".", s"$gray.$cyan")
      val link      = if hyperlink then s"$esc]8;;https://consequent.style/$rule$bel" else ""
      val unlink    = if hyperlink then s"$esc]8;;$bel" else ""
      s"$link$gray[$orange↯$cyan$rendered$gray]$reset$unlink "
    else
      s"[↯$rule] "

  // The `-language` features this compilation is running with. The standalone
  // parser used for sibling files must agree with the compiler's own parse, so
  // they are read from the live context rather than assumed — a plugin built
  // against one compiler is then correct on any compiler that accepts it.
  private def languageFeatures(using context: Context): List[String] =
    config.language.getOrElse:
      try
        import dotty.tools.dotc.config.Settings.Setting.value
        value(context.settings.language)(using context).map(Parsing.name)
      catch case _: Throwable => Nil

  override def transformUnit(tree: tpd.Tree)(using context: Context): tpd.Tree =
    val source: SourceFile = context.compilationUnit.source
    val path: String       = source.file.path

    if !reportedOptions then
      reportedOptions = true
      optionErrors.foreach: error =>
        report.warning(s"[↯consequent] $error")

    if seen.add(path) then
      val text: String = String(source.content)
      val module       = Checker.expectedModule(path, config.moduleRoot)

      val useColor     =
        try
          import dotty.tools.dotc.config.Settings.Setting.value
          value(context.settings.color)(using context) != "never"
        catch case _: Throwable => false

      val unitTree          = context.compilationUnit.untpdTree
      val siblingTypes      = umbrellaSiblingModules(path)
      val siblingExtensions = umbrellaSiblingExtensions(path)
      val unexported        = umbrellaUnexported(path) ++ umbrellaSiblingSurfaceExports(path)

      val violations =
        Checker.check
          ( path, module, text, unitTree, source, siblingTypes, siblingExtensions, unexported,
            config )

      violations.foreach: violation =>
        val pos = position(source, violation.line, violation.column)
        val msg = colourPrefix(violation.rule, useColor)+violation.message

        // Rendering a diagnostic at `pos` exercises the compiler's own message
        // renderer, which can throw on a pathological position (e.g. one mapping
        // into an empty or truncated source). A house-style check must never abort
        // the build because of that: fall back to a position-less diagnostic.
        val fatal = errors || strict(violation.rule)

        try if fatal then report.error(msg, pos) else report.warning(msg, pos)
        catch case _: Throwable =>
          if fatal then report.error(msg) else report.warning(msg)
    super.transformUnit(tree)

  // An export surface: the file that re-exports one component's public modules
  // into the umbrella package. It is named `<umbrella>_<component>_<suffix>
  // .scala` and sits in the component's own source directory, which the
  // `moduleRoot` option identifies (`<moduleRoot>/<component>/…`).
  //
  // Returns the component name when `path` is such a file, and `None`
  // otherwise — which is every file, for a project that sets no `umbrella`.
  // The umbrella rules (L4, L5) are then no-ops.
  //
  // Compiler-plugin source roots (`/src/plugin/`) are excluded: their
  // extraction helpers are deliberately unexported plugin internals.
  private def surfaceComponent(path: String): Option[String] =
    config.umbrella.flatMap: umbrella =>
      if path.contains("/src/plugin/") then None else
        val parts = path.split(s"/${config.moduleRoot}/").nn
        if parts.length < 2 then None else
          val component = parts(1).nn.split("/").nn(0).nn
          val fileName  = java.io.File(path).getName.nn

          Option.when(fileName.startsWith(s"${umbrella}_${component}_"))(component)

  // The `.scala` files beside `path`, or `Nil` if the directory can't be read.
  private def siblingFiles(path: String): List[java.io.File] =
    val parent = java.io.File(path).getParentFile
    val files  = if parent == null then null else parent.listFiles

    if files == null then Nil
    else files.toList.map(_.nn).filter(_.getName.nn.endsWith(".scala"))

  // When `path` is an export surface, the names of the sibling public modules
  // (`<component>.<Name>.scala` files in the same directory, excluding any
  // `internal` module) that L4 requires to be re-exported. A module is
  // included only when its file declares `Name` publicly: modules with no
  // accessible top-level declaration (e.g. a `private[component] object`)
  // cannot be — and need not be — re-exported.
  private def umbrellaSiblingModules(path: String): List[String] =
    surfaceComponent(path).to(List).flatMap: component =>
      val prefix = s"$component."

      siblingFiles(path).flatMap: sibling =>
        val name = sibling.getName.nn
        Option.when(name.startsWith(prefix)):
          val mid = name.substring(prefix.length, name.length - ".scala".length).nn
          mid.takeWhile(_ != '.')

        . filter: module =>
            module.nonEmpty && !module.toLowerCase.nn.contains("internal")
              && declaresPublicly(sibling, module)

  // When `path` is an export surface, the names of the public top-level
  // extension methods that L5 requires to be re-exported. They live in the
  // sibling `<component>_<suffix>.scala` definition files (e.g.
  // `xylophone_core.scala`) in the same directory; their lowercase
  // `<component>_` prefix distinguishes them from the `<umbrella>_…` and
  // `<other>_<component>_…` export surfaces, which begin with a different
  // prefix. `internal` definition files are skipped.
  private def umbrellaSiblingExtensions(path: String)(using Context): List[String] =
    surfaceComponent(path).to(List).flatMap: component =>
      val prefix = s"${component}_"

      siblingFiles(path).filter: sibling =>
        val name = sibling.getName.nn
        name.startsWith(prefix) && !name.toLowerCase.nn.contains("internal")

      . flatMap(extensionMethods)

    . distinct

  // Parse `file` and return the leaf names of its public top-level extension
  // methods. Returns `Nil` if the file can't be read or parsed.
  private def extensionMethods(file: java.io.File)(using Context): List[String] =
    parseFile(file).map { parsed => Extensions.extract(parsed.tree, parsed.source) }.getOrElse(Nil)

  // When `path` is an export surface, the simple names of every sibling
  // definition annotated `@unexported` — both in the `<component>.<Name>
  // .scala` module files and the `<component>_<suffix>.scala` definition files
  // in the same directory. L4 and L5 treat these names as deliberately
  // excluded from the umbrella package.
  private def umbrellaUnexported(path: String)(using Context): Set[String] =
    surfaceComponent(path).to(Set).flatMap: component =>
      siblingFiles(path).filter: sibling =>
        val name = sibling.getName.nn
        name.startsWith(s"$component.") || name.startsWith(s"${component}_")

      . flatMap(unexportedNames).to(Set)

  // Parse `file` and return the simple names of definitions it annotates
  // `@unexported`. Returns the empty set if the file can't be read or parsed.
  private def unexportedNames(file: java.io.File)(using Context): Set[String] =
    parseFile(file).map { parsed => Annotations.unexported(parsed.tree) }.getOrElse(Set.empty)

  // When `path` is an export surface, the export names from the *other*
  // surfaces in the same directory. A component split across several surfaces
  // (e.g. a `core`/`parser` split) re-exports each module from exactly one of
  // them; a module exported by a sibling surface is therefore not missing from
  // this one.
  private def umbrellaSiblingSurfaceExports(path: String)(using Context): Set[String] =
    val fileName = java.io.File(path).getName.nn

    config.umbrella.to(Set).flatMap: umbrella =>
      surfaceComponent(path).to(Set).flatMap: component =>
        siblingFiles(path).filter: sibling =>
          val name = sibling.getName.nn
          name != fileName && name.startsWith(s"${umbrella}_${component}_")

        . flatMap(surfaceExportNames).to(Set)

  // Parse an export-surface `file` and return the simple names it re-exports.
  private def surfaceExportNames(file: java.io.File)(using Context): Set[String] =
    parseFile(file)
    . map { parsed => UmbrellaExports.extract(parsed.tree, parsed.source).names }
    . getOrElse(Set.empty)

  // Read and parse a sibling file, using the same language features the
  // compilation itself is running with. `None` if it can't be read.
  private def parseFile(file: java.io.File)(using Context): Option[Parsing.Parsed] =
    try
      val content = String(java.nio.file.Files.readAllBytes(file.toPath))
      Some(Parsing.parse(file.getPath.nn, content, languageFeatures))
    catch case _: Throwable => None

  // Modifiers that may precede a public top-level declaration; `private` and
  // `protected` are deliberately absent, so a `private[x] object Foo` line never
  // matches the public-declaration pattern below.
  private val PublicModifiers =
    "(?:final|sealed|abstract|case|open|transparent|inline|erased|lazy|override|implicit)"

  // Does `file` declare `module` with a publicly-accessible top-level definition?
  // A column-0 `object`/`class`/`trait`/`enum`/`type`/`val`/`def`/`given Name`
  // (optionally preceded by non-access modifiers) counts; a `private`/`protected`
  // declaration does not, because the line then begins with that access modifier.
  private def declaresPublicly(file: java.io.File, module: String): Boolean =
    val keywords = "opaque\\s+type|object|class|trait|enum|type|val|def|given"
    val pattern  = s"(?m)^(?:$PublicModifiers\\s+)*(?:$keywords)\\s+$module(?![A-Za-z0-9])"
    try
      val content = String(java.nio.file.Files.readAllBytes(file.toPath))
      pattern.r.findFirstIn(content).isDefined
    catch case _: Throwable => true

  private def position(source: SourceFile, line: Int, column: Int): SourcePosition =
    val lineStart =
      try source.lineToOffset((line - 1).max(0))
      catch case _: Throwable => 0

    val offset = (lineStart + (column - 1).max(0)).min(source.content.length)
    SourcePosition(source, Span(offset))
