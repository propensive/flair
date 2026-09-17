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

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.config.Settings
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.{Diagnostic, Reporter}
import dotty.tools.dotc.util.SourceFile

object Parsing:
  // A Reporter that discards everything; we don't surface parser diagnostics
  // from Consequent — the main compiler will report parse errors itself.
  private class SilentReporter extends Reporter:
    def doReport(dia: Diagnostic)(using Context): Unit = ()

  // The plain name of a `-language` choice. The compiler models choices as
  // `ChoiceWithHelp(name, description)` whose `toString` renders the help
  // text, so the name must be read off explicitly.
  def name(choice: Any): String = choice match
    case choice: Settings.Setting.ChoiceWithHelp[?] => String.valueOf(choice.name)
    case other                                      => String.valueOf(other)

  // The outcome of a standalone parse. `setupError` distinguishes the two ways
  // an empty tree can arise, which must not be conflated: a source that does
  // not parse is ordinary (the compiler reports it itself, and there is
  // nothing for the tree rules to say), whereas a parser that could not be
  // configured means every tree-based rule would pass vacuously. The second is
  // a misconfiguration and is reported.
  final case class Parsed
    ( tree: untpd.Tree, source: SourceFile, setupError: Option[String] = None ):
    def failed: Boolean = tree.isEmpty

  // Parse `text` into an untyped Scala 3 AST.
  //
  // The standalone parse must agree with the compiler's own parse of the same
  // file, so `features` must be the `-language` features the compilation is
  // running with — without `relaxedLambdaSyntax`, for example, a one-line
  // `f: x => body` colon lambda misparses as a type ascription and misleads
  // every tree rule. The plugin reads them from the live compiler context
  // rather than hardcoding a list, which both guarantees the match and keeps
  // the plugin portable across compilers that offer different features.
  def parse(file: String, text: String, features: List[String] = Nil): Parsed =
    val source = SourceFile.virtual(file, text)

    val ctx =
      try
        val base  = new ContextBase
        val fresh = base.initialCtx.fresh.setReporter(new SilentReporter).setSource(source)

        // `setSetting` accepts an unknown choice silently, so the features are
        // checked against what this compiler advertises first. Left unchecked,
        // a stale or misspelled feature name would produce a parse subtly
        // different from the compiler's own and weaken every tree rule
        // without any sign that it had happened.
        val legal =
          fresh.settings.language.choices.fold(Set.empty[String]): choices =>
            choices.map(name).to(Set)

        val unknown = features.filterNot(legal)

        if unknown.nonEmpty
        then Left(s"this compiler does not support ${unknown.mkString("`", "`, `", "`")}")
        else
          val choices = features.map(Settings.Setting.ChoiceWithHelp(_, ""))
          if features.nonEmpty then fresh.setSetting(fresh.settings.language, choices)
          Right(fresh)

      catch case _: Throwable =>
        Left(s"could not enable ${features.mkString("`", "`, `", "`")}")

    ctx match
      case Left(error) => Parsed(untpd.EmptyTree, source, Some(error))

      case Right(ctx) =>
        try Parsed(new Parsers.Parser(source)(using ctx).parse(), source)
        catch case _: Throwable => Parsed(untpd.EmptyTree, source)
