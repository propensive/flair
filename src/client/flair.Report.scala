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
┃   Flair, version 0.2.1.                                                                          ┃
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

// Pyrocosm's `Language`, `Token`, `Status` and friends shadow Soundness exports of the same
// names, so the umbrella import leaves those out and the model's own names are used directly.
import soundness.{Language as _, *}
import dysasymptotics.linearSize

import pyrocosm.{Block, Inline, Language, TerminalRenderer, Tone, modelLines}

import textMetrics.uniformMetric
import tableStyles.thickTableStyle
import palettes.solarizedDarkGaugePalette

// What a check found, rendered once: a paragraph and a highlighted excerpt per violation, then a
// table of the rules that fired, through Pyrocosm's semantic model and its terminal renderer. In
// terse mode (for CI, or on request) one grep-able line per finding instead.
object Report:
  case class Finding(path: Text, line: Int, column: Int, rule: Text, message: Text, error: Boolean)

  // `path` relative to the project root, for the report; the full path is noise the reader
  // already knows.
  def relative(root: Text, path: Text): Text =
    if path.starts(t"$root/") then path.skip(root.length + 1) else path

  def rich(root: Text, findings: List[Finding], texts: Map[Text, Text], files: Int, width: Int)
     (using Stdio)
  :   Unit =
    val renderer = TerminalRenderer()

    val blocks: List[Block] =
      findings.map { (finding: Finding) => excerpt(root, finding, texts.at(finding.path).or(t"")) }
        + List(summary(findings, files))

    renderer.blocks(blocks, width).each { line => Out.println(line) }

  // The finding as a paragraph — where, which rule, what — and the line it is on, marked at
  // the column, with a line of context either side.
  def excerpt(root: Text, finding: Finding, text: Text): Block =
    val tone  = if finding.error then Tone.Failure else Tone.Warning
    val style = if finding.error then Block.Note.Style.Erroneous else Block.Note.Style.Caution

    val heading: Block =
      Block.Paragraph
        ( List
            ( Inline.Reference(t"${relative(root, finding.path)}:${finding.line}:${finding.column}"),
              Inline.Textual(t"  "),
              Inline.Toned(tone, Inline.text(t"[↯${finding.rule}]")),
              Inline.Textual(t" ${finding.message}") ) )

    val lines: List[Text] = text.cut(t"\n")
    val first = (finding.line - 2).max(0)
    val last  = finding.line.min(lines.size)
    val shown: List[Text] = List.from(lines.stdlib.slice(first, last))

    if shown.nil then heading else
      val focus  = finding.line - 1 - first
      val length = shown.stdlib.lift(focus).map(_.length).getOrElse(1)
      val start  = (finding.column - 1).max(0).min(length)
      val end    = (start + 1).min(length.max(1))
      val note   = Block.Note(focus, start, end, style)
      val code   = Block.Code(Language.Scala, shown.map(highlighted), List(note))

      Block.Group(List(heading, code))

  // A line of Scala, tokenised by harlequin and carried into the model by pyrocosm's compiler
  // bridge; a line that fails to tokenise is shown plain.
  private def highlighted(line: Text): Block.Line =
    safely(Scala.highlight(line).modelLines.prim).or(Block.Line(List(pyrocosm.Token.plain(line))))

  // Rule, count, severity.
  private def summary(findings: List[Finding], files: Int): Block =
    val rules: List[Text] = findings.map(_.rule).distinct

    val rows: List[Block.Row] =
      rules.map: (rule: Text) =>
        val hits  = findings.filter(_.rule == rule)
        val error = hits.exists(_.error)
        Block.Row
          ( List
              ( Block.Cell(List(Inline.Toned(if error then Tone.Failure else Tone.Warning, Inline.text(rule)))),
                Block.Cell(List(Inline.Figure(hits.size.toDouble, 0))),
                Block.Cell(Inline.text(if error then t"error" else t"warning")) ) )

    val caption =
      if findings.nil then t"No violations in $files files"
      else t"${findings.size} violations in $files files"

    if rows.nil then Block.Paragraph(List(Inline.Toned(Tone.Success, Inline.text(caption))))
    else
      Block.Table
        ( List
            ( Block.Column(Inline.text(t"Rule")),
              Block.Column(Inline.text(t"Count"), Block.Alignment.End, numeric = true),
              Block.Column(Inline.text(t"Severity")) ),
          rows,
          Inline.text(caption) )

  // The census as a table of indicator and total, with the tree hash it was measured on.
  def census(totals: List[(Text, Int)], files: Int, tree: Text, width: Int)(using Stdio): Unit =
    val renderer = TerminalRenderer()

    val rows: List[Block.Row] =
      totals.map: (indicator: Text, total: Int) =>
        Block.Row(List(Block.Cell(Inline.text(indicator)), Block.Cell(List(Inline.Figure(total.toDouble, 0)))))

    val table: Block =
      if rows.nil then Block.Paragraph(Inline.text(t"Nothing to count in $files files"))
      else
        Block.Table
          ( List
              ( Block.Column(Inline.text(t"Indicator")),
                Block.Column(Inline.text(t"Total"), Block.Alignment.End, numeric = true) ),
            rows,
            Inline.text(t"$files files; input tree $tree") )

    renderer.blocks(List(table), width).each { line => Out.println(line) }
