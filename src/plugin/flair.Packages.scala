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

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile

case class PackageInfo
  ( line:                     Int,
    segments:                 List[String],
    extraStatementOnSameLine: Boolean )

object Packages:
  // The parser always wraps a file in a `PackageDef`; when the source has no
  // `package` declaration the wrapper is synthetic with an empty-name `pid`.
  // Distinguish "no real declaration" (return None) from a real one.
  def extract(tree: untpd.Tree, source: SourceFile): Option[PackageInfo] = tree match
    case pkg: untpd.PackageDef => describe(pkg, source)
    case _                     => None

  private def describe(pkg: untpd.PackageDef, source: SourceFile): Option[PackageInfo] =
    val segments = pidSegments(pkg.pid)
    val span     = pkg.span
    if segments.isEmpty || !span.exists then None
    else
      val line = source.offsetToLine(span.start) + 1
      val extra = pkg.stats.exists: stat =>
        val ss = stat.span
        ss.exists && source.offsetToLine(ss.start) + 1 == line
      Some(PackageInfo(line, segments, extra))

  // Render a package qualifier (`a.b.c`) as a list of segments. The empty
  // package's synthetic `Ident("<empty>")` produces an empty list.
  private def pidSegments(t: untpd.Tree): List[String] = t match
    case untpd.Ident(name) =>
      val n = name.toString
      if n == "<empty>" || n.isEmpty then Nil else List(n)

    case untpd.Select(qual, name) => pidSegments(qual) :+ name.toString
    case _                        => Nil
