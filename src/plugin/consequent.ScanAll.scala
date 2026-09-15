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

import java.nio.file.{Files, Path as JPath}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object ScanAll:
  def main(args: Array[String]): Unit =
    val libRoot     = JPath.of(args.head)
    val rest        = args.tail.to(List)

    // Anything of the form `key=value` configures the checker exactly as the
    // corresponding `-P:consequent:` option would, so a scan can be run with
    // the same settings as the project's build. A bare argument is the rule
    // filter or the mode.
    val (options, positional) = rest.partition(_.contains("="))
    val (config, optionErrors) = Config.parse(options)
    optionErrors.foreach { error => System.err.nn.println(s"[consequent] $error") }

    val ruleFilter  = positional.headOption
    val all         = mutable.ArrayBuffer[Violation]()

    val files = Files.walk(libRoot).nn.iterator.nn.asScala.filter: path =>
      val s = path.toString
      s.endsWith(".scala") && s.contains("/src/") && !s.contains("/src/test")

    . toList

    // Mechanical-fix mode for D1: emit, for every flagged site, the raw
    // byte region `[start, end)` and the one-line rendering the checker
    // measured — a driver substitutes exactly that string, so the joined
    // form is byte-for-byte what was width-checked.
    if ruleFilter == Some("--fix-D1") then
      files.foreach: path =>
        val text           = Files.readString(path).nn
        val parsed = Parsing.parse(path.toString, text, config.language.getOrElse(Nil))

        Necessity.extract(parsed.tree, parsed.source, text).foreach: site =>
          println(s"${path}\t${site.start}\t${site.end}\t${site.rendering}")

      return

    // Census mode: the same counts the plugin writes during a build, summed
    // over the corpus and printed as `count<TAB>indicator`, largest first. It
    // needs no build, so it is the way to check a build's table against the
    // sources it was made from.
    if ruleFilter == Some("--metrics") then
      val totals = mutable.LinkedHashMap[String, Int]()

      files.foreach: path =>
        val name   = path.toString
        val text   = Files.readString(path).nn
        val parsed = Parsing.parse(name, text, config.language.getOrElse(Nil))

        val ctx =
          Context
            ( name, Checker.expectedModule(name, config.moduleRoot), text, parsed.tree,
              parsed.source, Nil, Nil, Set.empty, config )

        Metrics.collect(ctx).foreach: (indicator, count) =>
          totals(indicator) = totals.getOrElse(indicator, 0) + count

      totals.toList.sortBy { total => (-total(1), total(0)) }.foreach: (indicator, count) =>
        println(s"$count\t$indicator")

      return

    files.foreach: path =>
      val s    = path.toString
      val text = Files.readString(path).nn
      Checker.check(s, Checker.expectedModule(s, config.moduleRoot), text, config = config)
      . foreach(all += _)

    val filtered = ruleFilter match
      case Some(r) => all.filter(_.rule == r).toList
      case None    => all.toList

    filtered.foreach: v =>
      val short = v.file.split("/lib/").nn.map(_.nn) match
        case parts if parts.length >= 2 => "lib/"+parts(1)
        case _                          => v.file

      println(s"${short}:${v.line}:${v.column}  [${v.rule}] ${v.message}")
