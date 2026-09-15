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

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameOps.isConstructorName

// A census of the constructs a project wants to watch the size of, counted
// per file so the totals can be tracked over time.
//
// This is deliberately not a rule: none of what it counts is a violation.
// A project that concentrates its unsafety behind a small set of named
// constructs still needs to know how much is behind them, and whether that
// is growing — a question no per-file diagnostic can answer, because the
// answer is a number over the whole corpus.
//
// Counting is by name and by tree shape, on the same untyped tree every rule
// sees. That is the same precision class as the rest of the checker: a
// `Select` named `get` is counted wherever it appears, without asking what it
// selects from. The counts are therefore a gauge rather than a census of
// semantics — good enough to see a trend, and not to be read as anything
// finer.
object Metrics:
  // Structural indicators, counted whenever metrics are collected at all.
  // Each name is either a Scala keyword or hyphenated, so none can collide
  // with an identifier counted by the `count` option.
  val While:    String = "while"
  val Var:      String = "var"
  val Null:     String = "null"
  val Throw:    String = "throw"
  val CatchAll: String = "catch-all"
  val Gate:     String = "unsafe-gate"
  val Ungated:  String = "unsafe-ungated"

  // Names that are unsafe by construction: an `unsafe`-prefixed reference is
  // counted under its own name, so the report distinguishes `unsafeAssumePure`
  // from `unsafely` without either having to be configured.
  private val Prefixed = """unsafe[A-Za-z0-9_]*""".r

  // The types a `catch` clause names when it catches everything.
  private val CatchAllTypes = Set("Throwable", "Exception")

  // Count every watched construct in one file. The result is sorted by
  // indicator, and omits indicators that did not occur.
  def collect(ctx: Context): List[(String, Int)] =
    val counts = mutable.LinkedHashMap[String, Int]()
    def bump(key: String): Unit = counts(key) = counts.getOrElse(key, 0) + 1

    val watched = ctx.config.count
    val token   = ctx.config.unsafeToken.map(_.split("\\.").nn.last.nn)

    walk(ctx.tree): tree =>
      tree match
        case _: untpd.WhileDo                     => bump(While)
        case _: untpd.Throw                       => bump(Throw)
        case untpd.Literal(Constant(null))        => bump(Null)
        case defn: untpd.ValDef if defn.mods.is(Flags.Mutable) => bump(Var)

        case defn: untpd.DefDef if token.exists(gates(defn, _)) => bump(Gate)

        // A definition that claims unsafety without taking the token: what S1.2
        // reports. It is counted as well as reported because a warning can be
        // hidden -- dotty keeps only the first diagnostic at a position, so a
        // file that already warns about something enclosing loses it -- and an
        // exemption a project has decided to live with should still be visible
        // in the number.
        case defn: untpd.DefDef
        if token.isDefined && Prefixed.matches(defn.name.toString) && !defn.name.isConstructorName
            && !defn.mods.is(Flags.Given) && !token.exists(gates(defn, _)) =>
          bump(Ungated)

        case _ =>
          ()

      catchCases(tree).foreach: caseDef =>
        if catchesEverything(caseDef.pat) then bump(CatchAll)

      // Name occurrences are counted in addition to tree shape, so a `var`
      // whose name is watched counts once as each.
      name(tree).foreach: named =>
        if Prefixed.matches(named) then bump(named)
        else if watched.contains(named) then bump(named)

    counts.toList.sortBy(_(0))

  // The name this tree refers to, if it refers to one. Imports reach here
  // through their selectors, which are themselves trees, so an
  // `import scala.language.unsafeNulls` is counted like any other reference.
  private def name(tree: untpd.Tree): Option[String] = tree match
    case untpd.Ident(name)     => Some(name.toString)
    case untpd.Select(_, name) => Some(name.toString)
    case _                     => None

  // The `case` clauses of a `catch`, whichever shape the tree is in. The
  // parser produces a `ParsedTry` whose handler is an ordinary expression;
  // only after desugaring is it a `Try` carrying its cases directly, and the
  // census reads the untyped tree, so it must know both.
  private def catchCases(tree: untpd.Tree): List[untpd.CaseDef] = tree match
    case node: untpd.Try => node.cases

    case node: untpd.ParsedTry => node.handler match
      case handler: untpd.Match => handler.cases
      case _                    => Nil

    case _ => Nil

  // Does this `catch` pattern catch everything? `case _: Throwable` and
  // `case error: Exception` both do; a pattern naming a specific exception
  // type does not.
  private def catchesEverything(pattern: untpd.Tree): Boolean = pattern match
    case untpd.Typed(_, tpt) => name(tpt).exists(CatchAllTypes.contains)
    case _                   => false

  // Does `defn` take the unsafe token in a `using` clause? The same test S1
  // applies, so the gate count and the rule cannot disagree about what a gate
  // is.
  private def gates(defn: untpd.DefDef, token: String): Boolean =
    defn.paramss.exists:
      case params: List[?] => params.exists:
        case param: untpd.ValDef => param.mods.is(Flags.Given) && name(param.tpt).contains(token)
        case _                   => false

  // A definition's annotations live in its `Modifiers`, which is a field rather than a child,
  // so a `productIterator` walk never reaches them: `@untrackedCaptures private var buffer`
  // would be counted as a `var` and not as the capture-checking escape it also is. They are
  // visited explicitly here.
  private def walk(tree: untpd.Tree)(visit: untpd.Tree => Unit): Unit =
    visit(tree)

    tree match
      case defn: untpd.MemberDef => defn.mods.annotations.foreach(walk(_)(visit))
      case _                     => ()

    tree.productIterator.foreach(descend(_, visit))

  private def descend(node: Any, visit: untpd.Tree => Unit): Unit = node match
    case sub: untpd.Tree    => walk(sub)(visit)
    case items: Iterable[?] => items.foreach(descend(_, visit))
    case _                  => ()
