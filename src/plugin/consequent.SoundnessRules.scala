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
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.NameOps.isConstructorName
import dotty.tools.dotc.util.SourceFile

object SoundnessRules:
  // S1: a method that takes the project's unsafe token as a `using` parameter
  // must be named for what it is, and a method named for what it is must take
  // the token. The two directions are one rule because either alone is a lie:
  // an ungated `unsafe…` name claims a checked boundary that nothing enforces,
  // and a gated method with an ordinary name hides one that does.
  //
  // The token is named by `unsafeToken`, and the rule does not fire when that
  // option is unset — a project that has no such token has nothing to check.
  object UnsafeNaming extends Rule:
    def id: String = "S1"
    def principle: Principle = Principle.Soundness

    // `unsafe` followed by an uppercase letter: the prefix must be a word of
    // its own. This is what exempts `unsafely` — the entry point that supplies
    // the token — and `unsafety`, without either needing a special case.
    private val Prefixed = """unsafe[A-Z][A-Za-z0-9_]*""".r

    def check(ctx: Context): List[Violation] =
      ctx.config.unsafeToken.to(List).flatMap: token =>
        val out = mutable.ListBuffer[Violation]()

        walk(ctx.tree):
          case defn: untpd.DefDef if checkable(defn) =>
            val name     = defn.name.toString
            val prefixed = Prefixed.matches(name)
            val gated    = gates(defn, token)

            if gated && !prefixed then
              out += violation(ctx, defn, "S1.1",
                  s"`$name` is gated by `$token`, so its name must begin `unsafe`")
            else if prefixed && !gated then
              out += violation(ctx, defn, "S1.2",
                  s"`$name` is named as unsafe, so it must take `(using erased $token)`")

          case _ =>
            ()

        out.toList

    // Definitions the rule has an opinion about. A constructor cannot be
    // renamed, and a `given` is summoned by type rather than called by name,
    // so neither can carry the prefix; both are left to the soundness
    // argument in their comment.
    private def checkable(defn: untpd.DefDef): Boolean =
      !defn.name.isConstructorName && !defn.mods.is(Flags.Given)

    // Does `defn` take the unsafe token in a `using` clause? The token is
    // matched on its simple name, so `Unsafe`, `vacuous.Unsafe` and an
    // `erased` or anonymous parameter all count.
    private def gates(defn: untpd.DefDef, token: String): Boolean =
      val simple = token.split("\\.").nn.last.nn

      defn.paramss.exists:
        case params: List[?] => params.exists:
          case param: untpd.ValDef => param.mods.is(Flags.Given) && names(param.tpt, simple)
          case _                   => false

    // Does this type tree name `target`? Only the simple name is compared, so
    // a qualified reference matches its own last segment.
    private def names(tpt: untpd.Tree, target: String): Boolean = tpt match
      case untpd.Ident(name)     => name.toString == target
      case untpd.Select(_, name) => name.toString == target
      case _                     => false

    // A violation at the definition's name. `span.point` is the name's offset
    // for a `DefDef`, which puts the diagnostic on the identifier rather than
    // on a leading modifier.
    private def violation
       (ctx: Context, defn: untpd.DefDef, rule: String, message: String): Violation =
      val (line, column) = position(ctx.source, defn.span.point)
      Violation(ctx.file, line, column, rule, message)

    private def position(source: SourceFile, offset: Int): (Int, Int) =
      if offset < 0 || offset > source.content.length then (1, 1)
      else (source.offsetToLine(offset) + 1, source.column(offset) + 1)

  // Generic untyped-tree pre-order traversal, as in `Definitions`.
  private def walk(tree: untpd.Tree)(visit: untpd.Tree => Unit): Unit =
    visit(tree)
    tree.productIterator.foreach(descend(_, visit))

  private def descend(node: Any, visit: untpd.Tree => Unit): Unit = node match
    case sub: untpd.Tree  => walk(sub)(visit)
    case items: Iterable[?] => items.foreach(descend(_, visit))
    case _                  => ()
