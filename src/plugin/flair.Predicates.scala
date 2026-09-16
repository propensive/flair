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

import scala.collection.mutable

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.util.SourceFile

// The vocabulary of project rules: each predicate kind names one shape on the
// untyped tree (or, for `matches`, on the source text), and a rule is any
// number of them under one name. All of them are decided by the parser's
// output alone — no rule needs the typer — which is what lets `flair check`
// stop the compiler after parsing.
//
// Matching is by name and by tree shape, on the same untyped tree every style
// rule sees. That is the same precision class as the rest of the checker: a
// `Select` named `get` is counted wherever it appears, without asking what it
// selects from. A project rule is therefore a gauge rather than a semantic
// analysis — good enough to see a trend, or to forbid a spelling, and not to
// be read as anything finer.
object Predicates:
  val kinds: List[String] =
    List
      ( "invokes", "references", "annotates", "instantiates", "extends", "derives", "imports",
        "defines", "overrides", "uses", "matches" )

  // The constructs `uses` recognises.
  val constructs: Set[String] =
    Set
      ( "var", "null", "throw", "return", "while", "try", "catch-all", "implicit", "lazy",
        "abstract-type", "wildcard-import", "by-name", "default-argument", "symbolic-name" )

  // One match of one predicate, positioned at the construct it matched.
  final case class Hit(definition: Definition, predicate: Predicate, line: Int, column: Int)

  // Is this predicate well-formed? An unknown kind, an unknown construct or a
  // regex that does not compile is a configuration error, reported up front
  // rather than silently matching nothing.
  def validate(predicate: Predicate): Option[String] = predicate.kind match
    case "uses" if !constructs.contains(predicate.target) =>
      Some(s"`${predicate.target}` is not a construct `uses` recognises; expected one of "
          +constructs.toList.sorted.mkString("`", "`, `", "`"))

    case "matches" =>
      try { predicate.target.r; None }
      catch case _: Exception => Some(s"`${predicate.target}` is not a valid regular expression")

    case kind if !kinds.contains(kind) =>
      Some(s"`$kind` is not a predicate; expected one of ${kinds.mkString("`", "`, `", "`")}")

    case _ =>
      None

  // Every hit of every definition in one file, in one traversal.
  def hits(definitions: List[Definition], tree: untpd.Tree, source: SourceFile, text: String)
  :   List[Hit] =
    if definitions.isEmpty then Nil else
      val matchers = definitions.map(Matcher(_))
      val out      = mutable.ListBuffer[Hit]()

      def emit(definition: Definition, predicate: Predicate, offset: Int): Unit =
        val (line, column) = position(source, offset)
        out += Hit(definition, predicate, line, column)

      // Tree shapes. An annotation's own tree — `@Name(args)` is parsed as a
      // constructor call — is walked with `annotation` set, so that
      // `instantiates Name` and `invokes <init>` do not fire on it; only
      // `annotates` and `references` see inside an annotation.
      def walk(tree: untpd.Tree, annotation: Boolean): Unit =
        matchers.foreach(_.visit(tree, annotation, emit))

        tree match
          case defn: untpd.MemberDef =>
            defn.mods.annotations.foreach(walk(_, true))
          case _ => ()

        tree match
          case untpd.Annotated(arg, annot) =>
            walk(arg, annotation)
            walk(annot, true)
          case _ =>
            tree.productIterator.foreach(descend(_, annotation))

      def descend(node: Any, annotation: Boolean): Unit = node match
        case sub: untpd.Tree    => walk(sub, annotation)
        case items: Iterable[?] => items.foreach(descend(_, annotation))
        case _                  => ()

      walk(tree, false)

      // Text shapes: `matches` runs over the lines, independent of the tree.
      matchers.foreach(_.scan(text, emit))

      out.toList.sortBy(hit => (hit.line, hit.column))

  private def position(source: SourceFile, offset: Int): (Int, Int) =
    if offset < 0 || offset > source.content.length then (1, 1)
    else (source.offsetToLine(offset) + 1, source.column(offset) + 1)

  // A definition's predicates, compiled once: name globs become regexes.
  private final class Matcher(definition: Definition):
    private val compiled: List[(Predicate, Compiled)] =
      definition.predicates.map(p => (p, Compiled(p)))

    def visit(tree: untpd.Tree, annotation: Boolean, emit: (Definition, Predicate, Int) => Unit)
    :   Unit =
      compiled.foreach: (predicate, matcher) =>
        if matcher.tree(tree, annotation) then emit(definition, predicate, point(tree))

    def scan(text: String, emit: (Definition, Predicate, Int) => Unit): Unit =
      compiled.foreach: (predicate, matcher) =>
        matcher.regex.foreach: regex =>
          var offset = 0
          Config.lines(text).foreach: line =>
            regex.findFirstMatchIn(line).foreach(m => emit(definition, predicate, offset + m.start))
            offset += line.length + 1

  private def point(tree: untpd.Tree): Int =
    if tree.span.exists then tree.span.point else -1

  // One compiled predicate.
  private final class Compiled(predicate: Predicate):
    private val name: scala.util.matching.Regex = glob(predicate.target)

    // For `matches`: the pattern itself.
    val regex: Option[scala.util.matching.Regex] =
      if predicate.kind == "matches" then Some(predicate.target.r) else None

    // Does the name match the predicate's target? Targets are matched on the
    // simple name, with `*` standing for any run of characters.
    private def named(text: String): Boolean = name.matches(text)

    // Does a dotted path match? Exactly, by glob, or as a prefix: `imports
    // scala.collection.mutable` catches `scala.collection.mutable.Map`.
    private def pathed(path: String): Boolean =
      named(path) || path.startsWith(predicate.target + ".")

    def tree(tree: untpd.Tree, annotation: Boolean): Boolean = predicate.kind match
      case "references" => tree match
        case untpd.Ident(n)     => named(n.toString)
        case untpd.Select(_, n) => named(n.toString)
        case _                  => false

      case "annotates" => annotation && (tree match
        case untpd.New(tpt) => typeName(tpt).exists(named)
        case _              => false)

      case _ if annotation => false

      case "invokes" => tree match
        case untpd.Select(_, n)                   => named(n.toString)
        case untpd.Apply(fun, _)                  => invokedIdent(fun).exists(named)
        case untpd.TypeApply(fun, _)              => invokedIdent(fun).exists(named)
        case untpd.InfixOp(_, op, _)              => named(op.name.toString)
        case untpd.PostfixOp(_, op)               => named(op.name.toString)
        case untpd.PrefixOp(op, _)                => named(op.name.toString)
        case _                                    => false

      case "instantiates" => tree match
        case untpd.New(tpt) => typeName(tpt).exists(named)

        case untpd.Apply(fun, _) =>
          invokedIdent(fun).exists(n => n.headOption.exists(_.isUpper) && named(n))
            || (fun match
              case untpd.Select(_, n) => n.toString.headOption.exists(_.isUpper) && named(n.toString)
              case untpd.TypeApply(untpd.Select(_, n), _) =>
                n.toString.headOption.exists(_.isUpper) && named(n.toString)
              case _ => false)

        case _ => false

      case "extends" =>
        parents(tree).exists(parent => typeName(parent).exists(named))

      case "derives" =>
        template(tree).exists(_.derived.exists(d => typeName(d).exists(named)))

      case "imports" => tree match
        case untpd.Import(expr, selectors) =>
          path(expr).exists: base =>
            pathed(base) || selectors.exists: selector =>
              !selector.isWildcard && pathed(base + "." + selector.name.toString)
        case _ => false

      case "defines" => tree match
        case defn: untpd.MemberDef => definable(defn) && named(defn.name.toString)
        case _                     => false

      case "overrides" => tree match
        case defn: untpd.MemberDef => defn.mods.is(Flags.Override) && named(defn.name.toString)
        case _                     => false

      case "uses" => predicate.target match
        case "var"    => tree match
          case defn: untpd.ValDef => defn.mods.is(Flags.Mutable)
          case _                  => false
        case "null"   => tree match
          case untpd.Literal(Constant(null)) => true
          case _                             => false
        case "throw"  => tree.isInstanceOf[untpd.Throw]
        case "return" => tree.isInstanceOf[untpd.Return]
        case "while"  => tree.isInstanceOf[untpd.WhileDo]
        case "try"    => tree.isInstanceOf[untpd.Try] || tree.isInstanceOf[untpd.ParsedTry]
        case "catch-all" => catchCases(tree).exists(c => catchesEverything(c.pat))
        case "implicit" => tree match
          case defn: untpd.MemberDef => defn.mods.is(Flags.Implicit)
          case _                     => false
        case "lazy" => tree match
          case defn: untpd.MemberDef => defn.mods.is(Flags.Lazy)
          case _                     => false
        case "abstract-type" => tree match
          case defn: untpd.TypeDef =>
            !defn.mods.is(Flags.Param) && defn.rhs.isInstanceOf[untpd.TypeBoundsTree]
          case _ => false
        case "wildcard-import" => tree match
          case untpd.Import(_, selectors) => selectors.exists(_.isWildcard)
          case _                          => false
        case "by-name" => tree.isInstanceOf[untpd.ByNameTypeTree]
        case "default-argument" => tree match
          case defn: untpd.ValDef => defn.mods.is(Flags.Param) && !rhs(defn).isEmpty
          case _                  => false
        case "symbolic-name" => tree match
          case defn: untpd.DefDef =>
            val n = defn.name.toString
            n.nonEmpty && n.forall(c => !c.isLetterOrDigit && c != '_' && c != '<' && c != '>')
          case _ => false
        case _ => false

      case _ => false

  // The identifier a call applies, if it applies one directly: `f(x)` and
  // `f[T](x)`, but not `g(y)(x)` — the inner `Apply` counts that one.
  private def invokedIdent(fun: untpd.Tree): Option[String] = fun match
    case untpd.Ident(n)                     => Some(n.toString)
    case untpd.TypeApply(untpd.Ident(n), _) => Some(n.toString)
    case _                                  => None

  // The simple name of a type tree, through applied types and constructor
  // calls: `Foo`, `a.Foo`, `Foo[T]`, `new Foo(x)` and `Foo(x)` all name `Foo`.
  private def typeName(tree: untpd.Tree): Option[String] = tree match
    case untpd.Ident(n)                      => Some(n.toString)
    case untpd.Select(_, n)                  => Some(n.toString)
    case untpd.AppliedTypeTree(tycon, _)     => typeName(tycon)
    case untpd.New(tpt)                      => typeName(tpt)
    case untpd.Apply(fun, _)                 => typeName(fun).filterNot(_ == "<init>").orElse(constructed(fun))
    case untpd.TypeApply(fun, _)             => typeName(fun)
    case untpd.Parens(t)                     => typeName(t)
    case untpd.Annotated(arg, _)             => typeName(arg)
    case _                                   => None

  // `new Foo(x)` parses as `Apply(Select(New(Foo), <init>), x)`.
  private def constructed(fun: untpd.Tree): Option[String] = fun match
    case untpd.Select(untpd.New(tpt), _) => typeName(tpt)
    case untpd.TypeApply(f, _)           => constructed(f)
    case _                               => None

  // A dotted path from an import's prefix tree.
  private def path(tree: untpd.Tree): Option[String] = tree match
    case untpd.Ident(n)       => Some(n.toString)
    case untpd.Select(q, n)   => path(q).map(_ + "." + n.toString)
    case _                    => None

  // The template of a class, trait, object or enum, if `tree` defines one.
  private def template(tree: untpd.Tree): Option[untpd.Template] = tree match
    case defn: untpd.TypeDef   => defn.rhs match
      case t: untpd.Template => Some(t)
      case _                 => None
    case defn: untpd.ModuleDef => Some(defn.impl)
    case _                     => None

  // A template's parents, read off the tree directly: the accessor needs a
  // compiler context, and the parser's list is never lazy.
  private def parents(tree: untpd.Tree): List[untpd.Tree] =
    template(tree).to(List).flatMap: t =>
      t.productElement(1) match
        case list: List[?] => list.collect { case p: untpd.Tree => p }.filterNot(t.derived.contains)
        case _             => Nil

  // A `ValDef`'s right-hand side, read off the tree without a context.
  private def rhs(defn: untpd.ValDef): untpd.Tree = defn.productElement(2) match
    case t: untpd.Tree => t
    case _             => untpd.EmptyTree

  // Definitions `defines` has an opinion about: not parameters, not
  // constructors, not the synthetic self.
  private def definable(defn: untpd.MemberDef): Boolean =
    !defn.mods.is(Flags.Param) && !defn.mods.is(Flags.ParamAccessor)
      && !defn.name.toString.startsWith("<") && !defn.name.toString.isEmpty

  private def glob(target: String): scala.util.matching.Regex =
    target.split("\\*", -1).nn.map(part => java.util.regex.Pattern.quote(part).nn).mkString(".*").r

  // The `case` clauses of a `catch`, whichever shape the tree is in. The
  // parser produces a `ParsedTry` whose handler is an ordinary expression;
  // only after desugaring is it a `Try` carrying its cases directly, and the
  // census reads the untyped tree, so it must know both.
  def catchCases(tree: untpd.Tree): List[untpd.CaseDef] = tree match
    case node: untpd.Try => node.cases

    case node: untpd.ParsedTry => node.handler match
      case handler: untpd.Match => handler.cases
      case _                    => Nil

    case _ => Nil

  // Does this `catch` pattern catch everything? `case _: Throwable` and
  // `case error: Exception` both do; a pattern naming a specific exception
  // type does not.
  private val CatchAllTypes = Set("Throwable", "Exception")

  def catchesEverything(pattern: untpd.Tree): Boolean = pattern match
    case untpd.Typed(_, tpt)  => typeName(tpt).exists(CatchAllTypes.contains)
    case untpd.Bind(_, body)  => catchesEverything(body)
    case _                    => false
