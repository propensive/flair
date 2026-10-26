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
┃   Flare, version 0.2.1.                                                                          ┃
┃   © Copyright 2025-26 Jon Pretty, Propensive OÜ.                                                 ┃
┃                                                                                                  ┃
┃   The primary distribution site is:                                                              ┃
┃                                                                                                  ┃
┃       https://propensive.dev/flare/                                                              ┃
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
package flare

import scala.collection.mutable

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.util.SourceFile

// The definitions an assessment rule puts to a model: small methods (and `given Conversion`s)
// whose shape suggests they exist only to coerce a value from one type to another — the
// "plumbing" a missing abstraction leaves behind. This is the gathering half of `flare assess`;
// judging is the client's, since it needs the network. Everything here is decided from the
// untyped tree and the source text alone, as every other rule is.
//
// A candidate is identified SEMANTICALLY, never by line: its `locus` is the path plus the chain
// of enclosing definitions and its own signature as written (`Tels.Flag.kebab(s: String)`, or
// `parse(text: Text)/liftText(s: String)` for a local def), so inserting lines above it changes
// nothing. Its `normalised` text — the definition with parameter names replaced by `_p0…` and
// whitespace collapsed — is what a judgement is keyed by, so a rename, a move or a reformat
// keeps the judgement and only a change to the definition's own text invalidates it.
object Candidates:
  enum Kind:
    case Local, Private, Protected, Public, Conversion

    def word: String = this match
      case Local      => "local"
      case Private    => "private"
      case Protected  => "protected"
      case Public     => "public"
      case Conversion => "conversion"

  // Which shapes let a definition through: a `given Conversion`; a `match` whose cases map
  // constructors one-to-one (at least `ladder` of them, at most one not conforming); a round
  // trip through a foreign representation; a body of at most `adapter` lines built from adapter
  // calls; a coercion-verb name on a body of at most `verb` lines. `None` disables a gate.
  final case class Gates
    ( conversion: Boolean     = true,
      ladder:     Option[Int] = Some(2),
      roundTrip:  Boolean     = true,
      adapter:    Option[Int] = Some(3),
      verb:       Option[Int] = Some(3) )

  object Filters:
    val codecs: List[String] =
      List("read*", "write*", "parse*", "decode*", "encode*", "serialize*", "deserialize*",
          "apply", "unapply")

  // What a rule admits: bounds, kinds, exclusions and gates.
  final case class Filters
    ( maxLines:      Int          = 12,
      kinds:         Set[String]  = Set("local", "private", "protected", "public", "conversion"),
      excludeNames:  List[String] = Filters.codecs,
      excludeBodies: List[String] = List("'{", "Expr"),
      noUsing:       Boolean      = true,
      gates:         Gates        = Gates() )

  // One definition put forward. `line` is for display only. `calls` counts uses of the name
  // within its scope (the enclosing definition for a local, else the file).
  final case class Candidate
    ( path:       String,
      locus:      String,
      line:       Int,
      name:       String,
      kind:       Kind,
      params:     Int,
      bodyLines:  Int,
      oneLiner:   Boolean,
      calls:      Int,
      normalised: String,
      excerpt:    String )

  // One frame of the owner chain: a name, and whether it is a term (a def or val, making
  // everything beneath it local) rather than a type or object.
  private final case class Owner(name: String, term: Boolean)

  private val adapter =
    ("""\.(tt|s|nn|toString|toInt|toLong|toDouble|toByte|toChar|toList|toMap|toSeq|toArray""" +
      """|getBytes|stdlib|to)\b|\bText\(|\bArray\.unsafe(Frozen|Jvm)\(|\basInstanceOf\b""").r

  private val roundTrip =
    ("""\.s\b[\s\S]*\.tt\b|\.toString\b[\s\S]*\.tt\b|\.stdlib\b[\s\S]*\.to[\[(]""" +
      """|unsafeJvm\b[\s\S]*unsafeFrozen\b""").r

  private val logic = """=>|\bif\b|\bmatch\b|\bwhile\b|[-+*/%<>!]|&&|\|\|""".r

  private val verb =
    ("""^(to|from|as|make|mk|convert|lift|wrap|unwrap)[A-Z]""" +
      """|^(text|string|bytes|number|decimal|hex|key|node)$""").r

  private val modifier =
    ("""^(private|protected|inline|transparent|final|override|given|infix|implicit|erased""" +
      """|lazy)(\[[^\]]*\])?\s+""").r

  // A definition's right-hand side, read from the tree's fields rather than through `rhs`,
  // which the compiler's API guards with a `Context` this parse-time walk has none of.
  private def rhsOf(defn: untpd.ValOrDefDef): untpd.Tree =
    val index = defn match
      case _: untpd.DefDef => 3
      case _               => 2

    defn.productElement(index) match
      case t: untpd.Tree => t
      case _             => untpd.EmptyTree

  // The parser may wrap a body in an empty block or parentheses; the shape beneath is what
  // counts.
  private def unwrap(tree: untpd.Tree): untpd.Tree = tree match
    case untpd.Block(Nil, expr) => unwrap(expr)
    case untpd.Parens(expr)     => unwrap(expr)
    case _                      => tree

  private def typeName(tree: untpd.Tree): Option[String] = tree match
    case untpd.Ident(name)     => Some(name.toString)
    case untpd.Select(_, name) => Some(name.toString)
    case _                     => None

  private def capitalised(name: String): Boolean = name.headOption.exists(_.isUpper)

  // A case body that names a constructor, or applies one: `Frame.Z`, `TextNode(text)`,
  // `Frame.L(t"?")`. What the constructor is applied to does not matter; that it is a
  // constructor, and not a computation, does.
  private def constant(body: untpd.Tree): Boolean = unwrap(body) match
    case untpd.Ident(name)     => capitalised(name.toString)
    case untpd.Select(_, name) => capitalised(name.toString)
    case untpd.Apply(fun, _)   => typeName(fun).exists(capitalised)
    case _                     => false

  private def ladder(rhs: untpd.Tree, minimum: Int): Boolean = unwrap(rhs) match
    case untpd.Match(_, cases) =>
      val nonconforming = cases.count: c => !c.guard.isEmpty || !constant(c.body)
      cases.length >= minimum && nonconforming <= 1

    case _ =>
      false

  private def valueParams(defn: untpd.ValOrDefDef): List[untpd.ValDef] = defn match
    case defn: untpd.DefDef =>
      defn.paramss.flatMap:
        case clause: List[?] => clause.collect { case param: untpd.ValDef => param }

    case _ =>
      Nil

  private def contextual(defn: untpd.ValOrDefDef): Boolean =
    valueParams(defn).exists(_.mods.isOneOf(Flags.Given | Flags.Erased | Flags.Implicit))

  private def isConversion(defn: untpd.ValOrDefDef): Boolean =
    defn.mods.is(Flags.Given) && (defn.tpt match
      case untpd.AppliedTypeTree(tycon, _) => typeName(tycon).contains("Conversion")
      case _                               => false)

  // Uses of a name within a tree: identifiers and selections, which is what a call is at parse
  // time.
  private def uses(scope: untpd.Tree, name: String): Int =
    var count = 0

    def visit(t: untpd.Tree): Unit =
      t match
        case untpd.Ident(n) if n.toString == name     => count += 1
        case untpd.Select(_, n) if n.toString == name => count += 1
        case _                                        => ()

      t.productIterator.foreach(descend)

    def descend(x: Any): Unit = x match
      case sub: untpd.Tree  => visit(sub)
      case it:  Iterable[?] => it.foreach(descend)
      case _                => ()

    visit(scope)
    count

  // Leading modifiers, repeatedly.
  private def strip(text: String): String =
    val trimmed = text.trim.nn

    modifier.findPrefixOf(trimmed) match
      case Some(prefix) => strip(trimmed.substring(prefix.length).nn)
      case None         => trimmed

  private def normalise(definition: String, params: List[String]): String =
    val renamed = params.zipWithIndex.foldLeft(definition): (acc, entry) =>
      val (param, index) = entry
      acc.replaceAll("\\b" + java.util.regex.Pattern.quote(param) + "\\b", s"_p$index").nn

    renamed.replaceAll("\\s+", " ").nn.trim.nn

  // The signature of an owner, for the chain: a def's name with its value parameters.
  private def signatureOf(defn: untpd.DefDef): String =
    val name = defn.name.toString
    val params = valueParams(defn).map(_.name.toString).mkString(", ")
    if params.isEmpty then name else s"$name($params)"

  // Frames join with `.`, except that everything beneath a term (a def or val) is local to it
  // and joins with `/`.
  private def locus(path: String, owners: List[Owner], signature: String): String =
    val frames = owners.reverse.map(_.name) :+ signature
    val parents = owners.reverse.map(_.term)

    val joined = frames.zipWithIndex.map: (frame, index) =>
      if index == 0 then frame else (if parents(index - 1) then "/" else ".") + frame

    path + " ▸ " + joined.mkString

  // The candidates in one file.
  def collect(path: String, text: String, tree: untpd.Tree, source: SourceFile, filters: Filters)
  :   List[Candidate] =

    val found = mutable.ListBuffer[Candidate]()
    val lines: IndexedSeq[String] = text.split("\n", -1).toIndexedSeq

    def slice(start: Int, end: Int): String =
      if start < 0 || end > text.length || start >= end then "" else text.substring(start, end).nn

    def lineOf(offset: Int): Int = source.offsetToLine(offset) + 1

    def excluded(name: String): Boolean =
      filters.excludeNames.exists: pattern => Predicates.glob(pattern).matches(name)

    def gated(defn: untpd.ValOrDefDef, kind: Kind, body: String, bodyLines: Int): Boolean =
      val gates = filters.gates
      val name = defn.name.toString
      val adapted = adapter.findFirstIn(body).isDefined
      val tripped = gates.roundTrip && roundTrip.findFirstIn(body).isDefined

      if kind == Kind.Conversion then gates.conversion
      else if gates.ladder.exists(ladder(rhsOf(defn), _)) then true
      else if kind == Kind.Public then
        val chain = bodyLines == 1 && adapted && logic.findFirstIn(body).isEmpty
        tripped || (gates.adapter.isDefined && chain)
      else
        val verbed = gates.verb.exists(bodyLines <= _) && verb.findFirstIn(name).isDefined
        verbed || (gates.adapter.exists(bodyLines <= _) && adapted) || tripped

    def consider(defn: untpd.ValOrDefDef, owners: List[Owner], scope: untpd.Tree): Unit =
      // An anonymous given has no name at parse time; it is called by what it is.
      val name = if defn.name.toString.isEmpty then "conversion" else defn.name.toString
      val rhs = rhsOf(defn)
      val skipped = defn.mods.is(Flags.Override) || excluded(name)

      if rhs.isEmpty || !defn.span.exists || !rhs.span.exists then ()
      else if skipped || (filters.noUsing && contextual(defn)) then ()
      else
        val startLine = lineOf(defn.span.start)
        val rhsStart = lineOf(rhs.span.start)
        val endLine = lineOf(rhs.span.end)
        val bodyLines = endLine - rhsStart + 1
        val body = slice(rhs.span.start, rhs.span.end)

        val kind: Kind =
          if isConversion(defn) then Kind.Conversion
          else if owners.exists(_.term) then Kind.Local
          else if defn.mods.is(Flags.Private) then Kind.Private
          else if defn.mods.is(Flags.Protected) then Kind.Protected
          else Kind.Public

        val bodyExcluded = filters.excludeBodies.exists(body.contains(_))
        val admitted = filters.kinds.contains(kind.word) && !bodyExcluded

        if bodyLines > filters.maxLines || !admitted then ()
        else if !gated(defn, kind, body, bodyLines) then ()
        else
          // The signature as written: from the definition's start to its body, less the
          // modifiers and the `def` keyword, whitespace collapsed.
          val head = slice(defn.span.start, rhs.span.start).replaceAll("\\s*=\\s*$", "").nn
          val stripped = strip(head).replaceAll("\\s+", " ").nn.trim.nn
          val signature = if stripped.startsWith("def ") then stripped.substring(4).nn else stripped
          val params = valueParams(defn).map(_.name.toString)

          // The definition's own name is elided from the normalised text (a rename keeps the
          // digest; the locus carries the name), as are its parameter names.
          val pattern = "^(def\\s+|val\\s+|var\\s+)?" + java.util.regex.Pattern.quote(name)
          val whole = strip(slice(defn.span.start, rhs.span.end)).replaceFirst(pattern, "$1_").nn
          val enclosing: Option[String] = owners.headOption.filter(_.term).map(_.name + " …")
          val shown = lines.slice((startLine - 3).max(0), endLine.min(lines.length)).mkString("\n")
          val excerpt = enclosing.fold(shown)(_ + "\n" + shown)

          found +=
            Candidate
              ( path, locus(path, owners, signature), startLine, name, kind, params.length,
                bodyLines, rhsStart == startLine, uses(scope, name), normalise(whole, params),
                excerpt )

    def walk(t: untpd.Tree, owners: List[Owner], scope: untpd.Tree): Unit = t match
      case defn: untpd.DefDef =>
        consider(defn, owners, scope)
        val rhs = rhsOf(defn)
        walk(rhs, Owner(signatureOf(defn), true) :: owners, rhs)

      case defn: untpd.ValDef =>
        // A `given Conversion[A, B] = …` with no parameters is a val, and a candidate.
        if defn.mods.is(Flags.Given) then consider(defn, owners, scope)
        val rhs = rhsOf(defn)
        walk(rhs, Owner(defn.name.toString, true) :: owners, rhs)

      case module: untpd.ModuleDef =>
        walk(module.impl, Owner(module.name.toString, false) :: owners, scope)

      case typeDef: untpd.TypeDef =>
        typeDef.rhs match
          case template: untpd.Template =>
            val name = typeDef.name.toString
            val shown = if name.contains("$anon") then "<anon>" else name
            walk(template, Owner(shown, false) :: owners, scope)

          case _ =>
            ()

      case ext: untpd.ExtMethods =>
        val params = ext.paramss.flatMap:
          case clause: List[?] => clause.collect { case param: untpd.ValDef => param.name.toString }

        val owner = Owner(s"extension(${params.mkString(", ")})", false)
        ext.methods.foreach(walk(_, owner :: owners, scope))

      case _ =>
        t.productIterator.foreach(descend(_, owners, scope))

    def descend(x: Any, owners: List[Owner], scope: untpd.Tree): Unit = x match
      case sub: untpd.Tree  => walk(sub, owners, scope)
      case it:  Iterable[?] => it.foreach(descend(_, owners, scope))
      case _                => ()

    walk(tree, Nil, tree)
    found.toList

  // How many other candidates share each candidate's normalised text, across every file: the
  // same helper copied into six atomizers counts five twins for each.
  def duplicates(all: List[Candidate]): Map[String, Int] =
    val counts = all.groupBy(_.normalised).view.mapValues(_.length).toMap
    counts.map: (text, count) => (text, count - 1)
