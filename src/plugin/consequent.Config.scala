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

// The parameters a project fixes when it adopts the standard. Part I of
// doc/consequent-style.md states each principle in terms of these rather than
// of any particular project's choices: the licence header is `header` lines
// long, a line may be `columns` wide, and the umbrella re-export package — if
// the project has one — is named by `umbrella`.
//
// Every field has a default, so a project that adopts the conventional shape
// passes no options at all.
final case class Config
  ( // Report violations as errors rather than warnings.
    errors: Boolean = false,

    // The number of lines in the licence header. Line `header + 1` must be the
    // `package` declaration. Zero disables the header rules (F1, F2, F3)
    // entirely, for a project that has no licence header.
    header: Int = 32,

    // The maximum width of a line, in columns.
    columns: Int = 100,

    // The umbrella package into which every public module is re-exported. When
    // unset, the umbrella-export rules (L4, L5) do not fire — a project need
    // not have an umbrella package.
    umbrella: Option[String] = None,

    // The path segment below which each module has its own directory, so that
    // `<moduleRoot>/<module>/src/...` identifies the module a file belongs to.
    moduleRoot: String = "lib",

    // The `-language` features to enable when parsing a file standalone. When
    // unset, the compatibility default for the running compiler is used. The
    // standalone parse must match the compiler's own parse, or the tree-based
    // rules see a different program from the one being compiled.
    language: Option[List[String]] = None,

    // The project's unsafe token: the type whose presence as a `using`
    // parameter marks a definition as bypassing a guarantee the compiler
    // would otherwise enforce (Soundness's `vacuous.Unsafe`, for example).
    // Matched on its simple name, so a qualified value works too. When unset,
    // the soundness rules (S1) do not fire — a project without such a token
    // has nothing for them to check.
    unsafeToken: Option[String] = None,

    // Rules reported as errors even when `errors` is false, named by rule id
    // or by family prefix (`S1`, or `S` for every soundness rule). A project
    // adopting the standard gradually can hold the rules it has already
    // satisfied at error severity without promoting the rest.
    strict: Set[String] = Set.empty,

    // Where to write the per-file metrics census. When unset, nothing is
    // counted and nothing is written; the census costs one extra traversal
    // per file, which is why it is opt-in.
    metrics: Option[String] = None,

    // Identifiers the census counts by name, in addition to the constructs it
    // always counts. Every `unsafe`-prefixed name is counted whether listed or
    // not, so this is for the rest — `asInstanceOf`, `nn`, `get` and whatever
    // else a project has decided to watch. Repeating the option adds to the
    // set rather than replacing it, so a long list can be passed as several
    // arguments.
    count: Set[String] = Set.empty,

    // Interpolators whose leading and trailing whitespace is insignificant, so
    // that a multi-line `"""…"""` argument may be laid out as an indented
    // block (A8). Every other interpolator carries significant whitespace and
    // is exempt from the layout rules.
    interpolators: Set[String] = Set("m", "j", "x", "y", "tel") ):

  // The line on which the `package` declaration must appear.
  def packageLine: Int = header + 1

  // Does this project have a licence header at all?
  def framed: Boolean = header > 0


object Config:
  // Parse the plugin's `-P:consequent:<option>` arguments. An option the
  // plugin does not recognise, or one whose value does not parse, is reported
  // rather than ignored: a silently-dropped `columns=120` would enforce a
  // limit the project did not ask for.
  def parse(options: List[String]): (Config, List[String]) =
    var config = Config()
    val errors = List.newBuilder[String]

    def int(key: String, value: String)(update: Int => Unit): Unit =
      value.toIntOption match
        case Some(n) if n >= 0 => update(n)
        case _ => errors += s"the `$key` option requires a non-negative integer, not `$value`"

    // A list-valued option's items. Both `,` and `;` separate, because the
    // compiler splits a `-P:plugin:key=a,b` argument on the comma before the
    // plugin ever sees it — the `b` arrives as a bare `-P:b` and is rejected
    // as an unknown option. A list passed through `-P` must therefore use `;`;
    // the comma is still accepted for a `Config` built directly.
    def items(value: String): List[String] =
      value.split("[,;]").nn.toList.map(_.nn.trim.nn).filter(_.nonEmpty)

    options.foreach: option =>
      val (key, value) = option.indexOf('=') match
        case -1    => (option, "")
        case index => (option.substring(0, index).nn, option.substring(index + 1).nn)

      key match
        case "errors"        => config = config.copy(errors = true)
        case "header"        => int(key, value)(n => config = config.copy(header = n))
        case "columns"       => int(key, value)(n => config = config.copy(columns = n))
        case "umbrella"      => config = config.copy(umbrella = Some(value).filter(_.nonEmpty))
        case "moduleRoot"    => config = config.copy(moduleRoot = value)
        case "language"      => config = config.copy(language = Some(items(value)))
        case "interpolators" => config = config.copy(interpolators = items(value).to(Set))
        case "unsafeToken"   => config = config.copy(unsafeToken = Some(value).filter(_.nonEmpty))
        case "strict"        => config = config.copy(strict = config.strict ++ items(value))
        case "metrics"       => config = config.copy(metrics = Some(value).filter(_.nonEmpty))
        case "count"         => config = config.copy(count = config.count ++ items(value))

        case other =>
          errors +=
            ( s"`$other` is not a recognised option; expected one of `errors`, `header`, "
                +"`columns`, `umbrella`, `moduleRoot`, `language`, `interpolators`, "
                +"`unsafeToken`, `strict`, `metrics` or `count`" )

    (config, errors.result())
