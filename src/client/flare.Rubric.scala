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

import soundness.*

import alphabets.hexLowerCase
import charsets.utf8Charset
import providers.javaBaseProvider
import textSanitizers.skipSanitizer

object Rubric:
  val begin: Text = t"<!-- rubric:begin -->"
  val end: Text = t"<!-- rubric:end -->"

  def load(root: Text, path: Text): Optional[Rubric] =
    val absolute: Text = if path.starts(t"/") then path else t"$root/$path"
    val file: Optional[Path on Linux] = safely(absolute.as[Path on Linux])

    file.let { (file: Path on Linux) => safely(file.read[Text]) }.let: (whole: Text) =>
      val text: Text = section(whole)
      Rubric(path, text, text.digest[Sha2[256]].serialize[Hex])

  // The delimited section, or the whole text when it has no markers.
  def section(whole: Text): Text = whole.cut(begin) match
    case _ :: after :: _ => after.cut(end).head.trim
    case _               => whole.trim

// The prose a model is shown: the file an assessment rule names, or the section of it between
// `<!-- rubric:begin -->` and `<!-- rubric:end -->` when the markers are present, so a standard
// can carry its rubric among prose for people. Its digest travels with every judgement, so a
// rubric that has changed since is visible in the notes, and a changed rubric re-judges.
case class Rubric(path: Text, text: Text, digest: Text)
