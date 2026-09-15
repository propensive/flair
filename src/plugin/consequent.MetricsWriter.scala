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

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

import scala.collection.mutable

// Persists the per-file metrics census as a tab-separated table, one record
// per `file`, `indicator`, `count`.
//
// The write *merges*: a compile that rebuilds three files must leave the
// records for every other file alone, or an incremental build would report
// only what it happened to recompile. The records for the files this run
// compiled are replaced, and the rest are kept — which makes the file a
// running picture of the whole corpus, converging on the truth as modules
// are rebuilt and exactly right after a clean build.
//
// The format is deliberately the plainest thing that survives `sort`, `cut`
// and `awk`, because the consumer is a project's own reporting script.
object MetricsWriter:
  def merge(path: String, records: Iterable[(String, String, Int)], compiled: Set[String]): Unit =
    val file     = Paths.get(path).nn
    val retained = read(file).filterNot { record => compiled.contains(record(0)) }
    val merged   = (retained ++ records).distinct.sortBy { record => (record(0), record(1)) }

    write(file, merged)

  // Read the existing table, ignoring any line that is not a well-formed
  // record: a truncated or hand-edited file must not fail a compilation.
  private def read(file: Path): List[(String, String, Int)] =
    if !Files.exists(file) then Nil else
      try
        Files.readAllLines(file, UTF_8).nn.toArray.to(List).flatMap: line =>
          line.toString.split("\t").nn.toList match
            case List(source, indicator, count) => count.nn.toIntOption.map((source.nn, indicator.nn, _))
            case _                              => None

      catch case _: Exception => Nil

  // Write through a sibling temporary file and move it into place, so a
  // reader never sees a half-written table and a crash never truncates the
  // one that was there.
  private def write(file: Path, records: List[(String, String, Int)]): Unit =
    try
      Option(file.getParent).foreach { dir => Files.createDirectories(dir) }

      val text = mutable.StringBuilder()
      records.foreach { case (source, indicator, count) =>
        text.append(source).append('\t').append(indicator).append('\t').append(count).append('\n') }

      val temporary = Files.createTempFile(file.getParent.nn, file.getFileName.nn.toString, ".tmp").nn
      Files.write(temporary, text.toString.getBytes(UTF_8).nn)
      Files.move(temporary, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    catch case _: Exception => ()
