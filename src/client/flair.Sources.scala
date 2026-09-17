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
┃   Flair, version 0.2.0.                                                                          ┃
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

import soundness.*

import filesystemBackends.javaBaseFilesystem
import filesystemOptions.dereferenceSymlinks

// The files a profile applies to: its `source` globs expanded against the project root, less its
// `exclude` globs, and — when the command line names paths — only those beneath one of them.
object Sources:
  def expand(root: Text, profile: Workspace.Profile, restrict: List[Text]): List[Text] raises Io.Error =
    val base: Path on Linux = unsafely(root.as[Path on Linux])

    val matched: List[Text] =
      profile.sources.bind[List[Text], Text, List[Text]] { (pattern: Text) => base.glob(Glob.parse(pattern)).map(_.encode) }.distinct

    val excluded: List[Glob] = profile.excludes.map(Glob.parse(_))

    def relative(path: Text): Text = path.skip(root.length + 1)

    val kept: List[Text] =
      matched.filter { (path: Text) => !excluded.exists(_.matches(relative(path))) }

    if restrict.nil then kept
    else kept.filter { (path: Text) => restrict.exists { (r: Text) => path == r || path.starts(t"$r/") } }

  // A command-line path, resolved against the invocation directory to an absolute one with no
  // trailing slash, so the prefix test above is exact.
  def resolve(directory: Text, argument: Text): Text =
    val absolute = if argument.starts(t"/") then argument else t"$directory/$argument"
    safely(absolute.as[Path on Linux]).let(_.encode).or(absolute)
