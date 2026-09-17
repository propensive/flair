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

import logging.silentLogging
import gitCommands.searchpathGitCommand

// The census, and where it goes. `flair metrics` counts every project rule's matches and every
// gate's uses across a profile's sources, and records the result in git notes so that the counts
// can be followed commit by commit:
//
//  - the census itself is a note on the git TREE object of exactly the input files (made with a
//    temporary index and `write-tree`), so that identical inputs across commits share one
//    measurement and are never measured twice;
//  - a pointer note on the HEAD commit names the profile, the tree and the time, which is what a
//    walk of the history follows to plot a series.
//
// Both live under `refs/notes/<namespace>` (`flair` unless the profile says otherwise), and are
// pushed and fetched like any notes ref: `git push origin refs/notes/flair`.
//
// The input tree is a dangling object — no commit refers to it — so `git gc` may prune it once
// it expires. The note survives (it lives in the notes ref), so a stored census is read back
// through the notes ref's own tree, never by asking git to resolve the input tree again.
object Census:
  case class Measurement
    ( profile:  Text,
      tree:     Text,
      commit:   Text,
      dirty:    Boolean,
      measured: Text,
      files:    Int,
      totals:   List[(Text, Int)],
      perFile:  List[(Text, List[(Text, Int)])] )

  // The census note, as TEL: totals first, then each file's counts, so a reader — or a chart —
  // that wants only the totals need not read the rest.
  def body(m: Measurement): Text =
    val fixed: List[Text] =
      List
        ( t"tel 1.0", t"", t"profile ${m.profile}", t"tree ${m.tree}", t"commit ${m.commit}",
          t"measured ${m.measured}", t"flair $version", t"files ${m.files}" )

    val head: List[Text] = if m.dirty then fixed + List(t"dirty") else fixed

    val totals: List[Text] = m.totals.map { (indicator: Text, total: Int) => t"total $indicator $total" }

    val files: List[Text] =
      m.perFile.bind[List[Text], Text, List[Text]]: (file: Text, counts: List[(Text, Int)]) =>
        List(t"file $file") + counts.map { (indicator: Text, count: Int) => t"  $indicator $count" }

    (head + List(t"") + totals + List(t"") + files).join(t"\n") + t"\n"

  // The pointer note appended to the commit: one `measurement` per profile measured.
  def pointer(m: Measurement): Text =
    val fixed: List[Text] =
      List(t"measurement", t"  profile ${m.profile}", t"  tree ${m.tree}", t"  measured ${m.measured}")

    val lines: List[Text] = if m.dirty then fixed + List(t"  dirty") else fixed
    lines.join(t"\n") + t"\n"

  val version: Text = t"0.2.0"

  // Sum the per-file counts into totals, sorted by indicator.
  def totals(perFile: List[(Text, List[(Text, Int)])]): List[(Text, Int)] =
    val all: List[(Text, Int)] = perFile.bind[List[(Text, Int)], (Text, Int), List[(Text, Int)]](_(1))
    val indicators: List[Text] = List.from(all.map(_(0)).distinct.stdlib.sortBy(_.s))
    indicators.map { (indicator: Text) => (indicator, all.filter(_(0) == indicator).map(_(1)).stdlib.sum) }

// The git repository a project root sits in, and the operations the census needs of it. Every
// call shells out to `git` — through octogenarian for notes, and directly for the two things it
// has no word for (`write-tree` under a temporary index, and reading a notes ref's tree).
class Repository(val toplevel: Text, val gitDir: Text):
  private val repo: Git.Repo = Git.Repo(unsafely(gitDir.as[Path on Linux]))

  def head()(using WorkingDirectory): Optional[Text] =
    safely(sh"git -C $toplevel rev-parse --verify HEAD".exec[Text]().trim).let: (text: Text) =>
      if text.length == 40 then text else Unset

  // Whether any tracked file differs from HEAD: a measurement of a dirty tree is still a
  // measurement, but the pointer note should say so.
  def dirty()(using WorkingDirectory): Boolean =
    safely(sh"git -C $toplevel status --porcelain --untracked-files=no".exec[Text]().trim)
    . lay(false)(_ != t"")

  // The tree object of exactly `files`, written under a throwaway index so the real index and the
  // work tree are untouched. The index sorts its entries, so the hash does not depend on the
  // order the files arrive in.
  def inputTree(files: List[Text])(using WorkingDirectory): Optional[Text] =
    safely:
      val index = java.io.File.createTempFile("flair-index", ".idx").nn
      index.delete()
      val indexPath: Text = index.getAbsolutePath.nn.tt

      try
        val added: Exit =
          sh"env GIT_INDEX_FILE=$indexPath git -C $toplevel update-index --add -- $files".exec[Exit]()

        val tree: Text = sh"env GIT_INDEX_FILE=$indexPath git -C $toplevel write-tree".exec[Text]().trim
        if added == Exit.Ok && tree.length == 40 then tree else Unset
      finally index.delete()

  def resolve(refspec: Text)(using WorkingDirectory): Optional[Text] =
    safely(sh"git -C $toplevel rev-parse --verify --end-of-options $refspec".exec[Text]().trim)
    . let { (text: Text) => if text.length == 40 then text else Unset }

  def objectType(hash: Text)(using WorkingDirectory): Optional[Text] =
    safely(sh"git -C $toplevel cat-file -t $hash".exec[Text]().trim)

  private def ref(namespace: Text): Path on Git.Refs = unsafely(Git.Refs.notes(namespace))

  // A note, if the object has one under the namespace.
  def note(namespace: Text, hash: Text)(using WorkingDirectory): Optional[Text] =
    safely(repo.notes.show(Git.Hash(hash), ref(namespace)))

  // A note read through the notes ref's own tree, which is where it survives the pruning of a
  // dangling target: `git notes` fans out its tree past a size, so both layouts are tried.
  def storedNote(namespace: Text, hash: Text)(using WorkingDirectory): Optional[Text] =
    note(namespace, hash).or:
      val flat   = t"refs/notes/$namespace:$hash"
      val fanned = t"refs/notes/$namespace:${hash.keep(2)}/${hash.skip(2)}"
      safely(sh"git -C $toplevel cat-file -p $flat".exec[Text]())
      . or(safely(sh"git -C $toplevel cat-file -p $fanned".exec[Text]()))

  def addNote(namespace: Text, hash: Text, body: Text, force: Boolean)(using WorkingDirectory): Boolean =
    safely(repo.notes.add(Git.Hash(hash), body, force, ref(namespace))).present

  def appendNote(namespace: Text, hash: Text, body: Text)(using WorkingDirectory): Boolean =
    safely(repo.notes.append(Git.Hash(hash), body, ref(namespace))).present

object Repository:
  // The repository containing `root`, or `Unset` when it is not inside a git work tree.
  def locate(root: Text)(using WorkingDirectory): Optional[Repository] =
    safely:
      val toplevel = sh"git -C $root rev-parse --show-toplevel".exec[Text]().trim
      val gitDir   = sh"git -C $root rev-parse --absolute-git-dir".exec[Text]().trim
      if toplevel == t"" || gitDir == t"" then Unset else Repository(toplevel, gitDir)
