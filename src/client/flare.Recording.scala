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

import codepages.utf8Codepage

// Where `flare assess` keeps its records (see `Verdicts` for what they are): a judgement note on
// each definition's blob, an assessment note on the input tree, and a text-TEL pointer on the
// commit, all under `refs/notes/flare-assess/<rule>`.
object Recording:
  def namespace(rule: Text): Text = t"flare-assess/$rule"

  // The pointer note appended to the commit: one `assessment` per rule assessed.
  def pointer(a: Verdicts.Assessment): Text =
    val fixed: List[Text] =
      List
        ( t"assessment", t"  rule ${a.rule}", t"  tree ${a.tree}", t"  rubric ${a.rubric}",
          t"  measured ${a.measured}" )

    val lines: List[Text] = if a.dirty then fixed + List(t"  dirty") else fixed
    lines.join(t"\n") + t"\n"

  // The tree a commit's pointer note names for a rule; `Unset` when the commit carries none.
  def treeFor(repository: Repository, rule: Text, commit: Text)(using WorkingDirectory)
  :   Optional[Text] =

    repository.note(namespace(rule), commit).let: (pointer: Text) =>
      safely(pointer.read[Tel]).let: (tel: Tel) =>
        val entries: List[Tel] = tel.fields(t"assessment").to[List]
        val ours: List[Tel] = entries.filter(_.field(t"rule").let(_.primaryAtom) == rule)
        ours.head.let(_.field(t"tree")).let(_.primaryAtom)

  // A stored judgement for a definition, if one exists and was made under the same rule,
  // rubric, criteria and model.
  def judgement
    ( repository: Repository,
      rule:       Text,
      digest:     Text,
      rubric:     Text,
      criteria:   Text,
      model:      Text )
    ( using WorkingDirectory )
  :   Optional[Verdicts.Judgement] =

    val stored: Optional[Verdicts.Judgement] =
      repository.noteData(namespace(rule), digest).let(Verdicts.decodeJudgement(_))

    stored.let: (judgement: Verdicts.Judgement) =>
      val same: Boolean =
        judgement.rubric == rubric && judgement.criteria == criteria && judgement.model == model

      if same then judgement else Unset

  def record(repository: Repository, judgement: Verdicts.Judgement)(using WorkingDirectory)
  :   Boolean =

    val data: Data = Verdicts.encode(judgement)
    repository.addBinaryNote(namespace(judgement.rule), judgement.digest, data)

  def assessment(repository: Repository, rule: Text, tree: Text)(using WorkingDirectory)
  :   Optional[Verdicts.Assessment] =

    repository.noteData(namespace(rule), tree).let(Verdicts.decodeAssessment(_))

  def record(repository: Repository, assessment: Verdicts.Assessment)(using WorkingDirectory)
  :   Boolean =

    val data: Data = Verdicts.encode(assessment)
    repository.addBinaryNote(namespace(assessment.rule), assessment.tree, data)
