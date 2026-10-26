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

// What `flare assess` records, as BinTEL in git notes under `refs/notes/flare-assess/<rule>`:
//
//  - a JUDGEMENT on the blob of each definition's normalised text (the blob's hash is the
//    definition's digest, so the target exists the moment the digest is computed): the model's
//    answers for that definition, valid while the rule, rubric, criteria and model match;
//  - an ASSESSMENT on the input tree: every candidate's locus, the digest it had, its score and
//    band — the ranking — with the run's totals;
//  - a text-TEL POINTER appended to the HEAD commit's note, as `flare metrics` writes one.
//
// A definition's judgement therefore survives every edit around it: a line shift, a rename, a
// move to another file or a reformat keeps the digest, and only a change to its own text — or
// to the rubric, criteria or model — asks the model again. The tree notes map each locus to the
// digest it had, so a locus can be followed across commits and a changed verdict attributed
// either to the code (the digest changed) or to the rubric (its digest did).
//
// This module is compiled WITHOUT capture checking, as Pyrocosm's `remote` is: stratiform's
// derived TEL codecs expand to instances the capture checker sees as fresh capabilities where a
// pure instance is required, so their derivation sites must sit in an unchecked module. The
// client reads and writes the records through `encode` and `decode` alone.
object Verdicts:
  case class Sign(id: Text, value: Boolean)

  case class Judgement
    ( rule:        Text,
      model:       Text,
      rubric:      Text,
      criteria:    Text,
      flare:       Text,
      measured:    Text,
      digest:      Text,
      kind:        Text,
      calls:       Int,
      oneLiner:    Boolean,
      signs:       List[Sign],
      replacement: Text,
      reason:      Text,
      incomplete:  Boolean )

  case class Count(band: Text, count: Int)
  case class Usage(input: Int, output: Int, cacheRead: Int)

  case class Ranked
    ( locus:       Text,
      path:        Text,
      line:        Int,
      name:        Text,
      kind:        Text,
      digest:      Text,
      score:       Int,
      band:        Text,
      replacement: Text,
      duplicates:  Int )

  case class Assessment
    ( rule:       Text,
      model:      Text,
      rubric:     Text,
      criteria:   Text,
      flare:      Text,
      measured:   Text,
      tree:       Text,
      commit:     Text,
      dirty:      Boolean,
      files:      Int,
      candidates: Int,
      requests:   Int,
      usage:      Usage,
      bands:      List[Count],
      ranked:     List[Ranked] )

  def encode(judgement: Judgement): Data = unsafely(judgement.bintel)
  def encode(assessment: Assessment): Data = unsafely(assessment.bintel)

  def decodeJudgement(data: Data): Optional[Judgement] = safely(Bintel.read[Judgement](data))
  def decodeAssessment(data: Data): Optional[Assessment] = safely(Bintel.read[Assessment](data))
