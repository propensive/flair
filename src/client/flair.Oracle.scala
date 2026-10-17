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
┃   Flair, version 0.2.1.                                                                          ┃
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

import scala.caps

import soundness.*
import dysasymptotics.linearSize
import errorDiagnostics.stackTracesDiagnostics
import httpBackends.javaNetHttp
import internetAccess.online
import logging.silentLogging

// The model consulted by `flair assess`: given batches of candidates and the rubric, it answers
// each criterion per candidate. The answer is a plain type — `Answer` — that sibylline derives
// the schema for and marshals the reply into; flair never sees a schema. Behind the trait is
// Anthropic through sibylline, in `batches` mode (the Message Batches API, asynchronous and
// half-price) or `realtime` mode (one call per batch); a test substitutes the trait.
object Oracle:
  // Sealed as sibylline seals its own derived decoders: a derived decodable takes the tactic
  // both directly and inside its field thunks, which capture checking reads as overlapping uses
  // of one capability. One per record, so each level derives against a sealed instance.
  object Sign:
    given decodable: Tactic[Json.Error] => Sign is Json.Decodable =
      caps.unsafe.unsafeAssumeSeparate(Json.DecodableDerivation.derived[Sign])

  case class Sign(id: Text, value: Boolean)

  object Verdict:
    given decodable: Tactic[Json.Error] => Verdict is Json.Decodable =
      caps.unsafe.unsafeAssumeSeparate(Json.DecodableDerivation.derived[Verdict])

  case class Verdict(id: Text, signs: List[Sign], replacement: Text, reason: Text)

  object Answer:
    given decodable: Tactic[Json.Error] => Answer is Json.Decodable =
      caps.unsafe.unsafeAssumeSeparate(Json.DecodableDerivation.derived[Answer])

  case class Answer(verdicts: List[Verdict])

  // One batch of candidates put to the model, and what came back: an answer, or the error that
  // stands in for one.
  case class Outcome(batch: List[Candidates.Candidate], result: Answer | Llm.Error)

  trait Oracle:
    def judge(batches: List[List[Candidates.Candidate]]): List[Outcome]
    def usage: Llm.Usage

  // The prompt for one batch: what to do, then each candidate under its digest — the key its
  // judgement is stored by, which the model echoes back as the verdict's `id`. Nothing about
  // position or other files enters it, so a judgement depends only on the definition and the
  // rubric.
  def prompt(batch: List[Candidates.Candidate], digests: Text -> Text, criteria: List[Text])
  :   Text =

    val header: Text =
      t"Score these ${batch.size.show} candidates. Answer every one of the criteria " +
        t"${criteria.join(t", ")} for each, as `true` or `false`, and echo each candidate's id."

    val entries: List[Text] = batch.map: (candidate: Candidates.Candidate) =>
      val id = digests(candidate.normalised.tt)
      val kind = candidate.kind.word.tt
      val calls = candidate.calls.show
      val excerpt = candidate.excerpt.tt
      t"### $id\nkind: $kind; call sites in scope: $calls\n```scala\n$excerpt\n```"

    (List(header) + entries).join(t"\n\n")

  // A batch's request identifier: the digests it contains, digested again, which fits the
  // wire's rules for an identifier and maps an outcome back to its batch without a table.
  def identifier(batch: List[Candidates.Candidate], digests: Text -> Text): Text =
    import alphabets.hexLowerCase
    import codepages.utf8Codepage
    import providers.javaBaseProvider

    val joined: Text = batch.map { candidate => digests(candidate.normalised.tt) }.join(t"\n")
    joined.digest[Sha2[256]].serialize[Hex].keep(40)

  // Anthropic, through sibylline.
  class Anthropic
    ( model:    Workspace.Model,
      key:      Text,
      rubric:   Rubric,
      criteria: List[Text],
      digests:  Text -> Text,
      interval: Int )
  extends Oracle:

    @scala.caps.unsafe.untrackedCaptures
    private var usage0: Llm.Usage = Llm.Usage(0, 0)

    def usage: Llm.Usage = usage0

    private val target: sibylline.Anthropic =
      sibylline.Anthropic(model.model, key).prompted(rubric.text).limit(model.limit).caching

    def judge(batches: List[List[Candidates.Candidate]]): List[Outcome] =
      given TlsAcceptance = TlsAcceptance()
      if model.mode == t"realtime" then realtime(batches) else batched(batches)

    // The list decoder inside `Answer` asks for a JSON tactic where the schema is derived, so
    // one is made from the LLM tactic `protect` supplies, as sibylline's dialects do.
    private def jsonTactic(using tactic: Tactic[Llm.Error]): Tactic[Json.Error]^ =
      tactic.contramap: _ =>
        Llm.Error(Llm.Error.Reason.Malformed, t"the answer did not match its schema")

    private def realtime(batches: List[List[Candidates.Candidate]])(using TlsAcceptance)
    :   List[Outcome] =

      batches.map: (batch: List[Candidates.Candidate]) =>
        // Both arms ascribed to the union: `recover` types the handler by the block, and an
        // unascribed block would make it `Answer`, casting the error at runtime.
        val result: Answer | Llm.Error =
          recover:
            case error: Llm.Error => (error: Answer | Llm.Error)

          . protect:
              given jsonTactic0: (Tactic[Json.Error]^) = jsonTactic

              target.session:
                val answer = llm.solicit[Answer](prompt(batch, digests, criteria))
                usage0 = usage0 + llm.usage
                (answer: Answer | Llm.Error)

        Outcome(batch, result)

    private def batched(batches: List[List[Candidates.Candidate]])(using TlsAcceptance)
    :   List[Outcome] =

      recover:
        case error: Llm.Error => (batches.map(Outcome(_, error)): List[Outcome])

      . protect:
          given jsonTactic0: (Tactic[Json.Error]^) = jsonTactic

          val requests: List[Llm.Request] = batches.map: (batch: List[Candidates.Candidate]) =>
            val id = Llm.Id.request(identifier(batch, digests))
            Llm.Request(id, prompt(batch, digests, criteria))

          val submitted = target.elicitAll[Answer](requests)
          val ended = submitted.await(interval)
          val outcomes = ended.outcomes()

          batches.map: (batch: List[Candidates.Candidate]) =>
            val id = identifier(batch, digests)

            val result: Answer | Llm.Error =
              outcomes.filter(_.id.text == id).prim.let(_.result).or:
                Llm.Error(Llm.Error.Reason.Malformed, t"the batch returned no outcome for $id")

            Outcome(batch, result)
          . pipe { (list: List[Outcome]) => list }
