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

// The nine principles from which every Consequent rule derives. Part I of
// doc/consequent-style.md states each principle and its readability
// motivation; every rule in `Rules.all` cites the principle it derives
// from. Eight are about how code reads; the ninth, Soundness, is about what
// it claims, and is the one whose rules a project must configure to enable.
enum Principle:
  case Frame, Anchoring, Density, ContinuationMarking, Balance, Proximity, Tabulation,
    Locatability, Soundness

// One house-style rule: an SN identifier (a family such as `A2` may span
// several sub-rules emitted with suffixed identifiers), the principle it
// derives from, and a check over the per-file `Context`. A rule must not
// throw on malformed input: extraction failures surface as an absence of
// data, never as an exception.
trait Rule:
  def id: String
  def principle: Principle
  def check(ctx: Context): List[Violation]

object Rules:
  val all: List[Rule] =
    List
      ( // RawTabs (F5) leads the registry: the old raw pre-tokenizer scan
        // ran in the preamble of `check`, before every other rule, so any
        // positional collision with a later rule must keep resolving in its
        // favour. (No such collision exists in the corpus — the tree is
        // tab-free — but the historical order is preserved regardless.)
        FrameRules.RawTabs,
        FrameRules.LicenceFrame, FrameRules.PackageDeclaration, FrameRules.PackageBlank,
        FrameRules.ImportSeparation, FrameRules.ImportOrdering,
        AnchorRules.SequenceLayout, AnchorRules.DefinitionAnchors,
        // OperatorContinuation must precede ContinuationIndent: both can fire
        // at the same position (a continuation line led by an operator), and
        // dotty's reporter keeps only the first diagnostic per position — C2
        // is the more specific message there.
        ContinuationRules.OperatorContinuation,
        AnchorRules.BodyScopeIndent, AnchorRules.ContinuationIndent,
        AnchorRules.SignatureEqLast,
        AnchorRules.InterpolationLayout, TabulationRules.CaseAlignment,
        TabulationRules.ForComprehensionAlignment, DensityRules.LambdaLayout,
        ProximityRules.ChunkSeparation,
        LocatabilityRules.FileNaming, LocatabilityRules.CompanionOrdering,
        LocatabilityRules.UmbrellaExportCompleteness,
        LocatabilityRules.ExtensionExportCompleteness,
        // LineLength (F4) fired at the top of the old per-line walk, before
        // the quote/splice family — it must keep winning their one positional
        // collision (a 101-column line inside an inline splice).
        FrameRules.LineLength,
        // The former per-line-walk cluster, in its old within-line order:
        // F6, F7, P3, then the `checkTokens` family (F8, B1, B2,
        // B3, in `checkTokens` call order), then P1. All of these fired
        // after LineLength (F4) at the top of `checkLine` and before the
        // C3/C1/A7/P4 block below. Two placement constraints carry
        // evidence from the corpus:
        //  - IndentWidth (F6) must follow ProximityRules.ChunkSeparation:
        //    both fire at monotonous.Alphabet:186:1, and P2 is the
        //    surviving diagnostic in the baseline compile, so P2 must keep
        //    winning — dotty's reporter keeps only the first diagnostic per
        //    position.
        //  - No other positional collision involves F6, F7, P3, F8,
        //    B1, B2, B3 or P1 anywhere in the corpus (checked over
        //    the full parse-path oracle), so the remaining placements
        //    simply preserve the old walk's within-line order.
        FrameRules.IndentWidth, FrameRules.TrailingWhitespace,
        ProximityRules.BlankLineRun, FrameRules.CommentShape,
        BalanceRules.OperatorSpacing, BalanceRules.AssignmentSpacing,
        BalanceRules.SymbolicMethodNames, ProximityRules.CommaSpacing,
        // These four fired towards the end of the old per-line walk (in this
        // order: C3, C1, A7, P4), after LineLength and before the
        // quote/splice family, so they sit between those two here. No
        // positional collision with any other rule exists in the corpus, but
        // the relative order is preserved regardless.
        ContinuationRules.HardSpace, ContinuationRules.ChainContinuation,
        AnchorRules.GivenArrowAlign, ProximityRules.ReturnTypeBlank,
        // FormalBlockSpacing (B4) must follow FrameRules.LineLength: both
        // fire at turbulence.Benchmarks:523:101, and F4 fired first in the
        // old walk (LineLength ran at the top of checkLine, before the
        // bracket-interior checks), so F4 must keep winning there.
        // HeavyBracketAnchor (A4) must follow AnchorRules
        // .ContinuationIndent: both fire at jacinta.stagedInternal:621:21
        // and monotonous.Alphabet:287:25, and A1 won in the baseline
        // compile (A1 is the surviving diagnostic at monotonous
        // .Alphabet:287:25), so it must keep firing first. No other
        // positional collision involves B4, B5, T2 or A4 in the
        // corpus.
        BalanceRules.FormalBlockSpacing, BalanceRules.CompactBracketSpacing,
        TabulationRules.UsingAlignment, AnchorRules.HeavyBracketAnchor,
        // AnnotationAdjacency (P6) fired at the very bottom of the old
        // `checkLine` (the P6.2 blank-line check and the end-of-file P6.1
        // flush both ran after every other per-line check), so it sits after
        // the whole walk-era cluster; no positional collision involves P6
        // in the corpus.
        ProximityRules.AnnotationAdjacency,
        // QuoteSpliceLayout stays last: the A9.1–.7 family used to fire at
        // the end of the per-line walk, after every registry rule, so any
        // positional collision with an earlier rule (e.g. C2.1 with A9.4)
        // must keep resolving in the earlier rule's favour — dotty's
        // reporter keeps only the first diagnostic per position.
        QuoteRules.QuoteSpliceLayout,
        // UnnecessaryBreak (D1) is a new rule, not a port of the per-line
        // walk, so it carries no historical ordering evidence — it sits
        // last so that any positional collision with an established rule
        // resolves in the established rule's favour. The corpus has one
        // such collision: D1 and A9.4 both fire at stratiform
        // .bintelInternal:633:13, and A9.4 (the established diagnostic)
        // must keep winning — dotty's reporter keeps only the first
        // diagnostic per position.
        DensityRules.UnnecessaryBreak,
        // UnsafeNaming (S1) is likewise a new rule with no historical
        // ordering evidence, and reports on a definition's name — a position
        // no layout rule occupies — so it sits last and yields to any
        // established rule it should ever collide with.
        SoundnessRules.UnsafeNaming )
