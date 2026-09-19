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

import java.util.concurrent as juc

import soundness.*
import dysasymptotics.linearSize

import pyrocosm.{Action, Block, Control, Event, Hints, Inline, Interface, Live, Panel, Tone, Tool, hints}
import pyrocosm.Status as Gauge

// The dashboard as the web front-end `Tool` serves from the daemon: launched once, for as long
// as the daemon lives, when a config says `serve` (on its `port`, or 8070), and stopped by
// `flair quit`. `flair serve` remains the interactive way to serve it, from a terminal.
object Dashboard:
  val web: Tool.Web = new Tool.Web:
    def port: Int = 8070

    // The frontend holds the monitor and the error page, which outlive it; vouched pure so it
    // can be stopped from another invocation.
    @scala.caps.unsafe.untrackedCaptures
    @volatile
    private var frontend: Optional[pyrocosm.WebFrontend] = Unset

    // Serves until `stop`. A ticker drives the dashboard a few times a second for as long as
    // the front-end runs: it is what notices a project newly registered, runs a requested
    // check, and reads a selected profile's census; nothing runs on the front-end's threads.
    def serve(port: Int)(using Monitor, Probate): Unit =
      import webserverErrorPages.minimalErrorPage

      val dashboard = Dashboard()
      val serving: juc.atomic.AtomicBoolean = juc.atomic.AtomicBoolean(true)

      val running: pyrocosm.WebFrontend =
        scala.caps.unsafe.unsafeAssumePure(pyrocosm.WebFrontend(port))

      frontend = running

      try
        async:
          while serving.get do
            dashboard.tick()
            snooze(0.25*Second)

        running.run(dashboard.interface)(dashboard.handle)
      finally serving.set(false)

    def stop(): Unit = frontend.let(_.stop())

  // The projects the daemon has seen, by root: every invocation registers the configuration it
  // loads, so the dashboard offers each project's profiles whichever shell flair was run from,
  // and a configuration edited since is replaced by the next invocation, or by a check.
  private val projects: juc.ConcurrentHashMap[Text, Workspace.Config] = juc.ConcurrentHashMap()

  def register(config: Workspace.Config): Unit = projects.put(config.root, config)

  // Bumped whenever a census may have been recorded — by `flair metrics`, in any shell — so
  // that a dashboard showing that profile reads it again.
  private val edition0: juc.atomic.AtomicLong = juc.atomic.AtomicLong(0L)
  def edition: Long = edition0.get
  def measured(): Unit = edition0.incrementAndGet()

  def known: List[Workspace.Config] =
    val iterator = projects.values.nn.iterator.nn

    def recur(acc: List[Workspace.Config]): List[Workspace.Config] =
      if iterator.hasNext then recur(acc + List(iterator.next.nn)) else acc

    List.from(recur(Nil).stdlib.sortBy(_.root.s))

// The dashboard: every known project's profiles to choose from; the chosen profile's latest
// check — its findings as they arrive, then filtered by rule — and its summary: the rules that
// fired, and the census `flair metrics` has recorded for it, commit by commit.
final class Dashboard():
  // A profile in a project, as the navigation offers it.
  private case class Target(root: Text, profile: Text)

  private case class Checked
    ( config: Workspace.Config, profile: Workspace.Profile, outcome: Checking.Outcome, finished: Long )

  private val profileActions: juc.ConcurrentHashMap[Target, Action] = juc.ConcurrentHashMap()
  private val ruleActions: juc.ConcurrentHashMap[Text, Action] = juc.ConcurrentHashMap()
  private val checkAction: Action = Action(t"check")

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var selected: Optional[Target] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var filter: Optional[Text] = Unset

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var requested: Boolean = false

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var running: Boolean = false

  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var checked: Optional[Checked] = Unset

  // The census read for the selected profile, so that git is asked once per selection, and
  // again only when a measurement may have been recorded since.
  @scala.caps.unsafe.untrackedCaptures
  @volatile
  private var census: Optional[(Target, Long, List[Census.Record])] = Unset

  @scala.caps.unsafe.untrackedCaptures
  private var lastNavigation: List[Block] = Nil

  @scala.caps.unsafe.untrackedCaptures
  private var lastSummary: List[Block] = Nil

  val navigation: Live[List[Block]] = Live(Nil)
  val findings: Live[List[Block]] = Live(List(Block.paragraph(t"Choose a profile and press Check.")))
  val status: Live[List[Block]] = Live(Nil)
  val summary: Live[List[Block]] = Live(Nil)
  val enabled: Live[Boolean] = Live(true)

  val interface: Interface =
    Interface
      ( Inline.text(t"flair"),
        List
          ( Panel(Panel.Id(t"profiles"), Panel.Role.Navigation, Inline.text(t"Profiles"), navigation, Panel.Priority.Important),
            Panel(Panel.Id(t"findings"), Panel.Role.Primary, Unset, findings, Panel.Priority.Essential, hints = Hints(hints.Follow)),
            Panel(Panel.Id(t"status"), Panel.Role.Status, Unset, status, Panel.Priority.Important),
            Panel(Panel.Id(t"summary"), Panel.Role.Primary, Inline.text(t"Summary"), summary, Panel.Priority.Important) ),
        controls = List(Control.Button(Inline.text(t"Check"), checkAction, enabled)) )

  private def profileAction(target: Target): Action =
    profileActions.computeIfAbsent(target, _ => Action(t"${target.root}:${target.profile}")).nn

  private def ruleAction(rule: Text): Action = ruleActions.computeIfAbsent(rule, _ => Action(rule)).nn

  private def keyOf[key](map: juc.ConcurrentHashMap[key, Action], action: Action): Optional[key] =
    val entries = map.entrySet.nn.iterator.nn

    def recur(): Optional[key] =
      if !entries.hasNext then Unset else
        val entry = entries.next.nn
        if entry.getValue == action then entry.getKey.nn else recur()

    recur()

  def handle(event: Event): Unit = event match
    case Event.Pressed(action) if action == checkAction =>
      if !running then requested = true

    case Event.Pressed(action) =>
      keyOf(profileActions, action) match
        case target: Target =>
          selected = target
          filter = Unset
          showFindings()
          refresh()

        case _ =>
          keyOf(ruleActions, action).let: (rule: Text) =>
            filter = if filter == rule then Unset else rule
            showFindings()
            refresh()

    case _ =>
      ()

  // The serving loop's beat: a selection where there is none yet, a requested check, the
  // selection's census, and whatever the navigation and summary must now show.
  def tick(): Unit =
    if selected.absent then
      Dashboard.known.prim.let: (config: Workspace.Config) =>
        config.profiles.prim.let { (profile: Workspace.Profile) => selected = Target(config.root, profile.name) }

    if requested && !running then
      requested = false
      selected.let(check(_))

    selected.let: (target: Target) =>
      val edition: Long = Dashboard.edition
      val current: Boolean = census.lay(false) { (entry: (Target, Long, List[Census.Record])) => entry(0) == target && entry(1) == edition }
      if !current then census = (target, edition, history(target))

    refresh()

  // Rebuilds the navigation and the summary, assigning each only when it has changed, so a
  // quiet dashboard sends nothing.
  private def refresh(): Unit = synchronized:
    val navigation0 = navigationBlocks()

    if navigation0 != lastNavigation then
      lastNavigation = navigation0
      navigation() = navigation0

    val summary0 = summaryBlocks()

    if summary0 != lastSummary then
      lastSummary = summary0
      summary() = summary0

  private def failure(message: Text): List[Block] =
    List(Block.Paragraph(List(Inline.Toned(Tone.Failure, Inline.text(message)))))

  // Checks the profile, the findings scrolling into the primary panel as they are found under
  // a gauge in the status panel, then shows the whole outcome. The configuration is reloaded
  // for the check, so an edit since the project was registered is honoured.
  private def check(target: Target): Unit =
    running = true
    enabled() = false
    filter = Unset
    findings() = Nil
    status() = List(Block.Gauge(Gauge.Indeterminate(), Inline.text(t"Starting")))

    try
      Workspace.load(target.root) match
        case Workspace.Outcome.Loaded(config) =>
          Dashboard.register(config)

          config.profile(target.profile) match
            case profile: Workspace.Profile =>
              val plugin = config.pluginConfig(profile)

              if !plugin.errors.nil then
                status() =
                  List(Block.Notice(Tone.Failure, Inline.text(t"Profile `${profile.name}` could not be used"),
                      plugin.errors.map(Block.paragraph)))
              else
                val files = safely(Sources.expand(config.root, profile, Nil)).or(Nil)

                val sink: Checking.Sink = new Checking.Sink:
                  def aborted: Boolean = false
                  def gauge(status0: Gauge, caption: Text): Unit = status() = List(Block.Gauge(status0, Inline.text(caption)))

                  def found(finding: Report.Finding, text: Text): Unit =
                    findings.append(Report.excerpt(config.root, finding, text))

                val outcome = Checking.run(profile, plugin.config, files, sink)
                checked = Checked(config, profile, outcome, java.lang.System.currentTimeMillis)
                showFindings()
                showStatus()

            case _ =>
              status() = failure(t"The profile `${target.profile}` is no longer defined in ${target.root}")

        case Workspace.Outcome.Missing =>
          status() = failure(t"No .pyrocosm/flair/config.tel was found at ${target.root}")

        case Workspace.Outcome.Invalid(file, errors) =>
          status() = List(Block.Notice(Tone.Failure, Inline.text(t"$file could not be used"), errors.map(Block.paragraph)))
    catch case error: Exception =>
      status() = failure(t"The check failed: ${Optional(error.getMessage).let(_.tt).or(error.getClass.getName.nn.tt)}")
    finally
      running = false
      enabled() = true

  // The findings of the latest check, every one or those under the filtering rule.
  private def showFindings(): Unit = checked.let: (last: Checked) =>
    val texts: Map[Text, Text] = last.outcome.texts
    val all: List[Report.Finding] = last.outcome.all
    val shown: List[Report.Finding] = filter.lay(all) { (rule: Text) => all.filter(_.rule == rule) }

    findings() =
      if !shown.nil then
        shown.map { (finding: Report.Finding) => Report.excerpt(last.config.root, finding, texts.at(finding.path).or(t"")) }
      else
        val message: Text =
          filter.lay(t"No violations in ${last.outcome.files.show} files") { (rule: Text) => t"Nothing under the rule $rule" }

        List(Block.Paragraph(List(Inline.Toned(Tone.Success, Inline.text(message)))))

  // The outcome in a line: how many findings, in how many files, of which profile, when.
  private def showStatus(): Unit = checked.let: (last: Checked) =>
    val all: List[Report.Finding] = last.outcome.all
    val tone: Tone = if last.outcome.errors then Tone.Failure else if all.nil then Tone.Success else Tone.Warning
    val verdict: Text = if all.nil then t"No violations" else if last.outcome.errors then t"Errors" else t"Warnings"

    status() =
      List
        ( Block.Paragraph
            ( List
                ( Inline.Toned(tone, Inline.text(verdict)),
                  Inline.Textual(t"  "),
                  Inline.Figure(all.size.toDouble, 0),
                  Inline.Textual(t" findings in "),
                  Inline.Figure(last.outcome.files.toDouble, 0),
                  Inline.Textual(t" files · ${last.profile.name} · ${when(last.finished)}") ) ) )

  private def history(target: Target): List[Census.Record] =
    given WorkingDirectory = () => target.root

    Dashboard.known.seek(_.root == target.root).lay(Nil: List[Census.Record]): (config: Workspace.Config) =>
      config.profile(target.profile).lay(Nil: List[Census.Record]): (profile: Workspace.Profile) =>
        Repository.locate(config.root).lay(Nil: List[Census.Record]): (repository: Repository) =>
          Census.history(repository, profile.notes.or(t"flair"), profile.name, 200)

  // When something happened: relative to now within the hour, and as a time of day beyond it.
  private def when(millis: Long): Text =
    val minutes: Long = (java.lang.System.currentTimeMillis - millis)/60000L

    if minutes < 1L then t"just now"
    else if minutes == 1L then t"1 minute ago"
    else if minutes < 60L then t"${minutes.show} minutes ago"
    else
      val instant = java.time.Instant.ofEpochMilli(millis).nn
      val zoned = instant.atZone(java.time.ZoneId.systemDefault).nn
      java.time.format.DateTimeFormatter.ofPattern("HH:mm").nn.format(zoned).nn.tt

  // When a commit was made: as `when` within the day, and as a date beyond it.
  private def dated(millis: Long): Text =
    val now = java.time.ZonedDateTime.now(java.time.ZoneId.systemDefault).nn
    val zoned = java.time.Instant.ofEpochMilli(millis).nn.atZone(java.time.ZoneId.systemDefault).nn

    if zoned.toLocalDate == now.toLocalDate then when(millis)
    else
      val pattern: String = if zoned.getYear == now.getYear then "d MMM" else "d MMM yyyy"
      java.time.format.DateTimeFormatter.ofPattern(pattern).nn.format(zoned).nn.tt

  private def marked(chosen: Boolean, label: Text): List[Inline] =
    if chosen then List(Inline.Toned(Tone.Accent, List(Inline.Emphasis(Inline.text(label))))) else Inline.text(label)

  // Every project's profiles, the selected one marked, then the rules the latest check
  // found, each with its count, the filtering one marked.
  private def navigationBlocks(): List[Block] =
    val known: List[Workspace.Config] = Dashboard.known

    val projects: List[Block] =
      if known.nil then List(Block.paragraph(t"No project yet: run flair in one, and its profiles appear here."))
      else known.bind[List[Block], Block, List[Block]]: (config: Workspace.Config) =>
        val name: Text = Text(config.root.s.substring(config.root.s.lastIndexOf('/') + 1).nn)

        val items: List[Block.Item] =
          config.profiles.map: (profile: Workspace.Profile) =>
            val target = Target(config.root, profile.name)
            Block.Item(List(Block.Paragraph(marked(selected == target, profile.name))), profileAction(target))

        List(Block.Heading(3, Inline.text(name)), Block.Listing(false, items))

    val rules: List[Block] = checked.lay(Nil: List[Block]): (last: Checked) =>
      val all: List[Report.Finding] = last.outcome.all
      val counts: List[(Text, Int)] = List.from(all.map(_.rule).distinct.map { (rule: Text) => (rule, all.filter(_.rule == rule).size) }.stdlib.sortBy(-_(1)))

      if counts.nil then Nil else
        val items: List[Block.Item] =
          counts.map: (rule: Text, count: Int) =>
            val label: List[Inline] =
              marked(filter == rule, rule) + List(Inline.Textual(t" "), Inline.Toned(Tone.Muted, List(Inline.Figure(count.toDouble, 0))))

            Block.Item(List(Block.Paragraph(label)), ruleAction(rule))

        List(Block.Heading(3, Inline.text(t"Rules")), Block.Listing(false, items))

    projects + rules

  // The latest check's summary — the rules that fired, tabled and charted — and the census
  // recorded for the selected profile.
  private def summaryBlocks(): List[Block] =
    val report: List[Block] = checked.lay(Nil: List[Block]): (last: Checked) =>
      val all: List[Report.Finding] = last.outcome.all
      val rules: List[Text] = List.from(all.map(_.rule).distinct.stdlib.sortBy { (rule: Text) => -all.filter(_.rule == rule).size })

      // The findings in the rules' order, so that the table and the chart both lead with
      // the rule that fired most, as the navigation does.
      val ordered: List[Report.Finding] =
        rules.bind[List[Report.Finding], Report.Finding, List[Report.Finding]] { (rule: Text) => all.filter(_.rule == rule) }

      val chart: List[Block] =
        if rules.nil then Nil else
          val series: List[Block.Series] =
            rules.map: (rule: Text) =>
              val hits = all.filter(_.rule == rule)
              val tone = if hits.exists(_.error) then Tone.Failure else Tone.Warning
              Block.Series(Inline.text(rule), List(hits.size.toDouble), tone)

          List(Block.Chart(Block.Chart.Kind.Bars, series))

      Report.summary(ordered, last.outcome.files) :: chart

    val measured: List[Block] = selected.lay(Nil: List[Block]): (target: Target) =>
      val records: List[Census.Record] =
        census.lay(Nil: List[Census.Record]) { (entry: (Target, Long, List[Census.Record])) => if entry(0) == target then entry(2) else Nil }
      Block.Heading(3, Inline.text(t"Census")) :: censusBlocks(target, records)

    report + measured

  // The census, oldest first, as a sparkline per indicator and a table of every measurement,
  // newest first.
  private def censusBlocks(target: Target, records: List[Census.Record]): List[Block] =
    if records.nil then
      List
        ( Block.Paragraph
            ( List
                ( Inline.Toned(Tone.Muted, Inline.text(t"No census is recorded for ${target.profile}; ")),
                  Inline.Reference(t"flair metrics"),
                  Inline.Toned(Tone.Muted, Inline.text(t" records one.")) ) ) )
    else
      val indicators: List[Text] =
        List.from(records.bind[List[Text], Text, List[Text]](_.totals.map(_(0))).distinct.stdlib.sortBy(_.s))

      def total(record: Census.Record, indicator: Text): Int =
        record.totals.seek(_(0) == indicator).lay(0)(_(1))

      val chart: Block =
        Block.Chart
          ( Block.Chart.Kind.Sparkline,
            indicators.map: (indicator: Text) =>
              Block.Series(Inline.text(indicator), records.map { (record: Census.Record) => total(record, indicator).toDouble }) )

      val columns: List[Block.Column] =
        List(Block.Column(Inline.text(t"Commit")), Block.Column(Inline.text(t"Committed")))
        + indicators.map { (indicator: Text) => Block.Column(Inline.text(indicator), Block.Alignment.End, numeric = true) }

      val newestFirst: List[Census.Record] = List.from(records.stdlib.reverse)

      val rows: List[Block.Row] =
        newestFirst.map: (record: Census.Record) =>
          Block.Row
            ( List
                ( Block.Cell(List(Inline.Reference(record.commit.keep(7)))),
                  Block.Cell(Inline.text(dated(record.time*1000L))) )
              + indicators.map { (indicator: Text) => Block.Cell(List(Inline.Figure(total(record, indicator).toDouble, 0))) } )

      val caption: List[Inline] =
        if records.size == 1 then Inline.text(t"1 measurement over the latest 200 commits")
        else Inline.text(t"${records.size.show} measurements over the latest 200 commits")

      List(chart, Block.Table(columns, rows, caption))
