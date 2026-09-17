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

import scala.collection.immutable as sci

import dotty.tools.dotc.{Compiler, Driver}
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.interfaces
import dotty.tools.dotc.reporting.{Diagnostic as CompilerDiagnostic, HideNonSensicalMessages, Reporter, UniqueMessagePositions}
import dotty.tools.dotc.util.SourceFile
import dotty.tools.io.AbstractFile

// One run of the Scala compiler over a profile's sources, stopped after the last phase any
// enabled rule needs — the parser, today — so that a lint costs a parse and not a compile, and
// needs no classpath. What comes back is each unit's untyped tree, which is what the rules read,
// plus whatever the compiler reported on the way: a file that does not parse is a finding too.
object Frontend:
  case class Diagnostic(path: Text, line: Int, column: Int, message: Text, error: Boolean)
  case class Parsed(path: Text, text: Text, tree: untpd.Tree, source: SourceFile)
  case class Result(units: List[Parsed], diagnostics: List[Diagnostic])

  // The classpath the compiler initialises against: the jars the running application was loaded
  // from. Under the Burdock/xeq launcher those are the externalized dependency jars fetched into
  // `~/.cache/burdock` plus the executable itself — a shebang-prefixed file with no `.jar` suffix
  // that the compiler refuses to read (a `ZipFile` still reads the appended archive past the
  // shebang), so it is symlinked to a `.jar` path and the symlink substituted. Outside the
  // launcher, `java.class.path` is what there is.
  lazy val classpath: String =
    val entries = java.util.ArrayList[String]()

    val executable: String | Null = java.lang.System.getProperty("ethereal.script")
    val executablePath: String | Null =
      if executable == null then null else java.io.File(executable).getCanonicalPath

    if executablePath != null then
      try
        val directory = java.nio.file.Files.createTempDirectory("flair").nn
        val link = directory.resolve("flair.jar").nn
        java.nio.file.Files.createSymbolicLink(link, java.nio.file.Paths.get(executablePath))
        entries.add(link.toString)
      catch case _: Throwable => ()

    Thread.currentThread.nn.getContextClassLoader match
      case loader: java.net.URLClassLoader =>
        loader.getURLs.nn.foreach: url =>
          if url != null then
            val path = java.io.File(url.toURI).getCanonicalPath.nn
            if path != executablePath then entries.add(path)
      case _ => ()

    val system: String | Null = java.lang.System.getProperty("java.class.path")
    if system != null && !system.isEmpty then entries.add(system)

    String.join(java.io.File.pathSeparator, entries).nn

  // `started` is told each phase a unit begins, so a caller can show progress.
  def run(files: List[Text], languages: List[Text], stopAfter: Text)(started: String => Unit): Result =
    val collected: scala.collection.mutable.ListBuffer[Diagnostic] =
      scala.collection.mutable.ListBuffer()

    object reporter extends Reporter, UniqueMessagePositions, HideNonSensicalMessages:
      def doReport(diagnostic: CompilerDiagnostic)(using Contexts.Context): scala.Unit =
        val pos = diagnostic.pos
        val error = diagnostic.level == interfaces.Diagnostic.ERROR
        if pos.exists then
          collected += Diagnostic(pos.source.path.tt, pos.line + 1, pos.column + 1, diagnostic.message.tt, error)
        else
          collected += Diagnostic(t"", 0, 0, diagnostic.message.tt, error)

    object driver extends Driver:
      def context: Contexts.Context =
        // The trailing empty argument stops the driver from treating an argument list with no
        // source files as a request to print usage and bail out. Even a parse needs the core
        // library on the classpath (the compiler's definitions are initialised from it), and
        // under the launcher that is not `java.class.path`, so it is named explicitly.
        val args = java.util.ArrayList[String]()
        args.add("-classpath")
        args.add(classpath)
        languages.each { feature => args.add(s"-language:${feature.s}"); () }
        args.add("")
        setup(args.toArray(new scala.Array[String | Null](0)).nn.asInstanceOf[scala.Array[String]], initCtx.fresh)
        . map(_(1)).get

    val base: Contexts.FreshContext = driver.context.fresh
    base.setReporter(reporter)
    base.setSetting(base.settings.YstopAfter, sci.List(stopAfter.s))

    base.setProgressCallback(new dotty.tools.dotc.sbt.interfaces.ProgressCallback:
      override def informUnitStarting(phase: String | Null, unit: dotty.tools.dotc.CompilationUnit | Null): scala.Unit =
        if phase != null then started(phase))
    given context: Contexts.Context = base

    val sources: sci.List[SourceFile] =
      files.map { file => SourceFile(AbstractFile.getFile(file.s).nn, scala.io.Codec.UTF8) }.stdlib

    val run = new Compiler().newRun
    run.compileSources(sources)

    val units: List[Parsed] =
      List.from:
        run.units.map: unit =>
          Parsed(unit.source.path.tt, String(unit.source.content).tt, unit.untpdTree, unit.source)

    Result(units, List.from(collected.toList))
