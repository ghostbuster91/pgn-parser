import mill._
import mill.scalalib.publish._
import scalalib._
import mill.scalalib.scalafmt.ScalafmtModule
import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule
import $ivy.`com.goyeau::mill-scalafix:0.2.1`
import com.goyeau.mill.scalafix.ScalafixModule
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.9:0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion
import $ivy.`org.scalameta::mdoc:2.0.3`
import mill.define.{Command, Input, Sources, Target}
import os.{up, Path, PermSet, ProcessOutput, RelPath}
import scala.util.matching.Regex
import mill.api.Loose

object pgnParser extends ChessModule {
  def moduleDeps = Seq(chessModel)
  def ivyDeps = Agg(ivy"org.typelevel::cats-parse::0.2.0")
  object test extends Tests with CommonTestModule
}

object chessLib extends ChessModule {
  def moduleDeps = Seq(chessModel)

  object test extends Tests with CommonTestModule
}

object chessModel extends ChessModule {
  def ivyDeps =
    Agg(ivy"com.beachape::enumeratum::1.6.1", ivy"io.estatico::newtype::0.4.4")
}

object core extends ChessModule {
  def moduleDeps = Seq(chessModel)
}

object pgnReader extends ChessModule {
  def moduleDeps = Seq(pgnParser, core, chessLib)
  def ivyDeps = Agg(ivy"org.typelevel::cats-parse::0.2.0")

  object test extends Tests with CommonTestModule
}

object docs extends MDocModule {
  def moduleDeps = Seq(pgnReader)

  def scalaVersion = "2.13.3"
  def mdocVersion: Target[String] = "2.0.3"
  def mdocTargetDirectory: Target[Path] = millSourcePath / up
  def mdocSourceDirectory: Sources = T.sources {
    millSourcePath / 'mdoc
  }
}

trait ChessModule extends BaseModule with ChessPublishModule {
  def scalaVersion = "2.13.3"
}

trait ChessPublishModule extends PublishModule {
  def publishVersion = VcsVersion.vcsState().format()
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = Settings.pomOrg,
    url = Settings.projectUrl,
    licenses = Seq(License.`Apache-2.0`),
    versionControl =
      VersionControl.github(Settings.githubOrg, Settings.githubRepo),
    developers = Seq(
      Developer(
        "ghostbuster91",
        "Kasper Kondzielski",
        "https://github.com/ghostbuster91"
      )
    )
  )
  def sonatypeUri: String = "https://s01.oss.sonatype.org/service/local"

  def sonatypeSnapshotUri: String =
    "https://s01.oss.sonatype.org/content/repositories/snapshots"

}

trait CommonTestModule extends BaseModule with TestModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.7.7",
    ivy"com.softwaremill.diffx::diffx-utest::0.4.4",
    ivy"com.softwaremill.diffx::diffx-cats::0.4.4"
  )
  def testFrameworks = Seq("utest.runner.Framework")
}

trait BaseModule
    extends ScalaModule
    with ScalafmtModule
    with TpolecatModule
    with ScalafixModule {
  def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Xfatal-warnings")) ++ Seq(
      "-Ymacro-annotations"
    )
  }
}

object Settings {
  val pomOrg = "io.github.ghostbuster91.pgnparser"
  val githubOrg = "ghostbuster91"
  val githubRepo = "pgn-parser"
  val projectUrl = s"https://github.com/$githubOrg/$githubRepo"
}

/** Copied from https://gitlab.com/hmf/srllab/-/blob/master/build.sc
  * MDoc is a documentation tool which compiles and evaluates Scala code in
  * documentation files and provides various options for configuring how the
  * results will be displayed in the compiled documentation.
  *
  * Extending this trait declares a Scala module which compiles markdown (`md`),
  * HTML, structured (`rst`) and .txt` files in the `mdoc` folder of the
  * module with MDoc.
  *
  * By default the resulting documents are simply placed in the Mill build
  * output folder but they can be placed elsewhere by overriding the
  * [[mill.contrib.mdoc.MdocModule#tMdocTargetDirectory]] task.
  *
  * For example:
  *
  * {{{
  * // build.sc
  * import mill._, scalalib._, contrib.mdoc.__
  *
  * object example extends MDocModule {
  *   def scalaVersion = "2.12.6"
  *   def mdocVersion = "0.6.7"
  * }
  * }}}
  *
  * This defines a project with the following layout:
  *
  * {{{
  * build.sc
  * example/
  *     src/
  *     docs/mdoc/
  *     resources/
  * }}}
  *
  * In order to compile documentation we can execute the `mdoc` task in the module:
  *
  * {{{
  * sh> mill example.mdoc
  * }}}
  */
trait MDocModule extends ScalaModule {
  //override def moduleDeps = Seq(plots)

  /** The version of MDoc to use.
    */
  def mdocVersion: T[String]
  def mdocVerbose: Int = 0

  private def printVerbose[A](a: A, l: Int = 0): Unit =
    if (mdocVerbose > l) print(a)
  private def printlnVerbose[A](a: A, l: Int = 0): Unit =
    if (mdocVerbose > l) println(a)

  /** A task which determines how to fetch the MDoc jar file and all of the
    * dependencies required to compile documentation for the module and
    * returns the resulting files.
    */
  def mdocIvyDeps: T[Agg[PathRef]] = T {
    Lib.resolveDependencies(
      //repositories :+ MavenRepository(s"https://dl.bintray.com/tpolecat/maven"),
      repositories,
      Lib.depToDependency(_, scalaVersion()),
      compileIvyDeps() ++ transitiveIvyDeps() ++ Seq(
        ivy"org.scalameta::mdoc:${mdocVersion()}"
      )
    )
  }

  /** A task which determines the scalac plugins which will be used when
    * compiling code examples with MDoc. The default is to use the
    * [[mill.contrib.mdoc.MDocModule#scalacPluginIvyDeps]] for the module.
    */
  def mdocScalacPluginIvyDeps: T[Agg[Dep]] = scalacPluginIvyDeps()

  /** This task determines where documentation files must be placed in order to
    * be compiled with MDoc. By default this is the `mdoc` folder at the root
    * of the module.
    */
  def mdocSourceDirectory: Sources = T.sources {
    millSourcePath / 'docs / 'mdoc
  }

  /** A task which determines where the compiled documentation files will be
    * placed. By default this is simply the Mill build's output folder for this
    * task, but this can be reconfigured so that documentation goes to the root
    * of the module (e.g. `millSourcePath`) or to a dedicated folder (e.g.
    * `millSourcePath / 'docs`)
    */
  def mdocTargetDirectory: T[os.Path] = T(T.ctx().dest)

  /** A [[scala.util.matching.Regex]] task which will be used to determine
    * which files should be compiled with MDoc. The default pattern is as
    * follows: `.*\.(md|markdown|txt|htm|html)`.
    */
  def mdocNameFilter: T[Regex] = T {
    """.*\.(md|markdown|rst|txt|htm|html)""".r
  }

  /** A task which determines what classpath is used when compiling
    * documentation. By default this is configured to use the same inputs as
    * the [[mill.contrib.mdoc.MDocModule#runClasspath]], except for using
    * [[mill.contrib.mdoc.MDocModule#mdocIvyDeps]] rather than the module's
    * [[mill.contrib.mdoc.MDocModule#runIvyDeps]].
    */
  def mdocClasspath: T[Agg[PathRef]] = T {
    // Same as runClasspath but with mdoc added to ivyDeps from the start
    // This prevents duplicate, differently versioned copies of scala-library
    // ending up on the classpath which can happen when resolving separately
    transitiveLocalClasspath() ++
      resources() ++
      localClasspath() ++
      unmanagedClasspath() ++
      mdocIvyDeps()
  }

  /** The CLI options for the MDoc processor.
    * @see https://scalameta.org/mdoc/
    */
  def mdocOpts: T[Seq[String]] = T(List[String]())

  /** The scalac options which will be used when compiling code examples with
    * MDoc. The default is to use the [[mill.contrib.mdoc.MDoc#scalacOptions]]
    * for the module, but filtering out options which are problematic in the
    * REPL, e.g. `-Xfatal-warnings`, `-Ywarn-unused-imports`.
    */
  def mdocScalacOptions: T[Seq[String]] =
    scalaDocOptions() // ++ List("-Ylog-classpath")
  /*
    scalacOptions().filterNot(Set(
      "-Ywarn-unused:imports",
      "-Ywarn-unused-import",
      "-Ywarn-dead-code",
      "-Xfatal-warnings"
    ))*/

  /** A task which performs the dependency resolution for the scalac plugins to
    * be used with MDoc.
    */
  def mdocPluginJars: T[Agg[PathRef]] = resolveDeps(mdocScalacPluginIvyDeps)()

  /** This task is used to collect the `mdocs` project paths to its resource
    * and its compiled classes. This was created so that MDoc's post modifier
    * can be implemented and compiled before the mdoc tasks are called.
    *
    * This should optimally be defined and obtained via the Mill (Ammonite)
    * script, however there does not seem to be a way to compile a class in
    * a Mill script to a file and then obtain the path to this class.
    *
    * The hack is to make mdocs a dependency of the sdk module. Once that has
    * been compiled, we can get the resources and class destination paths
    * and add that to the MDoc command line.
    *
    * **NOTE:** this is ony required when using the embedded version of MDoc.
    * In the case of the spawning MDoc the mdocs path seems to be already
    * accessible.
    *
    * @see https://scalameta.org/mdoc/docs/modifiers.html#postmodifier
    *
    * @return
    */
  def mdDocPostModifierPaths: Target[Seq[PathRef]] = T {
    val postModifier: PathRef = compile().classes
    val postModifierResources = resources()
    printlnVerbose(
      s"MDoc postModifier = ${postModifier.path.toIO.getAbsolutePath}"
    )

    val resource = postModifierResources
      .map(_.path.toIO.getAbsolutePath)
      .mkString(java.io.File.pathSeparator)
    printlnVerbose(s"MDoc postModifierResources = $resource")

    postModifierResources ++ List(postModifier)
  }

  import upickle.default
  import upickle.default.{macroRW, ReadWriter => RW}
  import ujson.{IncompleteParseException, ParseException, Readable}
  import ujson.{BytesRenderer, StringRenderer, Value}
  import upickle.core.{NoOpVisitor, Visitor}
  import upickle.default._

  case class mDocCtx(
      // Resource path the MDoc PostModifiers
      postModifierPaths: Seq[PathRef],
      // Classpath to MDoc's libraries
      paths: Agg[PathRef],
      // Classpath to MDoc's libraries (string to pass as JVM parameter)
      libPaths: String,
      // Source path to documents
      in: Path,
      // Destination path were the processed files are placed
      out: Path,
      // Regular expression of includd files (not used)
      re: List[String],
      // Scala compiler parameters used by MDoc
      scalaOpts: List[String],
      // Options used ny MDoc (see MDoc documentation on command line)
      mdocOpt: Seq[String],
      // Plug-in options - not used
      pOpts: mill.api.Loose.Agg[String]
  )

  object mDocCtx {
    implicit val rw: RW[mDocCtx] = macroRW
  }

  // https://github.com/lihaoyi/mill/issues/598
  def initmDocCtx: T[mDocCtx] = T {
    val postModifierPaths: Seq[PathRef] = mdDocPostModifierPaths()
    val paths: Agg[PathRef] = mdocClasspath() //++ postModifierPaths
    val libPaths: String = paths
      .map(_.path.toIO.getAbsolutePath)
      .mkString(java.io.File.pathSeparator)
    printlnVerbose(s"MDoc libPaths = $libPaths", 2)
    val in: Path = mdocSourceDirectory().head.path
    printlnVerbose(s"MDoc in = ${in.toIO.getAbsolutePath}")
    val out: Path = mdocTargetDirectory()
    printlnVerbose(s"MDoc out = ${out.toIO.getAbsolutePath}")
    // TODO: fix
    val re: List[String] =
      if (mdocNameFilter().regex != "")
        List("--include", mdocNameFilter().regex)
      else List("")
    printlnVerbose(s"MDoc filter = $re")
    val scalaOpts: List[String] =
      if (mdocScalacOptions().nonEmpty)
        List("--scalac-options") ++ mdocScalacOptions()
      else List[String]()
    printlnVerbose(s"MDoc ScalacOptions = $scalaOpts")
    val mdocOpt: Seq[String] = mdocOpts()
    printlnVerbose(s"MDoc Options = $mdocOpt")
    val pOpts: mill.api.Loose.Agg[String] = mdocPluginJars().map(pathRef =>
      "-Xplugin:" + pathRef.path.toIO.getAbsolutePath
    )
    printlnVerbose(s"MDoc plugin options = $pOpts")

    val local: Loose.Agg[PathRef] = transitiveLocalClasspath()
    val localPaths: String = local
      .map(_.path.toIO.getAbsolutePath)
      .mkString(java.io.File.pathSeparator)
    printlnVerbose(s"MDoc localLibPaths = $localPaths")

    mDocCtx(
      postModifierPaths,
      paths,
      libPaths,
      in,
      out,
      re,
      scalaOpts,
      mdocOpt,
      pOpts
    )
  }

  /** Run MDoc using the configuration specified in this module. The working
    * directory used is the [[mill.contrib.mdoc.MDocModule#millSourcePath]].
    */
  def mDoc: T[os.CommandResult] = T {
    val ctx = initmDocCtx()

    // Correct format form options format
    val scalaOptions: List[String] = {
      ctx.scalaOpts match {
        case head :: tail => List(head, tail.mkString(" "))
        case Nil          => Nil
      }
    }

    // Because I am spawning a JVM to execute `mdoc.Main` so naturally I
    // cannot see the console's output because it is being redirected by
    // MDoc. If we used the API we could use the `Reporter`to see the output
    // Here we simple collect the output and print it at the end
    val args: List[String] = List(
      "--in",
      ctx.in.toIO.getAbsolutePath,
      "-out",
      ctx.out.toIO.getAbsolutePath,
      "--report-relative-paths"
    ) ++
      scalaOptions ++
      ctx.mdocOpt // ++ ctx.re
    val res = os
      .proc(
        'java,
        "-cp",
        ctx.libPaths,
        "mdoc.Main",
        args
      )
      .call(os.Path(ctx.in.toIO.getAbsolutePath))
    // Ths does not work. It has two issues.
    // The fist is that we get a java.io.IOException: Stream closed
    // The second is that the output of the PlotlyModifier does not show up

    // (Always) print out the collected output
    printlnVerbose(res.out)
    //println(res.out)
    res
  }

  /** MDoc has its own class loader that is used to load the PostModifier
    * implementations. We first access this class loader and instantiate
    * it as an Ammonite class loader (it has been subverted by Ammonite).
    * This class loader will be used to dynamically (at run-time) add the
    * PostModifier implementations to the MDoc resource path.
    *
    * We are also forced to add all local paths of code that the PostModifier
    * depends on for the same reason above. For example WebKit and Plots.
    * This may pose problems for use via a library.
    *
    * We first check and print the current MDoc resource path. We then take
    * the resource path to the (compiled) PostModifier classes and add those
    * to the MDoc resource path. Note that this will repeat the PostModifier
    * paths.
    *
    * Notes: "Not 100% sure I understand your question, but
    * `repl.sess.frames.flatMap(_.classloader.inMemoryClasses)`
    * gives a list of class names / class byte code generated by the repl."
    *
    * @see https://stackoverflow.com/questions/1010919/adding-files-to-java-classpath-at-runtime
    *      https://stackoverflow.com/questions/19414453/how-to-get-resources-directory-path-programmatically
    *      https://github.com/lihaoyi/Ammonite/blob/master/amm/runtime/src/main/scala/ammonite/runtime/ClassLoaders.scala
    */
  def addPathToLoader: Target[Unit] = T {
    import scala.collection.JavaConverters._

    // Get MDoc's class loader
    val cl = mdoc.Main.getClass.getClassLoader
    // Make sure we have Ammonite's class loader - it provides the add(URL) methods
    val cla = cl.asInstanceOf[ammonite.runtime.SpecialClassLoader]

    // Find all of the available MDoc PostModifier implementations using the Java service loader
    // This will be empty if have not added the correct resource path
    val post = java.util.ServiceLoader
      .load(classOf[mdoc.PostModifier], cl)
      .iterator()
      .asScala
      .toList
    // println(post.mkString(";\n"))
    printlnVerbose(
      s"""Checking for PostModifier (1): ${post.mkString("n")}""",
      2
    )

    // Same as above
    // Find all of the available MDoc PostModifier implementations using the Java service loader
    // This will be empty if have not added the correct resource path
    // Already available in the PostModifier
    val posts = mdoc.PostModifier.default()
    printlnVerbose(
      s"""Checking for PostModifier (2): ${posts.mkString("n")}""",
      2
    )

    // val postModifierResources = markdoc.resources()
    // val resources = postModifierResources.map(_.path.toIO.getAbsolutePath)
    // //val resource = new java.io.File(resources.head).toURI.toURL
    // printlnVerbose(
    //   s"""Existing resources paths: ${resources.mkString("n")}""",
    //   2
    // )

    // This is an example of how to invoke the private method of the class
    // loader that allows adding a resource or library path during run-time.
    // This does not work with Ammonite's classloader - it does not have this method
    /*
    val parameters = classOf[java.net.URL]
    val method: java.lang.reflect.Method = cl.getClass.getDeclaredMethod("addURL", parameters)
    method.setAccessible(true)
    val inputs = Array[Object](resource)
    method.invoke(cl, inputs)
     */

    // Add the resource paths of the project module where the PostModifiers are defined
    // Add transitive paths of the module project which will also include the PostModifiers dependencies
    val mods: Seq[PathRef] =
      mdDocPostModifierPaths() ++ transitiveLocalClasspath()
    // Add each of these paths to the class loader used by MDoc
    mods.foreach { e =>
      val f = e.path.toIO
      val resource = f.toURI.toURL
      //println("addPathToLoader URL ="+resource)
      cla.add(resource)
    }

  }

  /** Run MDoc using the configuration specified in the Ammomite script. The
    * working directory used is the [[mill.contrib.mdoc.MDocModule#millSourcePath]].
    * This uses a fixed version of the plug-in and runs within the Mill JVM.
    *
    * NOTE: MDoc needs to have its class loader updated with the compile
    * PostModifier implementations. This is done in the `addPathToLoader` call.
    * This is only required for the local API call because in the case of
    * the command line we add the resources path to the class path.
    */
  def mDocLocal: T[Int] = T {
    val ctx = initmDocCtx()

    val args = List() ++ ctx.mdocOpt //++ ctx.re

    /* java.lang.UnsupportedClassVersionError: com/sun/glass/ui/monocle/MonoclePlatformFactory has been compiled by a more recent version of the Java Runtime (class file version 54.0), this version of the Java Runtime only recognizes class file versions up to 52.0
    java.lang.ClassLoader.defineClass1(Native Method)
    java.lang.ClassLoader.defineClass(ClassLoader.java:763)
    java.security.SecureClassLoader.defineClass(SecureClassLoader.java:142)
    java.net.URLClassLoader.defineClass(URLClassLoader.java:468)
    java.net.URLClassLoader.access$100(URLClassLoader.java:74)
    java.net.URLClassLoader$1.run(URLClassLoader.java:369)
    java.net.URLClassLoader$1.run(URLClassLoader.java:363)
    java.security.AccessController.doPrivileged(Native Method)
    java.net.URLClassLoader.findClass(URLClassLoader.java:362)
    ammonite.runtime.SpecialClassLoader.findClass(ClassLoaders.scala:241)
    java.lang.ClassLoader.loadClass(ClassLoader.java:424)
    java.lang.ClassLoader.loadClass(ClassLoader.java:357)
    java.lang.Class.forName0(Native Method)
    java.lang.Class.forName(Class.java:264)
    utils.HeadlessWebKit$.assignMonoclePlatform(HeadlessWebKit.scala:616)
    utils.HeadlessWebKit$.initMonocleHeadless(HeadlessWebKit.scala:598)
    utils.HeadlessWebKit$.<init>(HeadlessWebKit.scala:127)
    utils.HeadlessWebKit$.<clinit>(HeadlessWebKit.scala)
    mdocs.PlotlyModifier$.initWebKit(PlotlyModifier.scala:30)
    mdocs.PlotlyModifier.<init>(PlotlyModifier.scala:59)
     */
// https://stackoverflow.com/questions/29116819/javafx-maven-testfx-monocle-dont-work-together

    // We cannot add the resources manually via the classpath parameter of the JVM
    // as we did with the Laika spawn. So here we add the path to the classloader
    // at run-time. This allows MDoc to load the PostModifiers when parsing a user
    // defined mode
    addPathToLoader()

    // 0.5.2-10-d7591b
    // JAVA_OPTS="-DsocksProxyHost=127.0.0.1 -DsocksProxyPort=1080 -DsocksProxyVersion=5" mill -i core.console
    // TODO: how do we pass the forkArgs of jFX and to the Mill JVM?
    // TODO: to have HeadlessWebKit we need to define it at the Mill level
    //import utils.{HeadlessWebKit, Utils}
    //HeadlessWebKit.launchBackground(Array[String]())
    //HeadlessWebKit.waitStart()

    val settings = mdoc
      .MainSettings()
      .withArgs(args) // for CLI only
      .withSiteVariables(Map("VERSION" -> "1.0.0"))
      .withIn(ctx.in.toIO.toPath)
      .withOut(ctx.out.toIO.toPath)
      .withScalacOptions(
        ctx.scalaOpts.drop(1).mkString(" ")
      ) // remove the flag name
      .withClasspath(ctx.libPaths)
      .withReportRelativePaths(true)
    // https://stackoverflow.com/questions/1247772/is-there-an-equivalent-of-java-util-regex-for-glob-type-patterns
    //java.nio.file.PathMatcher
    // https://www.programcreek.com/scala/java.util.regex.Pattern
    //.withIncludePath(mdocNameFilter())
    // generate out/readme.md from working directory
    val exitCode = mdoc.Main.process(settings)
    // (optional) exit the main function with exit code 0 (success) or 1 (error)
    //if (exitCode != 0) println(s"error = $exitCode")
    exitCode
  }

}
