import mill._, scalalib._
import mill.scalalib.publish._
import mill.scalalib.scalafmt.ScalafmtModule
import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule
import $ivy.`com.goyeau::mill-scalafix:0.2.1`
import com.goyeau.mill.scalafix.ScalafixModule
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version_mill0.9:0.1.1`
import de.tobiasroeser.mill.vcs.version.VcsVersion

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
  val pomOrg = "io.ghostbuster91"
  val githubOrg = "ghostbuster91"
  val githubRepo = "pgn-parser"
  val projectUrl = s"https://github.com/$githubOrg/$githubRepo"
}
