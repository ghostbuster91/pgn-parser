import mill._, scalalib._
import mill.scalalib.scalafmt.ScalafmtModule
import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule

object pgnParser extends ScalaModule with ScalafmtModule {
  def moduleDeps = Seq(chessModel)
  def scalaVersion = "2.13.5"
  def ivyDeps = Agg(ivy"org.typelevel::cats-parse::0.2.0")
  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.7",
      ivy"com.softwaremill.diffx::diffx-utest::0.4.4",
      ivy"com.softwaremill.diffx::diffx-cats::0.4.4"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object chessLib extends ScalaModule with ScalafmtModule {
  def moduleDeps = Seq(chessModel)
  def scalaVersion = "2.13.5"
  def ivyDeps = Agg(ivy"com.beachape::enumeratum::1.6.1")
  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.7",
      ivy"com.softwaremill.diffx::diffx-utest::0.4.4"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
}

object chessModel extends ScalaModule with ScalafmtModule with TpolecatModule {
  def scalaVersion = "2.13.5"
  def ivyDeps =
    Agg(ivy"com.beachape::enumeratum::1.6.1", ivy"io.estatico::newtype::0.4.4")
  def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Xfatal-warnings")) ++ Seq(
      "-Ymacro-annotations"
    )
  }

}

object core extends ScalaModule with ScalafmtModule {
  def moduleDeps = Seq(chessModel)
  def scalaVersion = "2.13.5"
  def ivyDeps = Agg(ivy"com.beachape::enumeratum::1.6.1")
}

object pgnReader extends ScalaModule with ScalafmtModule {
  def moduleDeps = Seq(pgnParser, core, chessLib)
  def scalaVersion = "2.13.5"
  def ivyDeps = Agg(ivy"org.typelevel::cats-parse::0.2.0")
  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.7",
      ivy"com.softwaremill.diffx::diffx-utest::0.4.4",
      ivy"com.softwaremill.diffx::diffx-cats::0.4.4"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
