import mill._, scalalib._
import mill.scalalib.scalafmt.ScalafmtModule
import $ivy.`io.github.davidgregory084::mill-tpolecat:0.2.0`
import io.github.davidgregory084.TpolecatModule

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

trait ChessModule extends ScalaModule with ScalafmtModule with TpolecatModule {
  def scalaVersion = "2.13.5"
  def scalacOptions = T {
    super.scalacOptions().filterNot(Set("-Xfatal-warnings")) ++ Seq(
      "-Ymacro-annotations"
    )
  }
}

trait CommonTestModule extends ScalaModule with TestModule {
  def ivyDeps = Agg(
    ivy"com.lihaoyi::utest::0.7.7",
    ivy"com.softwaremill.diffx::diffx-utest::0.4.4",
    ivy"com.softwaremill.diffx::diffx-cats::0.4.4"
  )
  def testFrameworks = Seq("utest.runner.Framework")
}
