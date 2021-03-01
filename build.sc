import mill._, scalalib._
import mill.scalalib.scalafmt.ScalafmtModule

object pgnParser extends ScalaModule with ScalafmtModule {
  def scalaVersion = "2.13.5"
  def ivyDeps = Agg(ivy"org.typelevel::cats-parse::0.2.0")
  object test extends Tests {
    def ivyDeps = Agg(
      ivy"com.lihaoyi::utest::0.7.7",
      ivy"com.softwaremill.diffx::diffx-utest::0.4.3"
    )
    def testFrameworks = Seq("utest.runner.Framework")
  }
}
