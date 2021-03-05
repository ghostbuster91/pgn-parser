package pgnparser

import cats.parse.Parser.Error
import cats.parse.Parser.Expectation
import com.softwaremill.diffx._
import com.softwaremill.diffx.cats._

trait DiffSemiSupport {
  implicit val dPos = Diff.derived[Position]
  implicit val dCheck = Diff.derived[Check]
  implicit val dFigure = Diff.derived[Figure]
  implicit val dExcep = Diff.derived[Expectation]
  implicit val dError = Diff.derived[Error]
  implicit val dMove = Diff.derived[Move]
  implicit val dRound = Diff.derived[Round]
  implicit val dResult = Diff.derived[GameResult]
  implicit val dMeta = Diff.derived[Meta]
  implicit val dPgnGame = Diff.derived[PgnGame]
}
