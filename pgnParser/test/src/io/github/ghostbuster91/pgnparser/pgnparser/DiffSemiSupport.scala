package io.github.ghostbuster91.pgnparser.pgnparser

import cats.parse.Parser.Error
import cats.parse.Parser.Expectation
import com.softwaremill.diffx._
import com.softwaremill.diffx.cats._
import chessmodel._
import chessmodel.position._

trait DiffSemiSupport {
  implicit val dFile = Diff.useEquals[File]
  implicit val dRank = Diff.useEquals[Rank]
  implicit val dPos = Diff.derived[Position]
  implicit val dCheck = Diff.derived[Check]
  implicit val dFigure = Diff.derived[Figure]
  implicit val dExcep = Diff.derived[Expectation]
  implicit val dError = Diff.derived[Error]
  implicit val dMove = Diff.derived[SanMove]
  implicit val dRound = Diff.derived[Round]
  implicit val dResult = Diff.derived[GameResult]
  implicit val dMeta = Diff.derived[Tag]
  implicit val dPgnGame = Diff.derived[PgnGame]
}
