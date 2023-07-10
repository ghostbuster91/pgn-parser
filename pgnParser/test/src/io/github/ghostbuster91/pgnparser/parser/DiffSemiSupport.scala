package io.github.ghostbuster91.pgnparser.parser

import cats.parse.Parser.Error
import cats.parse.Parser.Expectation
import chessmodel._
import chessmodel.position._
import difflicious.DifferTupleInstances
import difflicious.Differ
import difflicious.cats.implicits._

trait DiffSemiSupport extends DifferTupleInstances {
  implicit val dFile = Differ.useEquals[File](valueToString = _.toString)
  implicit val dRank = Differ.useEquals[Rank](valueToString = _.toString)
  implicit val dPos = Differ.derived[Position]
  implicit val dCheck = Differ.derived[Check]
  implicit val dFigure = Differ.derived[Figure]
  implicit val dExcep = Differ.derived[Expectation]
  implicit val dError = Differ.derived[Error]
  implicit val dMove = Differ.derived[SanMove]
  implicit val dRound = Differ.derived[Round]
  implicit val dResult = Differ.derived[GameResult]
  implicit val dMeta = Differ.derived[Tag]
  implicit val dPgnGame = Differ.derived[PgnGame]

}
