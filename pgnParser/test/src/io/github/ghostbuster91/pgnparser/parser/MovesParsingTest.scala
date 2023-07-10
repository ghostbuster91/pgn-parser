package io.github.ghostbuster91.pgnparser.parser

import utest._
import PgnParser._

import chessmodel._
import chessmodel.position._
import difflicious.Differ
import cats.parse.Parser
import difflicious.scalatest.ScalatestDiff._

case class Person(name: String, age: Int)

object MovesParsingTest extends TestSuite with DiffSemiSupport {

  val tests = Tests {
    "parse move - figure capture with column source" - {
      Differ[Either[Parser.Error, (String, SanMove)]].assertNoDiff(
        move.parse("Rexf2"),
        Right(
          "" -> SanMove.FigureMove(
            Figure.Rook,
            Position(File('f'), Rank('2')),
            Check.NoCheck,
            None,
            Some(File('e')),
            isCapture = true
          )
        )
      )
    }
    "parse move - figure capture with row source" - {
      Differ[Either[Parser.Error, (String, SanMove)]].assertNoDiff(
        move.parse("R1xf2"),
        Right(
          "" -> SanMove.FigureMove(
            Figure.Rook,
            Position(File('f'), Rank('2')),
            Check.NoCheck,
            Some(Rank('1')),
            None,
            isCapture = true
          )
        )
      )
    }
    "parse move - figure capture with position source" - {
      Differ[Either[Parser.Error, (String, SanMove)]].assertNoDiff(
        move.parse("Re1xf2"),
        Right(
          "" -> SanMove.FigureMove(
            Figure.Rook,
            Position(File('f'), Rank('2')),
            Check.NoCheck,
            Some(Rank('1')),
            Some(File('e')),
            isCapture = true
          )
        )
      )
    }
  }
}
