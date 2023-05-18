package io.github.ghostbuster91.pgnparser.parser

import utest._
import PgnParser._

import chessmodel._
import chessmodel.position._
import difflicious.Differ
import cats.parse.Parser
import difflicious.generic.auto._
import difflicious.DifferTupleInstances
// import com.softwaremill.diffx.generic.auto._
import com.softwaremill.diffx.Diff
import difflicious.DiffResultPrinter

case class Person(name: String, age: Int)

object MovesParsingTest extends TestSuite with DifferTupleInstances {

  val tests = Tests {
    "parse move - figure capture with column source" - {
      Differ[Either[Parser.Error, (String, SanMove)]].assertNoDiff(
        move.parse("Rexf2"),
        Right(
          "" -> SanMove.FigureMove(
            Figure.Rook,
            Position(File('g'), Rank('2')),
            Check.NoCheck,
            None,
            Some(File('e')),
            isCapture = true
          )
        )
      )
    }
    /* "parse move - figure capture with row source" - {
      assertEqual(
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
      assertEqual(
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
    } */
  }

  implicit class DifferExtensions[A](differ: Differ[A]) {
    def assertNoDiff(obtained: A, expected: A): Unit = {
      val result = differ.diff(obtained, expected)
      if (!result.isOk) {
        println(DiffResultPrinter.consoleOutput(result, 0).render)
        assert(false)
      }
    }
  }
}
