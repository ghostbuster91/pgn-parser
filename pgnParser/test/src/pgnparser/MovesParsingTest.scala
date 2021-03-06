package pgnparser

import utest._
import PgnParser._
import com.softwaremill.diffx.utest.DiffxAssertions._
import cats.parse.Parser.Error
import cats.parse.Parser.Expectation
import com.softwaremill.diffx._
import com.softwaremill.diffx.cats._
import chessmodel._

object MovesParsingTest extends TestSuite with DiffSemiSupport {

  val tests = Tests {
    "parse move - figure capture with column source" - {
      assertEqual(
        move.parse("Rexf2"),
        Right(
          "" -> SanMove.FigureMove(
            Figure.Rook,
            Position('f', '2'),
            Check.NoCheck,
            None,
            Some('e'),
            isCapture = true
          )
        )
      )
    }
    "parse move - figure capture with row source" - {
      assertEqual(
        move.parse("R1xf2"),
        Right(
          "" -> SanMove.FigureMove(
            Figure.Rook,
            Position('f', '2'),
            Check.NoCheck,
            Some('1'),
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
            Position('f', '2'),
            Check.NoCheck,
            Some('1'),
            Some('e'),
            isCapture = true
          )
        )
      )
    }
  }
}
