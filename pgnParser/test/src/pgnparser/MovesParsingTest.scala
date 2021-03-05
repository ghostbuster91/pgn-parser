package pgnparser

import utest._
import PgnParser._
import com.softwaremill.diffx.utest.DiffxAssertions._
import cats.parse.Parser.Error
import cats.parse.Parser.Expectation
import com.softwaremill.diffx._
import com.softwaremill.diffx.cats._

object MovesParsingTest extends TestSuite with DiffSemiSupport {

  val tests = Tests {
    "parse move - figure capture with column source" - {
      assertEqual(
        move.parse("Rexf2"),
        Right(
          "" -> Move.FigureCapture(
            Position('f', '2'),
            Figure.Rook,
            Check.NoCheck,
            None,
            Some('e')
          )
        )
      )
    }
    "parse move - figure capture with row source" - {
      assertEqual(
        move.parse("R1xf2"),
        Right(
          "" -> Move.FigureCapture(
            Position('f', '2'),
            Figure.Rook,
            Check.NoCheck,
            Some('1'),
            None
          )
        )
      )
    }
    "parse move - figure capture with position source" - {
      assertEqual(
        move.parse("Re1xf2"),
        Right(
          "" -> Move.FigureCapture(
            Position('f', '2'),
            Figure.Rook,
            Check.NoCheck,
            Some('1'),
            Some('e')
          )
        )
      )
    }
  }
}
