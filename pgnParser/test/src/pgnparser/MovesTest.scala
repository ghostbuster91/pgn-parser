package pgnparser

import utest._
import PgnParser._
import com.softwaremill.diffx.generic.auto._
import com.softwaremill.diffx.utest.DiffxAssertions._

object MovesTest extends TestSuite {
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
