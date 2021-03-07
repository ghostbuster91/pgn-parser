package pgnparser

import utest._
import PgnParser._
import com.softwaremill.diffx.utest.DiffxAssertions._
import chessmodel._
import chessmodel.position._

object MovesParsingTest extends TestSuite with DiffSemiSupport {

  val tests = Tests {
    "parse move - figure capture with column source" - {
      assertEqual(
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
    }
  }
}
