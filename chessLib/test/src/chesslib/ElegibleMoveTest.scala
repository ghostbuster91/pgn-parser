package chesslib

import utest._
import chessmodel._

object ElegibleMoveTest extends TestSuite {
  val tests = Tests {
    "rook should be able to move horizontally and vertically" - {
      test(
        """-------K
          |--------
          |--------
          |--------
          |---R----
          |--------
          |--------
          |--------""",
        """---X----
          |---X----
          |---X----
          |---X----
          |XXX-XXXX
          |---X----
          |---X----
          |---X----"""
      )(LocatedPeace(Position('d', '4'), Player.White))
    }

    "pinned rook shouldn't be able to move" - {
      test(
        """-------K
          |------R-
          |-----b--
          |--------
          |--------
          |--------
          |--------
          |--------""",
        """--------
          |--------
          |--------
          |--------
          |--------
          |--------
          |--------
          |--------"""
      )(LocatedPeace(Position('g', '7'), Player.White))
    }
  }
  private def test(strBoard: String, strMatrix: String)(lp: LocatedPeace) = {
    val matrix = TestCoordinateParser.parseExp(strMatrix.stripMargin)
    val board = TestCoordinateParser.parseBoard(strBoard.stripMargin)
    matrix.foreach { case (coord, expected) =>
      val figure =
        board.getSquare(lp.position.toCoord()).get.peace.asInstanceOf[Figure]
      val result =
        Engine.isEligibleToMove(
          lp.position.toCoord(),
          coord,
          board,
          figure,
          lp.player
        )
      Predef.assert(
        result == expected,
        s"${Position.fromCoord(coord)} expected: $expected"
      )
    }
  }
  case class LocatedPeace(position: Position, player: Player)
}
