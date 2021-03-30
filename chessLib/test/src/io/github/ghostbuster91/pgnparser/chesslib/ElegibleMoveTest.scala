package io.github.ghostbuster91.pgnparser.chesslib

import utest._
import chessmodel._
import chessmodel.position._

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
      )(LocatedPiece(Position(File('d'), Rank('4')), Player.White))
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
      )(LocatedPiece(Position(File('g'), Rank('7')), Player.White))
    }
  }
  private def test(strBoard: String, strMatrix: String)(lp: LocatedPiece) = {
    val matrix = TestCoordinateParser.parseExp(strMatrix.stripMargin)
    val board = TestCoordinateParser.parseBoard(strBoard.stripMargin)
    matrix.foreach { case (coord, expected) =>
      board.getSquare(lp.position.toCoord()).get.piece match {
        case figure: Figure =>
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
        case Piece.Pawn => //TODO
      }
    }
  }
  case class LocatedPiece(position: Position, player: Player)
}
