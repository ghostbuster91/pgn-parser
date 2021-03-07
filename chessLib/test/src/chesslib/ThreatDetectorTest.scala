package chesslib

import utest._
import chessmodel._
import chessmodel.position._

object ThreatDetectorTest extends TestSuite {

  val tests = Tests {
    "empty board shouldn't produce any checks" - {
      test(
        """--------
          |--------
          |--------
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
      )(Player.Black)
    }
    "should correctly detect queen's threats" - {
      test(
        """--------
          |--------
          |--------
          |--------
          |--------
          |--------
          |-Q------
          |--------""",
        """-X-----X
          |-X----X-
          |-X---X--
          |-X--X---
          |-X-X----
          |XXX-----
          |X-XXXXXX
          |XXX-----"""
      )(Player.Black)
    }
    "should not detect threat if there is a friendly figure in the way" - {
      test(
        """--------
          |--------
          |--------
          |-r------
          |--------
          |--------
          |-Q------
          |--------""",
        """-------X
          |------X-
          |-----X--
          |-X--X---
          |-X-X----
          |XXX-----
          |X-XXXXXX
          |XXX-----"""
      )(Player.Black)
    }
    "should detect threat from evil knight" - {
      test(
        """--------
          |--------
          |--------
          |--------
          |---N----
          |--------
          |--------
          |--------""",
        """--------
          |--------
          |--X-X---
          |-X---X--
          |--------
          |-X---X--
          |--X-X---
          |--------"""
      )(Player.Black)
    }
    "should detect threat from evil white pawn" - {
      test(
        """--------
          |--------
          |--------
          |--------
          |---P----
          |--------
          |--------
          |--------""",
        """--------
          |--------
          |--------
          |--X-X---
          |--------
          |--------
          |--------
          |--------"""
      )(Player.Black)
    }
    "should detect threat from evil black pawn" - {
      test(
        """--------
          |--------
          |--------
          |--------
          |---p----
          |--------
          |--------
          |--------""",
        """--------
          |--------
          |--------
          |--------
          |--------
          |--X-X---
          |--------
          |--------"""
      )(Player.White)
    }
    "should correctly detect bishop's threats" - {
      test(
        """--------
          |--------
          |-----b--
          |--------
          |---B----
          |--------
          |--------
          |--------""",
        """--------
          |X-------
          |-X---X--
          |--X-X---
          |--------
          |--X-X---
          |-X---X--
          |X-----X-"""
      )(Player.Black)
    }
    "should correctly detect rook's threats" - {
      test(
        """--------
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
          |---X-----"""
      )(Player.Black)
    }
    "should correctly detect kings's threats" - {
      test(
        """--------
          |--------
          |--------
          |--------
          |---K----
          |--------
          |--------
          |--------""",
        """--------
          |--------
          |--------
          |--XXX---
          |--X-X---
          |--XXX---
          |--------
          |---------"""
      )(Player.Black)
    }
  }

  private def test(strBoard: String, strMatrix: String)(player: Player) = {
    val matrix = TestCoordinateParser.parseExp(strMatrix.stripMargin)
    val board = TestCoordinateParser.parseBoard(strBoard.stripMargin)
    matrix.foreach { case (coord, expected) =>
      val result = Engine.isSquareCheckedBy(coord, board, player.opponent)
      Predef.assert(
        result == expected,
        s"${Position.fromCoord(coord)} expected: $expected"
      )
    }
  }
}
