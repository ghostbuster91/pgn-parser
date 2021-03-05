package chesslib

import utest._

object ChessLibTest extends TestSuite {

  val AllCords: Set[Coordinate] = (for {
    x <- 0 until 8
    y <- 0 until 8
  } yield Coordinate(x, y)).toSet

  val tests = Tests {
    "empty board shouldn't produce any checks" - {
      val board = Board(Map())
      AllCords.foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(!result)
      }
    }
    "should correctly detect queen's threats" - {
      val board =
        Board(
          Map(
            Coordinate(1, 1) -> PlayerPeace(Figure.Queen, Player.White)
          )
        )
      val expectedThreads = Set(
        Coordinate(1, 0),
        Coordinate(0, 1),
        Coordinate(2, 1),
        Coordinate(3, 1),
        Coordinate(4, 1),
        Coordinate(5, 1),
        Coordinate(6, 1),
        Coordinate(7, 1),
        Coordinate(1, 2),
        Coordinate(1, 3),
        Coordinate(1, 4),
        Coordinate(1, 5),
        Coordinate(1, 6),
        Coordinate(1, 7),
        Coordinate(0, 0),
        Coordinate(2, 2),
        Coordinate(3, 3),
        Coordinate(4, 4),
        Coordinate(5, 5),
        Coordinate(6, 6),
        Coordinate(7, 7),
        Coordinate(2, 0),
        Coordinate(0, 2)
      )
      expectedThreads.foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(result)
      }
      (AllCords -- expectedThreads).foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(!result)
      }
    }
    "should not detect threat if there is a friendly figure in the way" - {
      val board =
        Board(
          Map(
            Coordinate(1, 1) -> PlayerPeace(Figure.Queen, Player.White),
            Coordinate(3, 1) -> PlayerPeace(Figure.Queen, Player.Black)
          )
        )
      val result = Engine.isSquareChecked(Coordinate(7, 1), board, Player.Black)
      assert(!result)
    }
    "should detect threat from evil knight" - {
      val board =
        Board(
          Map(
            Coordinate(1, 1) -> PlayerPeace(Figure.Knight, Player.White)
          )
        )
      val expectedThreads = List(
        Coordinate(3, 2),
        Coordinate(2, 3),
        Coordinate(0, 3),
        Coordinate(3, 0)
      )
      expectedThreads.foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(result)
      }
      (AllCords -- expectedThreads).foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(!result)
      }
    }
    "should detect threat from evil white pawn" - {
      val board =
        Board(
          Map(
            Coordinate(1, 1) -> PlayerPeace(Peace.Pawn, Player.White)
          )
        )
      val expectedThreads = List(
        Coordinate(2, 0),
        Coordinate(2, 2)
      )
      expectedThreads.foreach { coord =>
        println(coord)
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(result)
      }
      (AllCords -- expectedThreads).foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(!result)
      }
    }
    "should detect threat from evil black pawn" - {
      val board =
        Board(
          Map(
            Coordinate(1, 1) -> PlayerPeace(Peace.Pawn, Player.Black)
          )
        )
      val expectedThreads = List(
        Coordinate(0, 0),
        Coordinate(0, 2)
      )
      expectedThreads.foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.White)
        assert(result)
      }
      (AllCords -- expectedThreads).foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.White)
        assert(!result)
      }
    }
    "should correctly detect bishop's threats" - {
      val board =
        Board(
          Map(
            Coordinate(1, 1) -> PlayerPeace(Figure.Bishop, Player.White)
          )
        )
      val expectedThreads = Set(
        Coordinate(0, 0),
        Coordinate(2, 2),
        Coordinate(3, 3),
        Coordinate(4, 4),
        Coordinate(5, 5),
        Coordinate(6, 6),
        Coordinate(7, 7),
        Coordinate(2, 0),
        Coordinate(0, 2)
      )
      expectedThreads.foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(result)
      }
      (AllCords -- expectedThreads).foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(!result)
      }
    }
    "should correctly detect rook's threats" - {
      val board =
        Board(
          Map(
            Coordinate(1, 1) -> PlayerPeace(Figure.Rook, Player.White)
          )
        )
      val expectedThreads = Set(
        Coordinate(1, 0),
        Coordinate(0, 1),
        Coordinate(2, 1),
        Coordinate(3, 1),
        Coordinate(4, 1),
        Coordinate(5, 1),
        Coordinate(6, 1),
        Coordinate(7, 1),
        Coordinate(1, 2),
        Coordinate(1, 3),
        Coordinate(1, 4),
        Coordinate(1, 5),
        Coordinate(1, 6),
        Coordinate(1, 7)
      )
      expectedThreads.foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(result)
      }
      (AllCords -- expectedThreads).foreach { coord =>
        val result = Engine.isSquareChecked(coord, board, Player.Black)
        assert(!result)
      }
    }
  }
}
