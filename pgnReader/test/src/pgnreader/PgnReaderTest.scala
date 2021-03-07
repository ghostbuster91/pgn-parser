package pgnreader

import utest._
import chessmodel._
import chessmodel.coordinate._
import chessmodel.position._
import core.ChessGame
import pgnparser.SanMove

object PgnReaderTest extends TestSuite {
  val tests = Tests {

    "should handle en passant" - {
      val input = """-nb--rk-
                    |r-----bp
                    |p--p--p-
                    |q-pPppN-
                    |-pP-----
                    |-P-B-N--
                    |P-Q--PPP
                    |---RR-K-""".stripMargin
      val board = parseBoard(input)
      val gameAfter = new PgnReader().applyMove(
        ChessGame(board, Player.White, None),
        SanMove.PawnCapture(
          Position(File('e'), Rank('6')),
          File('d'),
          Check.NoCheck,
          None
        )
      )
      val boardAfter = gameAfter.unwrap().board.dump
      val expected = """-nb--rk-
                      |r-----bp
                      |p--pP-p-
                      |q-p--pN-
                      |-pP-----
                      |-P-B-N--
                      |P-Q--PPP
                      |---RR-K-""".stripMargin
      assert(boardAfter == expected)
    }

    // val games: List[String] = loadGames

    // "parse successfully 10k games" - {
    //   games.zipWithIndex.foreach { case (pgnGame, idx) =>
    //     val res = new PgnReader().read(pgnGame)
    //     res match {
    //       case Left(value) =>
    //         println(s"Game $idx failed to parse with error: $value")
    //         assert(false)
    //       case Right(_) => // noop
    //     }
    //   }
    // }
  }

  private def loadGames = { //TODO deduplicate
    val source =
      scala.io.Source.fromResource("lichess_db_10k_lines_stripped_2020-12.pgn")
    try source.getLines().toList
    finally source.close()
  }

  def parseBoard(str: String): Board = { //TODO deduplicate
    val rows = str.split('\n').zipWithIndex
    val board = rows
      .flatMap { case (textRow, rowId) =>
        textRow.zipWithIndex.map { case (square, colId) =>
          Coordinate(Column(colId), Row(7 - rowId)) -> (PlayerPeace.fromChar(
            square
          ))
        }
      }
      .collect { case (coord, Some(p)) => coord -> p }
      .toMap

    Board(board)
  }

  implicit class RichEither[L <: Exception, R](e: Either[L, R]) {
    def unwrap(): R = {
      e match {
        case Left(value)  => throw value
        case Right(value) => value
      }
    }
  }
}
