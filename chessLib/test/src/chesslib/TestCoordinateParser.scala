package chesslib

import chessmodel.Board
import chessmodel.Coordinate
import chessmodel.PlayerPeace
import chessmodel.Figure
import chessmodel.Peace
import chessmodel.Player

object TestCoordinateParser {
  def parseExp(expectations: String): Map[Coordinate, Boolean] = {
    val rows = expectations.split('\n').zipWithIndex
    val expectationMatrix = rows.flatMap { case (textRow, rowId) =>
      textRow.zipWithIndex.map { case (square, colId) =>
        Coordinate(7 - rowId, colId) -> (square == 'X')
      }
    }.toMap
    expectationMatrix
  }
  def parseBoard(str: String): Board = {
    val rows = str.split('\n').zipWithIndex
    val board = rows
      .flatMap { case (textRow, rowId) =>
        textRow.zipWithIndex.map { case (square, colId) =>
          Coordinate(7 - rowId, colId) -> (square match {
            case 'Q' => Some(PlayerPeace(Figure.Queen, Player.White))
            case 'K' => Some(PlayerPeace(Figure.King, Player.White))
            case 'B' => Some(PlayerPeace(Figure.Bishop, Player.White))
            case 'P' => Some(PlayerPeace(Peace.Pawn, Player.White))
            case 'R' => Some(PlayerPeace(Figure.Rook, Player.White))
            case 'N' => Some(PlayerPeace(Figure.Knight, Player.White))
            case 'q' => Some(PlayerPeace(Figure.Queen, Player.Black))
            case 'k' => Some(PlayerPeace(Figure.King, Player.Black))
            case 'b' => Some(PlayerPeace(Figure.Bishop, Player.Black))
            case 'p' => Some(PlayerPeace(Peace.Pawn, Player.Black))
            case 'r' => Some(PlayerPeace(Figure.Rook, Player.Black))
            case 'n' => Some(PlayerPeace(Figure.Knight, Player.Black))
            case _   => None
          })
        }
      }
      .collect { case (coord, Some(p)) => coord -> p }
      .toMap

    Board(board)
  }
}
