package io.github.ghostbuster91.pgnparser.chesslib

import chessmodel.Board
import chessmodel.coordinate._
import chessmodel.PlayerPiece

object TestCoordinateParser {
  def parseExp(expectations: String): Map[Coordinate, Boolean] = {
    val rows = expectations.split('\n').zipWithIndex
    rows.flatMap { case (textRow, rowId) =>
      textRow.zipWithIndex.map { case (square, colId) =>
        Coordinate(Column(colId), Row(7 - rowId)) -> (square == 'X')
      }
    }.toMap
  }
  def parseBoard(str: String): Board = {
    val rows = str.split('\n').zipWithIndex
    val board = rows
      .flatMap { case (textRow, rowId) =>
        textRow.zipWithIndex.map { case (square, colId) =>
          Coordinate(Column(colId), Row(7 - rowId)) -> (PlayerPiece.fromChar(
            square
          ))
        }
      }
      .collect { case (coord, Some(p)) => coord -> p }
      .toMap

    Board(board)
  }
}
