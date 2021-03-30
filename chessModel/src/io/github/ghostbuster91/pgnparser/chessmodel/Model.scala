package chessmodel

import chessmodel.coordinate._

sealed trait Piece {
  def asChar(player: Player): Char
}
object Piece {
  case object Pawn extends Piece {
    def asChar(player: Player): Char = player match {
      case Player.White => 'P'
      case Player.Black => 'p'
    }
  }
}

sealed trait Figure extends Piece

object Figure {
  case object King extends Figure {
    def asChar(player: Player): Char = player match {
      case Player.White => 'K'
      case Player.Black => 'k'
    }
  }
  case object Queen extends Figure {
    def asChar(player: Player): Char = player match {
      case Player.White => 'Q'
      case Player.Black => 'q'
    }
  }
  case object Bishop extends Figure {
    def asChar(player: Player): Char = player match {
      case Player.White => 'B'
      case Player.Black => 'b'
    }
  }
  case object Knight extends Figure {
    def asChar(player: Player): Char = player match {
      case Player.White => 'N'
      case Player.Black => 'n'
    }
  }
  case object Rook extends Figure {
    def asChar(player: Player): Char = player match {
      case Player.White => 'R'
      case Player.Black => 'r'
    }
  }
}

sealed trait Check
object Check {
  case object NoCheck extends Check
  case object SimpleCheck extends Check
  case object Checkmate extends Check
}

case class PlayerPiece(piece: Piece, player: Player) {
  def asChar = piece.asChar(player)
}
object PlayerPiece {
  def fromChar(square: Char): Option[PlayerPiece] =
    square match {
      case 'Q' => Some(PlayerPiece(Figure.Queen, Player.White))
      case 'K' => Some(PlayerPiece(Figure.King, Player.White))
      case 'B' => Some(PlayerPiece(Figure.Bishop, Player.White))
      case 'P' => Some(PlayerPiece(Piece.Pawn, Player.White))
      case 'R' => Some(PlayerPiece(Figure.Rook, Player.White))
      case 'N' => Some(PlayerPiece(Figure.Knight, Player.White))
      case 'q' => Some(PlayerPiece(Figure.Queen, Player.Black))
      case 'k' => Some(PlayerPiece(Figure.King, Player.Black))
      case 'b' => Some(PlayerPiece(Figure.Bishop, Player.Black))
      case 'p' => Some(PlayerPiece(Piece.Pawn, Player.Black))
      case 'r' => Some(PlayerPiece(Figure.Rook, Player.Black))
      case 'n' => Some(PlayerPiece(Figure.Knight, Player.Black))
      case _   => None
    }
}

case class Board(pieces: Map[Coordinate, PlayerPiece]) {
  def getSquare(coords: Coordinate): Option[PlayerPiece] = pieces.get(coords)
  def move(m: List[Transformation]): Board = m.foldLeft(this) {
    case (acc, item) =>
      acc.move(item)
  }

  def move(t: Transformation): Board =
    t match {
      case r: Transformation.Remove =>
        copy(pieces = pieces - r.from)
      case m: Transformation.Move =>
        copy(pieces = pieces - m.from + (m.to -> m.piece))
    }

  def dump: String =
    (0 until 8)
      .map { row =>
        (0 until 8).map { col =>
          pieces
            .get(Coordinate(Column(col), Row(7 - row)))
            .map(_.asChar)
            .getOrElse('-')
        }.mkString
      }
      .mkString("\n")
}

sealed trait Transformation
object Transformation {
  case class Move(from: Coordinate, to: Coordinate, piece: PlayerPiece)
      extends Transformation
  case class Remove(from: Coordinate) extends Transformation
}
object Board {

  private val Whites =
    pawns(Row(1), Player.White) ++ figures(Row(0), Player.White)
  private val Blacks =
    pawns(Row(6), Player.Black) ++ figures(Row(7), Player.Black)
  val Starting: Board = Board(Whites ++ Blacks)

  private def pawns(row: Row, player: Player) = (0 until 8)
    .map(col => Coordinate(Column(col), row) -> PlayerPiece(Piece.Pawn, player))
    .toMap

  private def figures(
      row: Row,
      player: Player
  ): Map[Coordinate, PlayerPiece] =
    Map(
      Coordinate(Column(0), row) -> Figure.Rook,
      Coordinate(Column(1), row) -> Figure.Knight,
      Coordinate(Column(2), row) -> Figure.Bishop,
      Coordinate(Column(3), row) -> Figure.Queen,
      Coordinate(Column(4), row) -> Figure.King,
      Coordinate(Column(5), row) -> Figure.Bishop,
      Coordinate(Column(6), row) -> Figure.Knight,
      Coordinate(Column(7), row) -> Figure.Rook
    ).view.mapValues(f => PlayerPiece(f, player)).toMap
}

sealed trait Player {
  def opponent: Player
}
object Player {
  case object Black extends Player {
    def opponent: Player = White
  }
  case object White extends Player {
    def opponent: Player = Black
  }
}
