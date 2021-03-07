package chessmodel

import chessmodel.coordinate._

sealed trait Peace {
  def asChar(player: Player): Char
}
object Peace {
  case object Pawn extends Peace {
    def asChar(player: Player): Char = player match {
      case Player.White => 'P'
      case Player.Black => 'p'
    }
  }
}

sealed trait Figure extends Peace

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

case class PlayerPeace(peace: Peace, player: Player) {
  def asChar = peace.asChar(player)
}
object PlayerPeace {
  def fromChar(square: Char): Option[PlayerPeace] = {
    square match {
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
    }
  }
}

case class Board(peaces: Map[Coordinate, PlayerPeace]) {
  def getSquare(coords: Coordinate): Option[PlayerPeace] = peaces.get(coords)
  def move(m: List[Transformation]): Board = m.foldLeft(this) {
    case (acc, item) =>
      acc.move(item)
  }

  def move(t: Transformation): Board = {
    t match {
      case r: Transformation.Remove =>
        copy(peaces = peaces - r.from)
      case m: Transformation.Move =>
        copy(peaces = peaces - m.from + (m.to -> m.peace))
    }
  }

  def dump: String = {
    (0 until 8)
      .map { row =>
        (0 until 8).map { col =>
          peaces
            .get(Coordinate(Column(col), Row(7 - row)))
            .map(_.asChar)
            .getOrElse('-')
        }.mkString
      }
      .mkString("\n")
  }
}

sealed trait Transformation
object Transformation {
  case class Move(from: Coordinate, to: Coordinate, peace: PlayerPeace)
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
    .map(col => Coordinate(Column(col), row) -> PlayerPeace(Peace.Pawn, player))
    .toMap

  private def figures(
      row: Row,
      player: Player
  ): Map[Coordinate, PlayerPeace] = {
    Map(
      Coordinate(Column(0), row) -> Figure.Rook,
      Coordinate(Column(1), row) -> Figure.Knight,
      Coordinate(Column(2), row) -> Figure.Bishop,
      Coordinate(Column(3), row) -> Figure.Queen,
      Coordinate(Column(4), row) -> Figure.King,
      Coordinate(Column(5), row) -> Figure.Bishop,
      Coordinate(Column(6), row) -> Figure.Knight,
      Coordinate(Column(7), row) -> Figure.Rook
    ).view.mapValues(f => PlayerPeace(f, player)).toMap
  }
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
