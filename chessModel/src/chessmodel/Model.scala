package chessmodel

import chessmodel.coordinate._

sealed trait Peace
object Peace {
  case object Pawn extends Peace
}

sealed trait Figure extends Peace

object Figure {
  case object King extends Figure
  case object Queen extends Figure
  case object Bishop extends Figure
  case object Knight extends Figure
  case object Rook extends Figure
}

sealed trait Check
object Check {
  case object NoCheck extends Check
  case object SimpleCheck extends Check
  case object Checkmate extends Check
}

case class PlayerPeace(peace: Peace, player: Player)

case class Board(peaces: Map[Coordinate, PlayerPeace]) {
  def getSquare(coords: Coordinate): Option[PlayerPeace] = peaces.get(coords)
  def move(m: List[Move]): Board = m.foldLeft(this) { case (acc, item) =>
    acc.move(item)
  }

  def move(m: Move): Board = {
    copy(peaces = peaces - m.from + (m.to -> m.peace))
  }
}

case class Move(from: Coordinate, to: Coordinate, peace: PlayerPeace)

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
