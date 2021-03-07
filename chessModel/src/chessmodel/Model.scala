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
    pawns(1, Player.White) ++ figures(0, Player.White)
  private val Blacks =
    pawns(6, Player.Black) ++ figures(7, Player.Black)
  val Starting: Board = Board(Whites ++ Blacks)

  private def pawns(row: Int, player: Player) = (0 until 8)
    .map(col =>
      Coordinate(Column(row), Row(col)) -> PlayerPeace(Peace.Pawn, player)
    )
    .toMap

  private def figures(
      row: Int,
      player: Player
  ): Map[Coordinate, PlayerPeace] = {
    Map(
      Coordinate(Column(row), Row(0)) -> Figure.Rook,
      Coordinate(Column(row), Row(1)) -> Figure.Knight,
      Coordinate(Column(row), Row(2)) -> Figure.Bishop,
      Coordinate(Column(row), Row(3)) -> Figure.Queen,
      Coordinate(Column(row), Row(4)) -> Figure.King,
      Coordinate(Column(row), Row(5)) -> Figure.Bishop,
      Coordinate(Column(row), Row(6)) -> Figure.Knight,
      Coordinate(Column(row), Row(7)) -> Figure.Rook
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
