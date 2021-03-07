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
      Coordinate(Rank(row), File(col)) -> PlayerPeace(Peace.Pawn, player)
    )
    .toMap

  private def figures(
      row: Int,
      player: Player
  ): Map[Coordinate, PlayerPeace] = {
    Map(
      Coordinate(Rank(row), File(0)) -> Figure.Rook,
      Coordinate(Rank(row), File(1)) -> Figure.Knight,
      Coordinate(Rank(row), File(2)) -> Figure.Bishop,
      Coordinate(Rank(row), File(3)) -> Figure.Queen,
      Coordinate(Rank(row), File(4)) -> Figure.King,
      Coordinate(Rank(row), File(5)) -> Figure.Bishop,
      Coordinate(Rank(row), File(6)) -> Figure.Knight,
      Coordinate(Rank(row), File(7)) -> Figure.Rook
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

case class Position(file: Char, rank: Char) {
  def toCoord(): Coordinate = {
    val col = File(this.file match {
      case 'a' => 0
      case 'b' => 1
      case 'c' => 2
      case 'd' => 3
      case 'e' => 4
      case 'f' => 5
      case 'g' => 6
      case 'h' => 7
    })
    val row = Rank(this.rank match {
      case '1' => 0
      case '2' => 1
      case '3' => 2
      case '4' => 3
      case '5' => 4
      case '6' => 5
      case '7' => 6
      case '8' => 7
    })
    Coordinate(row, col)
  }
}
object Position {
  def fromCoord(coord: Coordinate): Position = {
    val col = coord.col match {
      case 0 => 'a'
      case 1 => 'b'
      case 2 => 'c'
      case 3 => 'd'
      case 4 => 'e'
      case 5 => 'f'
      case 6 => 'g'
      case 7 => 'h'
    }
    val row = coord.row match {
      case 0 => '1'
      case 1 => '2'
      case 2 => '3'
      case 3 => '4'
      case 4 => '5'
      case 5 => '6'
      case 6 => '7'
      case 7 => '8'
    }
    Position(col, row)
  }
}
