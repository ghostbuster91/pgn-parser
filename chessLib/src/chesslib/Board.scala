package chesslib

import enumeratum._
import chesslib.Peace.Pawn
import chesslib.Player.Black
import chesslib.Player.White
import chesslib.Direction.NorthWest
import chesslib.Direction.NorthEast
import chesslib.Direction.East
import chesslib.Direction.West
import chesslib.Direction.North
import scala.collection.immutable

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

case class PlayerPeace(peace: Peace, player: Player)
case class Shift(rowInc: Int, colInc: Int)
case class Coordinate(row: Int, col: Int) {
  def shift(s: Shift): Option[Coordinate] = {
    val nextSquare = copy(row = row + s.rowInc, col = col + s.colInc)
    val isValidRow = nextSquare.row < 8 || nextSquare.row >= 0
    val isValidCol = nextSquare.col < 8 || nextSquare.col >= 0
    if (isValidRow && isValidCol) {
      Some(nextSquare)
    } else {
      None
    }
  }
}

case class Board(peaces: Map[Coordinate, PlayerPeace]) {
  def getSquare(coords: Coordinate): Option[PlayerPeace] = peaces.get(coords)
}

object Board {

  private val Whites =
    pawns(1, Player.White) ++ figures(0, Player.White)
  private val Blacks =
    pawns(6, Player.Black) ++ figures(7, Player.Black)
  val Starting = Whites ++ Blacks

  private def pawns(row: Int, player: Player) = (0 until 8)
    .map(col => Coordinate(row, col) -> PlayerPeace(Pawn, player))
    .toMap

  private def figures(
      row: Int,
      player: Player
  ): Map[Coordinate, PlayerPeace] = {
    Map(
      Coordinate(row, 0) -> Figure.Rook,
      Coordinate(row, 1) -> Figure.Knight,
      Coordinate(row, 2) -> Figure.Bishop,
      Coordinate(row, 3) -> Figure.Queen,
      Coordinate(row, 4) -> Figure.King,
      Coordinate(row, 5) -> Figure.Bishop,
      Coordinate(row, 6) -> Figure.Knight,
      Coordinate(row, 7) -> Figure.Rook
    ).view.mapValues(f => PlayerPeace(f, player)).toMap
  }
}

sealed trait Player
object Player {
  case object Black extends Player
  case object White extends Player
}

//TODO try changing to abstract class with constructor args
sealed trait Direction extends EnumEntry {
  def shift: Shift
  def opposite: Direction
}

sealed trait Diagonal extends Direction
sealed trait File extends Direction
sealed trait Rank extends Direction

object Direction extends Enum[Direction] {

  case object NorthWest extends Diagonal {
    def shift: Shift = Shift(1, -1)
    val opposite = SouthEast
  }
  case object NorthEast extends Diagonal {
    def shift: Shift = Shift(1, 1)
    val opposite = SouthWest
  }
  case object SouthWest extends Diagonal {
    def shift: Shift = Shift(-1, -1)
    val opposite = NorthEast
  }
  case object SouthEast extends Diagonal {
    def shift: Shift = Shift(-1, 1)
    val opposite = NorthWest
  }
  case object East extends Rank {
    def shift: Shift = Shift(0, 1)
    val opposite = West
  }
  case object West extends Rank {
    def shift: Shift = Shift(0, -1)
    val opposite = East
  }
  case object North extends File {
    def shift: Shift = Shift(1, 0)
    val opposite = South
  }
  case object South extends File {
    def shift: Shift = Shift(-1, 0)
    val opposite = North
  }
  override def values: IndexedSeq[Direction] = findValues
}
