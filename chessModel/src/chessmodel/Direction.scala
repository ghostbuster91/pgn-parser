package chessmodel

import enumeratum._
import scala.collection.immutable

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
