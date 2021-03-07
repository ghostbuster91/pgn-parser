package chessmodel

import io.estatico.newtype.macros.newtype
import chessmodel.coordinate._

package object position {
  @newtype case class File(v: Char) {
    def toColumn: Column = {
      Column(v match {
        case 'a' => 0
        case 'b' => 1
        case 'c' => 2
        case 'd' => 3
        case 'e' => 4
        case 'f' => 5
        case 'g' => 6
        case 'h' => 7
      })
    }
  }
  object File {
    def fromColumn(c: Column): File = {
      File(c match {
        case 0 => 'a'
        case 1 => 'b'
        case 2 => 'c'
        case 3 => 'd'
        case 4 => 'e'
        case 5 => 'f'
        case 6 => 'g'
        case 7 => 'h'
      })
    }
  }
  @newtype case class Rank(v: Char) {
    def toRow: Row = {
      Row(v match {
        case '1' => 0
        case '2' => 1
        case '3' => 2
        case '4' => 3
        case '5' => 4
        case '6' => 5
        case '7' => 6
        case '8' => 7
      })
    }
  }
  object Rank {
    def fromRow(r: Row): Rank = {
      Rank(r match {
        case 0 => '1'
        case 1 => '2'
        case 2 => '3'
        case 3 => '4'
        case 4 => '5'
        case 5 => '6'
        case 6 => '7'
        case 7 => '8'
      })
    }
  }
}
