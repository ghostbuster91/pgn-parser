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
}
