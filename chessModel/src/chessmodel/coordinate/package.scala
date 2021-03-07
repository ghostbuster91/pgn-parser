package chessmodel

import io.estatico.newtype.macros.newtype

package object coordinate {
  @newtype case class Column(v: Int)
  @newtype case class Row(v: Int)
}
