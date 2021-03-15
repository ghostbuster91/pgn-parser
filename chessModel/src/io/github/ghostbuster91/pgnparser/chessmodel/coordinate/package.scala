package chessmodel

import io.estatico.newtype.macros.newsubtype

package object coordinate {
  @newsubtype case class Column(v: Int)
  @newsubtype case class Row(v: Int)
}
