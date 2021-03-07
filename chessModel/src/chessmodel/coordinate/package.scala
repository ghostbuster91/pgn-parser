package chessmodel

import io.estatico.newtype.macros.newtype

package object coordinate {
  @newtype case class Rank(v: Int)
  @newtype case class File(v: Int)
}
