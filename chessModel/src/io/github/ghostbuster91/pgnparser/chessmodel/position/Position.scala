package chessmodel.position

import chessmodel.coordinate._

case class Position(file: File, rank: Rank) {
  def toCoord(): Coordinate =
    Coordinate(file.toColumn, rank.toRow)
}
object Position {
  def fromCoord(coord: Coordinate): Position = {
    val file = File.fromColumn(coord.col)
    val rank = Rank.fromRow(coord.row)
    Position(file, rank)
  }
}
