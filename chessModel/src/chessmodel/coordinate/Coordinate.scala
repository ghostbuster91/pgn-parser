package chessmodel.coordinate

case class Shift(rowInc: Int, colInc: Int)
case class Coordinate(row: Rank, col: File) {
  def shift(s: Shift): Option[Coordinate] = {
    val nextSquare =
      copy(row = Rank(row.v + s.rowInc), col = File(col.v + s.colInc))
    val isValidRow = nextSquare.row.v < 8 || nextSquare.row.v >= 0
    val isValidCol = nextSquare.col.v < 8 || nextSquare.col.v >= 0
    if (isValidRow && isValidCol) {
      Some(nextSquare)
    } else {
      None
    }
  }
}
