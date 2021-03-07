package chessmodel.coordinate

case class Shift(rowInc: Int, colInc: Int)
case class Coordinate(row: Column, col: Row) {
  def shift(s: Shift): Option[Coordinate] = {
    val nextSquare =
      copy(row = Column(row + s.rowInc), col = Row(col + s.colInc))
    val isValidRow = nextSquare.row < 8 || nextSquare.row >= 0
    val isValidCol = nextSquare.col < 8 || nextSquare.col >= 0
    if (isValidRow && isValidCol) {
      Some(nextSquare)
    } else {
      None
    }
  }
}
