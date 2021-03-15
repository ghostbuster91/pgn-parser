package chessmodel.coordinate

case class Shift(rowInc: Int, colInc: Int)
case class Coordinate(col: Column, row: Row) {
  def shift(s: Shift): Option[Coordinate] = {
    val nextSquare =
      copy(row = Row(row + s.rowInc), col = Column(col + s.colInc))
    val isValidRow = nextSquare.row < 8 || nextSquare.row >= 0
    val isValidCol = nextSquare.col < 8 || nextSquare.col >= 0
    if (isValidRow && isValidCol) {
      Some(nextSquare)
    } else {
      None
    }
  }
}
