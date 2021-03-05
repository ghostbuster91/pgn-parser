package chesslib

import chessmodel._

object Engine {

  def isSquareChecked(
      target: Coordinate,
      board: Board,
      currentPlayer: Player
  ): Boolean = {
    val knightCheck = isSquareCheckedFromKnight(
      target,
      board,
      currentPlayer
    )
    val rayCheck = Direction.values.toList.exists(d =>
      isSquareCheckedFromDirection(target, board, currentPlayer, d)
    )
    knightCheck || rayCheck //TODO lazy?
  }

  private def isSquareCheckedFromKnight(
      target: Coordinate,
      board: Board,
      currentPlayer: Player
  ): Boolean = {
    val possibleKnightPlaces = getPossibleKnightMoves(target)
    possibleKnightPlaces
      .flatMap(board.getSquare)
      .exists(pp => pp.player != currentPlayer && pp.peace == Figure.Knight)
  }

  private def getPossibleKnightMoves(from: Coordinate): List[Coordinate] = {
    val shifts =
      List(Shift(+2, +1), Shift(+2, -1), Shift(-2, +1), Shift(-2, -1)) ++ List(
        Shift(+1, +2),
        Shift(+1, -2),
        Shift(-1, +2),
        Shift(-1, -2)
      )

    shifts.flatMap(s => from.shift(s))
  }

  private def isSquareCheckedFromDirection(
      target: Coordinate,
      board: Board,
      currentPlayer: Player,
      direction: Direction
  ): Boolean = {
    getFirstPeace(target, board, direction) match {
      case Some((_, PlayerPeace(_, `currentPlayer`))) => false
      case Some((distance, PlayerPeace(peace, opponent))) =>
        peace match {
          case Peace.Pawn =>
            distance == 1 && isPawnThreat(direction.opposite, opponent)
          case Figure.Bishop => isBishopThreat(direction.opposite)
          case Figure.Rook   => isRookThreat(direction.opposite)
          case Figure.Queen  => true
          case Figure.King   => distance == 1
          case Figure.Knight => false
        }
      case None => false
    }
  }

  private def isBishopThreat(direction: Direction) =
    direction.isInstanceOf[Diagonal]

  private def isRookThreat(direction: Direction) =
    direction.isInstanceOf[File] || direction.isInstanceOf[Rank]

  private def isPawnThreat(direction: Direction, opponent: Player) = {
    opponent match {
      case Player.Black =>
        direction match {
          case Direction.SouthEast => true
          case Direction.SouthWest => true
          case _                   => false
        }
      case Player.White =>
        direction match {
          case Direction.NorthEast => true
          case Direction.NorthWest => true
          case _                   => false
        }
    }
  }

  type Distance = Int

  private def getFirstPeace(
      coords: Coordinate,
      board: Board,
      direction: Direction,
      distance: Distance = 1
  ): Option[(Distance, PlayerPeace)] = {
    if (distance <= 8) {
      coords
        .shift(direction.shift)
        .flatMap(nextSquare =>
          board
            .getSquare(nextSquare)
            .map(p => distance -> p)
            .orElse(getFirstPeace(nextSquare, board, direction, distance + 1))
        )
    } else {
      Option.empty
    }
  }
}
