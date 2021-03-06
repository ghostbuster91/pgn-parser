package chesslib

import chessmodel._

object Engine {

  def isEligibleToMove(
      from: Coordinate,
      to: Coordinate,
      board: Board,
      figure: Figure,
      currentPlayer: Player
  ): Boolean = {
    val figureRule = isEligibleToMove2(from, to, board, figure, currentPlayer)
    if (figureRule) {
      val afterMove =
        board.move(Move(from, to, PlayerPeace(figure, currentPlayer)))
      isEligibleToMoveKingRule(afterMove, currentPlayer)
    } else {
      false
    }
  }

  private def isEligibleToMoveKingRule(
      board: Board,
      currentPlayer: Player
  ): Boolean = {
    val kingLocation = board.peaces
      .collectFirst { case (coord, PlayerPeace(Figure.King, `currentPlayer`)) =>
        coord
      }
      .getOrElse(
        throw new RuntimeException(
          s"Couldn't find ${currentPlayer}'s king on board"
        )
      )
    !isSquareCheckedBy(kingLocation, board, currentPlayer.opponent)
  }

  private def isEligibleToMove2(
      from: Coordinate,
      to: Coordinate,
      board: Board,
      figure: Figure,
      currentPlayer: Player
  ): Boolean = {
    val r1 = figure match {
      case Figure.King =>
        throw new RuntimeException("Not implemented by design")
      case Figure.Queen =>
        val bishop = isBishopEligibleToMove(from, to, board)
        val rook = isRookEligibleToMove(from, to, board)
        rook || bishop
      case Figure.Bishop => isBishopEligibleToMove(from, to, board)
      case Figure.Knight => isKnightEligibleToMove(from, to)
      case Figure.Rook   => isRookEligibleToMove(from, to, board)
    }

    r1 && evilOrEmpty(to, board, currentPlayer)
  }

  private def isBishopEligibleToMove(
      from: Coordinate,
      to: Coordinate,
      board: Board
  ): Boolean = {
    val cCandidates = (-7) until 8
    // y = x + c
    val positive = cCandidates.exists { c =>
      from.row == from.col + c && to.row == to.col + c
    }
    // y = -x +c
    val negative = cCandidates.map(_ + 7).exists { c =>
      from.row == -from.col + c && to.row == -to.col + c
    }
    if (positive || negative) {
      val dir = if (positive) {
        if (from.col < to.col) {
          Direction.NorthEast
        } else {
          Direction.SouthWest
        }
      } else if (negative) {
        if (from.col < to.col) {
          Direction.SouthEast
        } else {
          Direction.NorthWest
        }
      } else {
        throw new RuntimeException("qwe")
      }
      val squaresBetween = LazyList
        .unfold(from)(s => s.shift(dir.shift).map(c => c -> c))
        .takeWhile(c => c != to)
        .drop(1)
      noFiguresAt(squaresBetween, board)
    } else {
      false
    }
  }

  private def evilOrEmpty(
      coord: Coordinate,
      board: Board,
      currentPlayer: Player
  ): Boolean = {
    board
      .getSquare(coord)
      .forall(pp => pp.player == currentPlayer.opponent)
  }

  private def noFiguresAt(
      squares: Iterable[Coordinate],
      board: Board
  ): Boolean =
    squares.forall(board.getSquare(_).isEmpty)

  private def isKnightEligibleToMove(
      from: Coordinate,
      to: Coordinate
  ): Boolean = {
    getPossibleKnightMoves(from)
      .contains(to)
  }
  private def isRookEligibleToMove( //return list of squares between instead
      from: Coordinate,
      to: Coordinate,
      board: Board
  ): Boolean = {
    if (from.col == to.col) {
      val squares = ((Math.min(from.row, to.row) + 1) until Math.max(
        from.row,
        to.row
      )).map(row => Coordinate(row, from.col))
      noFiguresAt(squares, board)
    } else if (from.row == to.row) {
      val squares = ((Math.min(from.col, to.col) + 1) until Math.max(
        from.col,
        to.col
      )).map(col => Coordinate(from.row, col))
      noFiguresAt(squares, board)
    } else {
      false
    }
  }

  def isSquareCheckedBy(
      target: Coordinate,
      board: Board,
      opponent: Player
  ): Boolean = {
    val knightCheck = isSquareCheckedFromKnight(
      target,
      board,
      opponent
    )
    val rayCheck = Direction.values.toList.exists(d =>
      isSquareCheckedFromDirection(target, board, opponent, d)
    )
    knightCheck || rayCheck //TODO lazy?
  }

  private def isSquareCheckedFromKnight(
      target: Coordinate,
      board: Board,
      opponent: Player
  ): Boolean = {
    val possibleKnightPlaces = getPossibleKnightMoves(target)
    possibleKnightPlaces
      .flatMap(board.getSquare)
      .exists(pp => pp.player == opponent && pp.peace == Figure.Knight)
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
      opponent: Player,
      direction: Direction
  ): Boolean = {
    val friend = opponent.opponent
    getFirstPeace(target, board, direction) match {
      case Some((_, PlayerPeace(_, `friend`))) => false
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
