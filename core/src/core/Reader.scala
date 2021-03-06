package core

import chessmodel._

trait Reader {
  def read(game: String): Either[ParsingException, ChessGame]
}

case class ParsingException(reason: String) extends RuntimeException

case class ChessGame(
    board: Board,
    currentPlayer: Player,
    previousState: Option[(ChessGame, LanMove)]
) {
  def move(move: LanMove): ChessGame = {
    ChessGame(
      board.move(lanMoveToMove(move)),
      currentPlayer.opponent,
      Some(this -> move)
    )
  }

  private def lanMoveToMove(lan: LanMove): List[Move] = {
    lan match {
      case LanMove.FigureMove(figure, destitnation, check, source) =>
        List(
          Move(
            source.toCoord(),
            destitnation.toCoord(),
            PlayerPeace(figure, currentPlayer)
          )
        )
      case LanMove.FigureCapture(destitnation, figure, check, source) =>
        List(
          Move(
            source.toCoord(),
            destitnation.toCoord(),
            PlayerPeace(figure, currentPlayer)
          )
        )
      case LanMove.PawnCapture(destitnation, source, check, promotion) =>
        List(
          Move(
            source.toCoord(),
            destitnation.toCoord(),
            PlayerPeace(promotion.getOrElse(Peace.Pawn), currentPlayer)
          )
        )
      case LanMove.PawnMove(source, destitnation, check, promotion) =>
        List(
          Move(
            source.toCoord(),
            destitnation.toCoord(),
            PlayerPeace(promotion.getOrElse(Peace.Pawn), currentPlayer)
          )
        )
      case LanMove.QueenSideCastle(check) =>
        currentPlayer match {
          case Player.Black => queenSideCastle('8')
          case Player.White => queenSideCastle('1')
        }
      case LanMove.KingSideCastle(check) =>
        currentPlayer match {
          case Player.Black => kingSideCastle('8')
          case Player.White => kingSideCastle('1')
        }
    }
  }

  private def queenSideCastle(row: Char) = {
    List(
      Move(
        Position('e', row).toCoord(),
        Position('c', row).toCoord(),
        PlayerPeace(Figure.King, currentPlayer)
      ),
      Move(
        Position('a', row).toCoord(),
        Position('d', row).toCoord(),
        PlayerPeace(Figure.Rook, currentPlayer)
      )
    )
  }

  private def kingSideCastle(row: Char) = {
    List(
      Move(
        Position('e', row).toCoord(),
        Position('g', row).toCoord(),
        PlayerPeace(Figure.King, currentPlayer)
      ),
      Move(
        Position('h', row).toCoord(),
        Position('f', row).toCoord(),
        PlayerPeace(Figure.Rook, currentPlayer)
      )
    )
  }
}

object ChessGame {
  val Starting: ChessGame =
    ChessGame(Board.Starting, Player.White, None)
}

sealed trait LanMove {
  def check: Check
}

object LanMove {
  case class PawnMove(
      source: Position,
      destitnation: Position,
      check: Check,
      promotion: Option[Figure]
  ) extends LanMove

  case class FigureMove(
      figure: Figure,
      destitnation: Position,
      check: Check,
      source: Position
  ) extends LanMove

  case class FigureCapture(
      destitnation: Position,
      figure: Figure,
      check: Check,
      source: Position
  ) extends LanMove

  case class PawnCapture(
      destitnation: Position,
      source: Position,
      check: Check,
      promotion: Option[Figure]
  ) extends LanMove

  case class QueenSideCastle(check: Check) extends LanMove
  case class KingSideCastle(check: Check) extends LanMove
}
