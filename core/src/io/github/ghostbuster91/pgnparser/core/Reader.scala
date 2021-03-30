package io.github.ghostbuster91.pgnparser.core

import chessmodel._
import chessmodel.position._
import chessmodel.coordinate.Coordinate

trait Reader {
  def read(game: String): Either[ParsingException, ChessGame]
}

case class ParsingException(reason: String) extends RuntimeException(reason)

case class ChessGame(
    board: Board,
    currentPlayer: Player,
    previousState: Option[(ChessGame, LanMove)]
) {
  def move(move: LanMove): ChessGame =
    ChessGame(
      board.move(lanMoveToMove(move)),
      currentPlayer.opponent,
      Some(this -> move)
    )

  private def lanMoveToMove(lan: LanMove): List[Transformation] =
    lan match {
      case LanMove.FigureMove(figure, destitnation, _, source, _) =>
        List(
          Transformation.Move(
            source.toCoord(),
            destitnation.toCoord(),
            PlayerPiece(figure, currentPlayer)
          )
        )
      case LanMove.PawnCapture(destitnation, source, _, promotion) =>
        board.getSquare(destitnation.toCoord()) match {
          case Some(_) =>
            List(
              Transformation.Move(
                source.toCoord(),
                destitnation.toCoord(),
                PlayerPiece(promotion.getOrElse(Piece.Pawn), currentPlayer)
              )
            )
          case None =>
            List(
              Transformation.Move(
                source.toCoord(),
                destitnation.toCoord(),
                PlayerPiece(promotion.getOrElse(Piece.Pawn), currentPlayer)
              ),
              Transformation.Remove(
                Coordinate(destitnation.toCoord().col, source.toCoord().row)
              )
            )
        }
      case LanMove.PawnMove(source, destitnation, _, promotion) =>
        List(
          Transformation.Move(
            source.toCoord(),
            destitnation.toCoord(),
            PlayerPiece(promotion.getOrElse(Piece.Pawn), currentPlayer)
          )
        )
      case LanMove.QueenSideCastle(_) =>
        currentPlayer match {
          case Player.Black => queenSideCastle(Rank('8'))
          case Player.White => queenSideCastle(Rank('1'))
        }
      case LanMove.KingSideCastle(_) =>
        currentPlayer match {
          case Player.Black => kingSideCastle(Rank('8'))
          case Player.White => kingSideCastle(Rank('1'))
        }
    }

  private def queenSideCastle(row: Rank) =
    List(
      Transformation.Move(
        Position(File('e'), row).toCoord(),
        Position(File('c'), row).toCoord(),
        PlayerPiece(Figure.King, currentPlayer)
      ),
      Transformation.Move(
        Position(File('a'), row).toCoord(),
        Position(File('d'), row).toCoord(),
        PlayerPiece(Figure.Rook, currentPlayer)
      )
    )

  private def kingSideCastle(row: Rank) =
    List(
      Transformation.Move(
        Position(File('e'), row).toCoord(),
        Position(File('g'), row).toCoord(),
        PlayerPiece(Figure.King, currentPlayer)
      ),
      Transformation.Move(
        Position(File('h'), row).toCoord(),
        Position(File('f'), row).toCoord(),
        PlayerPiece(Figure.Rook, currentPlayer)
      )
    )
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
      source: Position,
      isCapture: Boolean
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
