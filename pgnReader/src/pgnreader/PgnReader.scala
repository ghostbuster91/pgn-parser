package pgnreader

import core.Reader
import core.LanMove
import pgnparser._
import core.ChessGame
import core.ParsingException
import cats.syntax.all._
import chessmodel._
import chesslib._
import scala.collection.immutable

class PgnReader extends Reader {
  def read(pgnString: String): Either[ParsingException, ChessGame] = {
    val result = PgnParser.pgnGame.parseAll(pgnString)
    result match {
      case Left(value) =>
        Left(
          ParsingException(
            s"${value.failedAtOffset} ${value.expected.map(_.toString()).mkString_(", ")}"
          )
        )
      case Right(pgnGame) =>
        val pgnMoves = pgnGame.rounds
          .flatMap(r => List(r.firstMove) ++ r.secondMove)
        Right(pgnMoves.foldLeft(ChessGame.Starting) { case (game, sanMove) =>
          val lanMove = toLanMove(game, sanMove)
          game.move(lanMove)
        })
    }
  }

  def toLanMove(
      game: ChessGame,
      sanMove: SanMove
  ): LanMove = {
    sanMove match {
      case spm: SanMove.PawnMove          => handlePawnMove(spm, game)
      case sfm: SanMove.FigureMove        => handleFigureMove(sfm, game)
      case spc: SanMove.PawnCapture       => handlePawnCapture(spc, game)
      case SanMove.QueenSideCastle(check) => LanMove.QueenSideCastle(check)
      case SanMove.KingSideCastle(check)  => LanMove.KingSideCastle(check)
    }
  }

  private def handlePawnMove(sanMove: SanMove.PawnMove, game: ChessGame) = {
    val coordDest = sanMove.destitnation.toCoord()
    val source =
      findPawnMoveSource(game.board, coordDest, game.currentPlayer)
    LanMove.PawnMove(
      Position.fromCoord(source),
      sanMove.destitnation,
      sanMove.check,
      sanMove.promotion
    )
  }

  private def handlePawnCapture(
      sanMove: SanMove.PawnCapture,
      game: ChessGame
  ) = {
    val coordDest = sanMove.destitnation.toCoord()
    val source = findPawnCaptureSourec(
      game.board,
      coordDest,
      game.currentPlayer,
      sanMove.sourceCol
    )
    LanMove.PawnCapture(
      sanMove.destitnation,
      Position.fromCoord(source),
      sanMove.check,
      sanMove.promotion
    )
  }

  private def findPawnMoveSource(
      board: Board,
      dest: Coordinate,
      currentPlayer: Player
  ): Coordinate = {
    board.peaces
      .collect { case (c, PlayerPeace(Peace.Pawn, `currentPlayer`)) => c }
      .find(src => src.col == dest.col && src.row == dest.row - 1)
      .getOrElse(
        throw new RuntimeException(
          s"Couldn't find a pawn to move to ${dest} by ${currentPlayer}"
        )
      )
  }
  private def findPawnCaptureSourec(
      board: Board,
      dest: Coordinate,
      currentPlayer: Player,
      sourceCol: Char
  ): Coordinate = {
    board.peaces
      .collect { case (coord, PlayerPeace(Peace.Pawn, currentPlayer)) =>
        coord
      }
      .find(source => source.row == dest.row - 1 && source.col == sourceCol)
      .getOrElse(
        throw new RuntimeException(
          s"Couldn't find a pawn to capture on ${dest} by ${currentPlayer} from ${sourceCol}"
        )
      )
  }

  private def handleFigureMove(
      sanMove: SanMove.FigureMove,
      game: ChessGame
  ): LanMove.FigureMove = {
    (sanMove.sourceCol, sanMove.sourceRow) match {
      case (Some(srcCol), Some(srcRow)) =>
        LanMove.FigureMove(
          sanMove.figure,
          sanMove.destitnation,
          sanMove.check,
          Position.fromCoord(Coordinate(srcRow, srcCol)),
          isCapture = sanMove.isCapture
        )
      case (Some(srcCol), None) =>
        handleFigureMoveGeneric(game, sanMove) {
          case (c, PlayerPeace(sanMove.figure, game.currentPlayer))
              if c.col == srcCol =>
            c
        }
      case (None, Some(srcRow)) =>
        handleFigureMoveGeneric(game, sanMove) {
          case (coord, PlayerPeace(sanMove.figure, game.currentPlayer))
              if coord.row == srcRow =>
            coord
        }
      case (None, None) =>
        handleFigureMoveGeneric(game, sanMove) {
          case (c, PlayerPeace(sanMove.figure, game.currentPlayer)) => c
        }
    }
  }

  private def handleFigureMoveGeneric(
      game: ChessGame,
      sanMove: SanMove.FigureMove
  )(
      f: PartialFunction[(Coordinate, PlayerPeace), Coordinate]
  ) = {
    val candidates = game.board.peaces.toList.collect(f)
    handleFigureMoveCandidates(game, sanMove, candidates)
  }

  private def handleFigureMoveCandidates(
      game: ChessGame,
      sanMove: SanMove.FigureMove,
      candidates: List[Coordinate]
  ) = {
    candidates match {
      case coord :: Nil =>
        LanMove.FigureMove(
          sanMove.figure,
          sanMove.destitnation,
          sanMove.check,
          Position.fromCoord(coord),
          sanMove.isCapture
        )
      case multiple @ _ :: _ => handleAmbigiousMove(game, sanMove, multiple)
      case Nil               => throw new RuntimeException("No figures to move")
    }
  }

  private def handleAmbigiousMove(
      game: ChessGame,
      sanMove: SanMove.FigureMove,
      candidates: List[Coordinate]
  ) = {
    candidates
      .find { coord =>
        Engine.isEligibleToMove(
          coord,
          sanMove.destitnation.toCoord(),
          game.board,
          sanMove.figure,
          game.currentPlayer
        )
      }
      .map { source =>
        LanMove.FigureMove(
          sanMove.figure,
          sanMove.destitnation,
          sanMove.check,
          Position.fromCoord(source),
          isCapture = sanMove.isCapture
        )
      }
      .getOrElse(
        throw new RuntimeException("No eligible figures to move")
      ) //TODO handle multiple
  }
}
