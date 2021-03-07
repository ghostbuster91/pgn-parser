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
import chessmodel.Player.Black
import chessmodel.Player.White

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

  private def findPawnMoveSource( //Move to engine
      board: Board,
      dest: Coordinate,
      currentPlayer: Player
  ): Coordinate = {
    board.peaces
      .collect { case (c, PlayerPeace(Peace.Pawn, `currentPlayer`)) => c }
      .find(src => Engine.isPawnEligibleToMove(src, dest, board, currentPlayer))
      .getOrElse(
        throw new RuntimeException(
          s"Couldn't find a pawn to move to ${Position.fromCoord(dest)} by ${currentPlayer}"
        )
      )
  }

  private def findPawnCaptureSourec( //TODO add bicie w przelocie
      board: Board,
      dest: Coordinate,
      currentPlayer: Player,
      sourceCol: Char
  ): Coordinate = {
    board.peaces
      .collect {
        case (coord, PlayerPeace(Peace.Pawn, currentPlayer))
            if coord.col == columnToInt(sourceCol) =>
          coord
      }
      .find { source =>
        Engine.isPawnEligibleToCapture(
          source,
          dest,
          board,
          currentPlayer
        )
      }
      .getOrElse(
        throw new RuntimeException(
          s"Couldn't find a pawn to capture on ${Position
            .fromCoord(dest)} by ${currentPlayer} from ${sourceCol}"
        )
      )
  }

  private def columnToInt(char: Char): Int = { //TODO create proper type for column
    char match {
      case 'a' => 0
      case 'b' => 1
      case 'c' => 2
      case 'd' => 3
      case 'e' => 4
      case 'f' => 5
      case 'g' => 6
      case 'h' => 7
    }
  }

  private def rowToInt(char: Char): Int = { //TODO create proper type for row
    char match {
      case '1' => 0
      case '2' => 1
      case '3' => 2
      case '4' => 3
      case '5' => 4
      case '6' => 5
      case '7' => 6
      case '8' => 7
    }
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
          Position(srcCol, srcRow),
          isCapture = sanMove.isCapture
        )
      case (Some(srcCol), None) =>
        handleFigureMoveGeneric(game, sanMove) {
          case (c, PlayerPeace(sanMove.figure, game.currentPlayer))
              if c.col == columnToInt(srcCol) =>
            c
        }
      case (None, Some(srcRow)) =>
        handleFigureMoveGeneric(game, sanMove) {
          case (coord, PlayerPeace(sanMove.figure, game.currentPlayer))
              if coord.row == rowToInt(srcRow) =>
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
      .filter { coord =>
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
      } match {
      case head :: Nil => head
      case multiple @ _ :: _ =>
        throw new RuntimeException("Multiple ambigious figures to move")
      case Nil => throw new RuntimeException("No eligible figures to move")
    }
  }
}
