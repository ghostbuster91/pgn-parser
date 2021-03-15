package io.github.ghostbuster91.pgnparser.pgnparser

import cats.parse.{Parser => P}
import cats.syntax.all._
import chessmodel._
import chessmodel.position._

object PgnParser {
  val whitespace = P.charIn(" \t\n")
  def parened[T](p: P[T]) = p.between(P.char('['), P.char(']'))
  val quotedString =
    P.char('"') *> P.charsWhile1(c => c != '"').map(_.mkString) <* P.char('"')
  val string = P.charsWhile1(c => c != ' ').map(_.mkString)
  val property = parened(
    (string <* whitespace) ~ quotedString
  ).map { case (k, v) => Tag(k, v) }
  val properties =
    (property.with1 <* P.char('\n')).backtrack.rep
  val column = P.charIn("abcdefgh").map(File.apply)
  val digit = P.charIn("0123456789")
  val row = P.charIn("12345678").map(Rank.apply)
  val position = (column ~ row).map { case (c, r) =>
    Position(c, r)
  }
  val roundNumber = digit.rep1.map(_.mkString_("").toInt)
  val figure = P.charIn("KQNBR").map {
    case 'K' => Figure.King
    case 'Q' => Figure.Queen
    case 'N' => Figure.Knight
    case 'B' => Figure.Bishop
    case 'R' => Figure.Rook
  }
  val check = P.char('+').as[Check](Check.SimpleCheck)
  val checkmate = P.char('#').as[Check](Check.Checkmate)
  val checkRule = (check.orElse(checkmate)).?.map(_.getOrElse(Check.NoCheck))
  val capture = P.char('x').as(true).?.map(_.getOrElse(false))

  val pawnPromo = (P.char('=') *> figure).?
  val pawnMove = (position ~ pawnPromo ~ checkRule).map {
    case ((pos, f), check) =>
      SanMove.PawnMove(pos, check, f): SanMove
  }
  val sourceDest = position
    .map(p => SourceDest(None, None, p))
    .backtrack
    .orElse1((column ~ position).map { case (c, p) =>
      SourceDest(Some(c), None, p)
    })
    .backtrack
    .orElse1((row ~ position).map { case (r, p) =>
      SourceDest(None, Some(r), p)
    })
    .backtrack
    .orElse1((position ~ position).map { case (p1, p2) =>
      SourceDest(Some(p1.file), Some(p1.rank), p2)
    })

  val source = position.backtrack
    .map(p => Source(Some(p.file), Some(p.rank)))
    .orElse1(column.map(c => Source(Some(c), None)))
    .orElse1(row.map(r => Source(None, Some(r))))

  //Nf3, Nbf3
  val figureMove = (figure ~ sourceDest ~ checkRule).map {
    case ((f, pos), check) =>
      SanMove.FigureMove(
        f,
        pos.position,
        check,
        pos.rank,
        pos.file,
        isCapture = false
      )
  }

//Bdxd4
  val figureCapture =
    ((figure ~ source.backtrack.? <* P.char('x')) ~ position ~ checkRule).map {
      case (((f, src), pos), check) =>
        SanMove.FigureMove(
          f,
          pos,
          check,
          src.flatMap(_.row),
          src.flatMap(_.col),
          isCapture = true
        ): SanMove
    }
  //cxd4
  val pawnCapture =
    ((column <* P.char('x')) ~ position ~ pawnPromo ~ checkRule)
      .map { case (((c, pos), f), check) =>
        SanMove.PawnCapture(pos, c, check, f): SanMove
      }
  val kingSideCastle =
    (P.string1("O-O") *> checkRule).map(check =>
      SanMove.KingSideCastle(check): SanMove
    )
  val queenSideCastle = (P.string1("O-O-O") *> checkRule).map(check =>
    SanMove.QueenSideCastle(check): SanMove
  )

  val move = pawnMove.backtrack
    .orElse1(figureCapture.backtrack)
    .orElse1(pawnCapture.backtrack)
    .orElse1(figureMove.backtrack)
    .orElse1(queenSideCastle.backtrack)
    .orElse1(kingSideCastle)

  val round =
    (
      roundNumber <* P.char('.'),
      (whitespace *> move) ~ (whitespace *> move).backtrack.?
    ).tupled
      .map { case (round, (move1, move2)) =>
        Round(round, move1, move2)
      }
  val score =
    P.string("0-1")
      .as(GameResult.BlackWins: GameResult)
      .orElse(P.string("1-0").as(GameResult.WhiteWins: GameResult))
      .orElse(P.string("1/2-1/2").as(GameResult.Draw: GameResult))
      .orElse(P.char('*').as(GameResult.Unfinished: GameResult))

  val rounds =
    (round <* whitespace).backtrack.rep

  val pgnGame =
    P.char('\n').? *> ((properties <* P.char('\n')).? ~ rounds ~ score <* P.end)
      .map { case ((props, rounds), score) =>
        PgnGame(props.getOrElse(List.empty), rounds, score)
      }
}
case class Source(col: Option[File], row: Option[Rank])
case class SourceDest(
    file: Option[File],
    rank: Option[Rank],
    position: Position
)
case class Round(number: Int, firstMove: SanMove, secondMove: Option[SanMove])
sealed trait SanMove {
  def check: Check
}

object SanMove {
  case class PawnMove(
      destitnation: Position,
      check: Check,
      promotion: Option[Figure]
  ) extends SanMove

  case class FigureMove(
      figure: Figure,
      destitnation: Position,
      check: Check,
      sourceRow: Option[Rank],
      sourceCol: Option[File],
      isCapture: Boolean
  ) extends SanMove

  case class PawnCapture(
      destitnation: Position,
      sourceCol: File,
      check: Check,
      promotion: Option[Figure]
  ) extends SanMove

  case class QueenSideCastle(check: Check) extends SanMove
  case class KingSideCastle(check: Check) extends SanMove
}

sealed trait GameResult
object GameResult {
  case object WhiteWins extends GameResult
  case object BlackWins extends GameResult
  case object Draw extends GameResult
  case object Unfinished extends GameResult
}
case class Tag(key: String, value: String)
case class PgnGame(meta: List[Tag], rounds: List[Round], result: GameResult)
