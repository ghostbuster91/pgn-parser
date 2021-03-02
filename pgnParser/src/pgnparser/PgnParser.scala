package pgnparser

import cats.parse.{Parser => P, Parser1 => P1, _}
import cats.syntax.all._
import Move.Capture

object PgnParser {
  val whitespace = P.charIn(" \t\n")
  def parened[T](p: P[T]) = p.between(P.char('['), P.char(']'))
  val quotedString =
    P.char('"') *> P.charsWhile1(c => c != '"').map(_.mkString) <* P.char('"')
  val string = P.charsWhile1(c => c != ' ').map(_.mkString)
  val property = parened(
    (string <* whitespace) ~ quotedString
  ).map { case (k, v) => Meta(k, v) }
  val properties =
    (property.with1 <* P.char('\n')).backtrack.rep
  val column = P.charIn("abcdefgh")
  val digit = P.charIn("0123456789")
  val row = P.charIn("12345678")
  val position = (column ~ row).map { case (c, r) => Position(c, r) }
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

  val pawnMove = (position ~ checkRule).map { case (pos, check) =>
    Move.PawnMove(pos, check, None): Move
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
      SourceDest(Some(p1.column), Some(p1.row), p2)
    })

  //Nf3, Nbf3
  val figureMove = (figure ~ sourceDest ~ checkRule).map {
    case ((f, pos), check) =>
      Move.FigureMove(f, pos.position, check, pos.row, pos.col)
  }

//Bdxd4
  val figureCapture =
    ((figure <* P.char('x')) ~ sourceDest ~ checkRule).map {
      case ((f, pos), check) =>
        Move.FigureCapture(pos.position, f, check, pos.row, pos.row): Move
    }
  //cxd4
  val pawnCapture = ((column <* P.char('x')) ~ position ~ checkRule).map {
    case ((c, pos), check) => Move.PawnCapture(pos, c, check, None): Move
  }
  val promotionCapture =
    (column <* P.char('x')).map(Capture(_))
  val promotion =
    (
      (promotionCapture.map(Some(_)) ~ position).backtrack.orElse1(
        position.map(a => Option.empty[Capture] -> a)
      ),
      (P.char('=') *> figure) ~ checkRule
    ).tupled
      .map {
        case ((Some(cap), pos), (f, check)) =>
          Move.PawnCapture(pos, cap.sourceRow, check, Some(f))
        case ((None, pos), (f, check)) =>
          Move.PawnMove(pos, check, Some(f))
      }
  val move = promotion.backtrack.orElse1(
    pawnMove.backtrack.orElse1(
      figureCapture.backtrack.orElse1(pawnCapture.backtrack.orElse1(figureMove))
    )
  )
  val round =
    (
      roundNumber <* P.char('.'),
      (whitespace *> move) ~ (whitespace *> move).backtrack.?
    ).tupled
      .map { case (round, (move1, move2)) =>
        Round(
          round,
          move1,
          move2
        )
      }
  val score =
    P.string("0-1")
      .as(Score.BlackWins: Score)
      .orElse(P.string("1-0").as(Score.WhiteWins: Score))
      .orElse(P.string("1/2-1/2").as(Score.Draw: Score))

  val rounds =
    (round <* whitespace).backtrack.rep

  val pgnGame =
    P.char('\n').? *> ((properties <* P.char('\n')).? ~ rounds ~ score).map {
      case ((props, rounds), score) =>
        PgnGame(props.getOrElse(List.empty), rounds, score)
    }
}
case class SourceDest(col: Option[Char], row: Option[Char], position: Position)
case class Round(number: Int, firstMove: Move, secondMove: Option[Move])
sealed trait Move {
  def check: Check
}

sealed trait Check
object Check {
  case object NoCheck extends Check
  case object SimpleCheck extends Check
  case object Checkmate extends Check
}

object Move {
  case class PawnMove(
      destitnation: Position,
      check: Check,
      promotion: Option[Figure]
  ) extends Move

  case class FigureMove(
      figure: Figure,
      destitnation: Position,
      check: Check,
      sourceRow: Option[Char],
      sourceCol: Option[Char]
  ) extends Move

  case class FigureCapture(
      destitnation: Position,
      figure: Figure,
      check: Check,
      sourceRow: Option[Char],
      sourceCol: Option[Char]
  ) extends Move

  case class PawnCapture(
      destitnation: Position,
      sourceRow: Char,
      check: Check,
      promotion: Option[Figure]
  ) extends Move

  case class Capture(sourceRow: Char)
}

case class Position(row: Char, column: Char)

sealed trait Figure

object Figure {
  case object King extends Figure
  case object Queen extends Figure
  case object Bishop extends Figure
  case object Knight extends Figure
  case object Rook extends Figure
}

sealed trait Score
object Score {
  case object WhiteWins extends Score
  case object BlackWins extends Score
  case object Draw extends Score
}
case class Meta(key: String, value: String)
case class PgnGame(meta: List[Meta], rounds: List[Round], score: Score)
