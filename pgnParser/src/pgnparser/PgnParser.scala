package pgnparser

import cats.parse.{Parser => P, _}
import cats.syntax.all._
import Move.PromotionCapture

object PgnParser {
  val whitespace = P.charIn(" \t\n").void
  val spaceChar = P.char(' ')
  def parened[T](p: P[T]) = p.between(P.char('['), P.char(']'))
  def quotes[T](p: P[T]) = p.between(P.char('"'), P.char('"'))
  val string = P.charsWhile(c => c >= ' ' && c != '"' && c != '\\')
  def property(p: String) = parened(
    (P.string(p) *> spaceChar *> quotes(string))
  )
  val event = property("Event")
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
  val simpleMove =
    (figure.?.with1 ~ (capture.with1 ~ (position ~ checkRule))).map {
      case (f, (capture, (pos, check))) =>
        Move.SimpleMove(f, pos, capture, check): Move
    }
  val promotionCapture =
    (column <* P.char('x')).map(PromotionCapture(_))
  val promotion =
    (
      (promotionCapture.map(Some(_)) ~ position).backtrack.orElse1(
        position.map(a => Option.empty[PromotionCapture] -> a)
      ),
      (P.char('=') *> figure) ~ checkRule
    ).tupled
      .map { case ((capture, pos), (f, check)) =>
        Move.Promotion(pos, f, capture, check): Move
      }
  val move = promotion.backtrack.orElse1(simpleMove)
  val round =
    (
      roundNumber <* P.char('.'),
      (whitespace *> move) ~ (whitespace *> move).?
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

  val roundsWithScore = rounds ~ score

}

case class PgnGame(moves: List[Round])
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
  case class SimpleMove(
      figure: Option[Figure],
      destitnation: Position,
      isCapture: Boolean,
      check: Check
  ) extends Move

  case class Promotion(
      target: Position,
      figure: Figure,
      capture: Option[PromotionCapture],
      check: Check
  ) extends Move

  case class PromotionCapture(sourceRow: Char)
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
