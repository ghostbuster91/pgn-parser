package pgnparser
import cats.parse.{Parser => P, _}
import cats.syntax.all._
import utest._
import com.softwaremill.diffx.generic.auto._
import com.softwaremill.diffx.utest.DiffxAssertions._

object PgnParserTest extends TestSuite {
  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val spaceChar: P[Unit] = P.char(' ')
  def parened[T](p: P[T]) = p.between(P.char('['), P.char(']'))
  def quotes[T](p: P[T]) = p.between(P.char('"'), P.char('"'))
  val string = P.charsWhile(c => c >= ' ' && c != '"' && c != '\\')
  def property(p: String) = parened(
    (P.string(p) *> spaceChar *> quotes(string))
  )
  val event = property("Event")
  val column = P.charIn("abcdefgh")
  val digit = P.charIn("12345678")
  val row = digit
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
  val move =
    (figure.?, position, (check.orElse(checkmate)).?).tupled.map {
      case (f, pos, check) =>
        Move.SimpleMove(f, pos, false, check.getOrElse(Check.NoCheck))
    }
  val round =
    (
      roundNumber <* P.char('.'),
      (whitespace *> move),
      (whitespace *> move).?
    ).tupled
      .map { case (round, move1, move2) =>
        Round(
          round,
          move1,
          move2
        )
      }

  val tests = Tests {
    "parse event property" - {
      val input = """[Event "Rated Blitz game"]"""
      assert(event.parse(input) == Right("" -> "Rated Blitz game"))
    }
    "parse round - pawns moves only" - {
      val input = "1. e4 e6"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            1,
            Move.SimpleMove(None, Position('e', '4'), false, Check.NoCheck),
            Some(
              Move.SimpleMove(None, Position('e', '6'), false, Check.NoCheck)
            )
          )
        )
      )
    }
    "parse round - figure move" - {
      val input = "1. Nf3 e6"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            1,
            Move.SimpleMove(
              Some(Figure.Knight),
              Position('f', '3'),
              false,
              Check.NoCheck
            ),
            Some(
              Move.SimpleMove(None, Position('e', '6'), false, Check.NoCheck)
            )
          )
        )
      )
    }
    "parse round - move with check" - {
      val input = "36. Ra6+ Kc5"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            36,
            Move.SimpleMove(
              Some(Figure.Rook),
              Position('a', '6'),
              false,
              Check.SimpleCheck
            ),
            Some(
              Move.SimpleMove(
                Some(Figure.King),
                Position('c', '5'),
                false,
                Check.NoCheck
              )
            )
          )
        )
      )
    }
    "parse round - move with checkmate" - {
      val input = "36. Ra6 Kc5#"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            36,
            Move.SimpleMove(
              Some(Figure.Rook),
              Position('a', '6'),
              false,
              Check.NoCheck
            ),
            Some(
              Move.SimpleMove(
                Some(Figure.King),
                Position('c', '5'),
                false,
                Check.Checkmate
              )
            )
          )
        )
      )
    }
    "parse round - single move" - {
      val input = "36. Ra6"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            36,
            Move.SimpleMove(
              Some(Figure.Rook),
              Position('a', '6'),
              false,
              Check.NoCheck
            ),
            None
          )
        )
      )
    }
  }
}

case class PgnGame(moves: List[Round])
case class Round(number: Int, firstMove: Move, secondMove: Option[Move])
sealed trait Move {
  def check: Check
  def isCapture: Boolean
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
      destitnation: Position,
      figure: Figure,
      isCapture: Boolean,
      check: Check
  ) extends Move
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
