package pgnparser
import cats.parse.{Parser => P, _}
import cats.syntax.all._
import utest._
import com.softwaremill.diffx.generic.auto._
import com.softwaremill.diffx.ConsoleColorConfig
import com.softwaremill.diffx.utest.DiffxAssertions._
import Move.PromotionCapture
import cats.data.Op
import PgnParser._

object PgnParserTest extends TestSuite {
  implicit val c: ConsoleColorConfig =
    ConsoleColorConfig(x => s"-$x", x => s"+$x", identity, identity)

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
    "parse round - move with capture" - {
      val input = "19. Bxf3"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.SimpleMove(
              Some(Figure.Bishop),
              Position('f', '3'),
              true,
              Check.NoCheck
            ),
            None
          )
        )
      )
    }
    "parse round - promotion move" - {
      val input = "19. a1=Q"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.Promotion(
              Position('a', '1'),
              Figure.Queen,
              None,
              Check.NoCheck
            ),
            None
          )
        )
      )
    }
    "parse round - capture and check" - {
      val input = "9. Bxc6+"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            9,
            Move.SimpleMove(
              Some(Figure.Bishop),
              Position('c', '6'),
              true,
              Check.SimpleCheck
            ),
            None
          )
        )
      )
    }
    "parse round - promotion move with check" - {
      val input = "19. a1=Q+"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.Promotion(
              Position('a', '1'),
              Figure.Queen,
              None,
              Check.SimpleCheck
            ),
            None
          )
        )
      )
    }
    "parse round - promotion move with capture" - {
      val input = "19. axb8=Q"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.Promotion(
              Position('b', '8'),
              Figure.Queen,
              Some(PromotionCapture('a')),
              Check.NoCheck
            ),
            None
          )
        )
      )
    }
    "parse round - promotion move with capture and checkmate" - {
      val input = "19. axb8=Q#"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.Promotion(
              Position('b', '8'),
              Figure.Queen,
              Some(PromotionCapture('a')),
              Check.Checkmate
            ),
            None
          )
        )
      )
    }
    "parse score" - {
      assertEqual(score.parse("1-0"), Right("" -> Score.WhiteWins))
      assertEqual(score.parse("0-1"), Right("" -> Score.BlackWins))
      assertEqual(score.parse("1/2-1/2"), Right("" -> Score.Draw))
    }
    "parse multiple rounds" - {
      val input = "1. d4 d5 2. f4 e6 3. Nf4 g6 "
      val output = rounds.parse(input)
      assertEqual(
        output,
        Right(
          "" -> List(
            Round(
              1,
              Move.SimpleMove(
                None,
                Position('d', '4'),
                false,
                Check.NoCheck
              ),
              Some(
                Move.SimpleMove(
                  None,
                  Position('d', '5'),
                  false,
                  Check.NoCheck
                )
              )
            ),
            Round(
              2,
              Move.SimpleMove(
                None,
                Position('f', '4'),
                false,
                Check.NoCheck
              ),
              Some(
                Move.SimpleMove(
                  None,
                  Position('e', '6'),
                  false,
                  Check.NoCheck
                )
              )
            ),
            Round(
              3,
              Move.SimpleMove(
                Some(Figure.Knight),
                Position('f', '4'),
                false,
                Check.NoCheck
              ),
              Some(
                Move.SimpleMove(
                  None,
                  Position('g', '6'),
                  false,
                  Check.NoCheck
                )
              )
            )
          )
        )
      )
    }
    "parse rounds ended with score" - {
      val input = "1. d4 d5 1/2-1/2"
      val output = roundsWithScore.parse(input)
      assertEqual(
        output,
        Right(
          "" -> (List(
            Round(
              1,
              Move.SimpleMove(
                None,
                Position('d', '4'),
                false,
                Check.NoCheck
              ),
              Some(
                Move.SimpleMove(
                  None,
                  Position('d', '5'),
                  false,
                  Check.NoCheck
                )
              )
            )
          ) -> (Score.Draw: Score))
        )
      )
    }
  }
}
