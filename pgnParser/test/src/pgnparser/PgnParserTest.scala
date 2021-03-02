package pgnparser
import cats.parse.{Parser => P, _}
import cats.syntax.all._
import utest._
import com.softwaremill.diffx.generic.auto._
import com.softwaremill.diffx.ConsoleColorConfig
import com.softwaremill.diffx.utest.DiffxAssertions._
import PgnParser._

object PgnParserTest extends TestSuite {
  implicit val c: ConsoleColorConfig =
    ConsoleColorConfig(x => s"-$x", x => s"+$x", identity, identity)

  val tests = Tests {
    "parse event property" - {
      val input = """[Event "Rated Blitz game"]"""
      val output = property.parse(input)
      assertEqual(
        output,
        Right("" -> Meta("Event", "Rated Blitz game"))
      )
    }
    "parse round - pawns moves only" - {
      val input = "1. e4 e6"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            1,
            Move.PawnMove(Position('e', '4'), Check.NoCheck, None),
            Some(
              Move.PawnMove(Position('e', '6'), Check.NoCheck, None)
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
            Move.FigureMove(
              Figure.Knight,
              Position('f', '3'),
              Check.NoCheck,
              None,
              None
            ),
            Some(
              Move.PawnMove(Position('e', '6'), Check.NoCheck, None)
            )
          )
        )
      )
    }
    "parse round - figure ambigious move" - {
      val input = "1. Rac1 e6"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            1,
            Move.FigureMove(
              Figure.Rook,
              Position('c', '1'),
              Check.NoCheck,
              None,
              Some('a')
            ),
            Some(Move.PawnMove(Position('e', '6'), Check.NoCheck, None))
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
            Move.FigureMove(
              Figure.Rook,
              Position('a', '6'),
              Check.SimpleCheck,
              None,
              None
            ),
            Some(
              Move.FigureMove(
                Figure.King,
                Position('c', '5'),
                Check.NoCheck,
                None,
                None
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
            Move.FigureMove(
              Figure.Rook,
              Position('a', '6'),
              Check.NoCheck,
              None,
              None
            ),
            Some(
              Move.FigureMove(
                Figure.King,
                Position('c', '5'),
                Check.Checkmate,
                None,
                None
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
            Move.FigureMove(
              Figure.Rook,
              Position('a', '6'),
              Check.NoCheck,
              None,
              None
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
            Move.FigureCapture(
              Position('f', '3'),
              Figure.Bishop,
              Check.NoCheck,
              None,
              None
            ),
            None
          )
        )
      )
    }
    "parse round - move with capture by pawn" - {
      val input = "19. cxf3"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.PawnCapture(
              Position('f', '3'),
              'c',
              Check.NoCheck,
              None
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
            Move.PawnMove(
              Position('a', '1'),
              Check.NoCheck,
              Some(Figure.Queen)
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
            Move.FigureCapture(
              Position('c', '6'),
              Figure.Bishop,
              Check.SimpleCheck,
              None,
              None
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
            Move.PawnMove(
              Position('a', '1'),
              Check.SimpleCheck,
              Some(Figure.Queen)
            ),
            None
          )
        )
      )
    }
    "parse round - king side castling" - {
      val input = "19. O-O"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.KingSideCastle(
              Check.NoCheck
            ),
            None
          )
        )
      )
    }
    "parse round - king side castling with check" - {
      val input = "19. O-O+"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.KingSideCastle(
              Check.SimpleCheck
            ),
            None
          )
        )
      )
    }
    "parse round - queen side castling" - {
      val input = "19. O-O-O"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.QueenSideCastle(
              Check.NoCheck
            ),
            None
          )
        )
      )
    }
    "parse round - queen side castling with checkmate" - {
      val input = "19. O-O-O#"
      val output = round.parse(input)
      assertEqual(
        output,
        Right(
          "" -> Round(
            19,
            Move.QueenSideCastle(
              Check.Checkmate
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
            Move.PawnCapture(
              Position('b', '8'),
              'a',
              Check.NoCheck,
              Some(Figure.Queen)
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
            Move.PawnCapture(
              Position('b', '8'),
              'a',
              Check.Checkmate,
              Some(Figure.Queen)
            ),
            None
          )
        )
      )
    }
    "parse score" - {
      assertEqual(score.parse("1-0"), Right("" -> GameResult.WhiteWins))
      assertEqual(score.parse("0-1"), Right("" -> GameResult.BlackWins))
      assertEqual(score.parse("1/2-1/2"), Right("" -> GameResult.Draw))
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
              Move.PawnMove(
                Position('d', '4'),
                Check.NoCheck,
                None
              ),
              Some(
                Move.PawnMove(
                  Position('d', '5'),
                  Check.NoCheck,
                  None
                )
              )
            ),
            Round(
              2,
              Move.PawnMove(
                Position('f', '4'),
                Check.NoCheck,
                None
              ),
              Some(
                Move.PawnMove(
                  Position('e', '6'),
                  Check.NoCheck,
                  None
                )
              )
            ),
            Round(
              3,
              Move.FigureMove(
                Figure.Knight,
                Position('f', '4'),
                Check.NoCheck,
                None,
                None
              ),
              Some(
                Move.PawnMove(
                  Position('g', '6'),
                  Check.NoCheck,
                  None
                )
              )
            )
          )
        )
      )
    }
    "parse rounds ended with score" - {
      val input = "1. d4 d5 1/2-1/2"
      val output = (rounds ~ score).parse(input)
      assertEqual(
        output,
        Right(
          "" -> (List(
            Round(
              1,
              Move.PawnMove(
                Position('d', '4'),
                Check.NoCheck,
                None
              ),
              Some(
                Move.PawnMove(
                  Position('d', '5'),
                  Check.NoCheck,
                  None
                )
              )
            )
          ) -> (GameResult.Draw: GameResult))
        )
      )
    }
    "parse round with single move followed by score" - {
      val input = """12. d4 0-1"""
      val output = (rounds ~ score).parse(input)
      assertEqual(
        output,
        Right(
          "" -> (List(
            Round(
              12,
              Move.PawnMove(Position('d', '4'), Check.NoCheck, None),
              None
            )
          ) -> (GameResult.BlackWins: GameResult))
        )
      )
    }
  }
}
