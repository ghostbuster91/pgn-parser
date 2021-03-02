package pgnparser
import cats.parse.{Parser => P, _}
import cats.syntax.all._
import utest._
import com.softwaremill.diffx.generic.auto._
import com.softwaremill.diffx.ConsoleColorConfig
import com.softwaremill.diffx.utest.DiffxAssertions._
import Move.Capture
import cats.data.Op
import PgnParser._

object PgnParserTest extends TestSuite {
  implicit val c: ConsoleColorConfig =
    ConsoleColorConfig(x => s"-$x", x => s"+$x", identity, identity)

  val tests = Tests {
    "parse event property" - {
      val input = """[Event "Rated Blitz game"]"""
      val output = property.parse(input)
      println(output)
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
            Move.PawnMove(Position('e', '4'), Check.NoCheck),
            Some(
              Move.PawnMove(Position('e', '6'), Check.NoCheck)
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
              Move.PawnMove(Position('e', '6'), Check.NoCheck)
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
            Some(Move.PawnMove(Position('e', '6'), Check.NoCheck))
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
              Some(Capture('a')),
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
              Some(Capture('a')),
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
              Move.PawnMove(
                Position('d', '4'),
                Check.NoCheck
              ),
              Some(
                Move.PawnMove(
                  Position('d', '5'),
                  Check.NoCheck
                )
              )
            ),
            Round(
              2,
              Move.PawnMove(
                Position('f', '4'),
                Check.NoCheck
              ),
              Some(
                Move.PawnMove(
                  Position('e', '6'),
                  Check.NoCheck
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
      val output = (rounds ~ score).parse(input)
      assertEqual(
        output,
        Right(
          "" -> (List(
            Round(
              1,
              Move.PawnMove(
                Position('d', '4'),
                Check.NoCheck
              ),
              Some(
                Move.PawnMove(
                  Position('d', '5'),
                  Check.NoCheck
                )
              )
            )
          ) -> (Score.Draw: Score))
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
            Round(12, Move.PawnMove(Position('d', '4'), Check.NoCheck), None)
          ) -> (Score.BlackWins: Score))
        )
      )
    }

    "parse complete game" - {
      val input =
        """1. e4 c5 2. c3 Nc6 3. Nf3 d6 4. d4 cxd4 5. cxd4 g6 6. Bb5 Nf6 7. d5 Qa5+ 8. Nc3 Nxe4 9. Bxc6+ bxc6 10. Bd2 Nxd2 11. Qxd2 Bg7 12. dxc6 13. Bg4 14. Rfe1 e6 15. a3 Rfd8 16. b4 Qc7 17. Rac1 Qxc6 18. Qd3 Bf5 19. Qe3 Qd7 20. Nd4 Bxd4 21. Qxd4 e5 22. Qh4 Be6 23. Ne4 Kg7 24. Nf6 Qe7 25. Nh5+ Kf8 26. Qxe7+ Kxe7 27. Ng3 f5 28. Rc7+ Kf6 29. Rxh7 d5 30. h3 d4 31. Rc7 d3 32. Rd1 e4 33. Nf1 Rdc8 34. Rdc1 Rxc7 35. Rxc7 Rc8 36. Rxc8 Bxc8 37. Nd2 Be6 38. Kf1 g5 39. Ke1 Ke5 40. Kd1 Kd4 41. Kc1 Kc3 42. b5 Bd7 43. a4 Kb4 44. Nb1 Kxa4 45. Nc3+ Kb4 46. Kd2 Bxb5 47. Nd5+ Kc5 48. Ne3 Bd7 49. Nd1 a5 50. Nc3 Kd4 51. Nb1 a4 52. Na3 f4 53. Nb1 e3+ 54. fxe3+ fxe3+ 55. Ke1 d2+ 56. Nxd2 exd2+ 57. Kxd2 a3 58. Kc2 a2 59. Kb2 Be6 60. g4 Ke4 61. Kc3 Kf4 62. Kd2 Kg3 63. Ke3 a1=Q 0-1"""
      val output = pgnGame.parse(input)
      assert(output.isRight)
    }
  }
}
