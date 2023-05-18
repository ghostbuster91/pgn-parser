package io.github.ghostbuster91.pgnparser.parser
import utest._
// import com.softwaremill.diffx.ConsoleColorConfig
import com.softwaremill.diffx.utest.DiffxAssertions._
import PgnParser._
import chessmodel._
import chessmodel.position._

object PgnParserTest extends TestSuite with DiffSemiSupport {
  // implicit val c: ConsoleColorConfig =
  //   ConsoleColorConfig(x => s"-$x", x => s"+$x", identity, identity)

  val tests = Tests {
    "parse event property" - {
      val input = """[Event "Rated Blitz game"]"""
      val output = property.parse(input)
      assertEqual(
        output,
        Right("" -> Tag("Event", "Rated Blitz game"))
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
            SanMove
              .PawnMove(Position(File('e'), Rank('4')), Check.NoCheck, None),
            Some(
              SanMove.PawnMove(
                Position(File('e'), Rank('6')),
                Check.NoCheck,
                None
              )
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
            SanMove.FigureMove(
              Figure.Knight,
              Position(File('f'), Rank('3')),
              Check.NoCheck,
              None,
              None,
              isCapture = false
            ),
            Some(
              SanMove.PawnMove(
                Position(File('e'), Rank('6')),
                Check.NoCheck,
                None
              )
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
            SanMove.FigureMove(
              Figure.Rook,
              Position(File('c'), Rank('1')),
              Check.NoCheck,
              None,
              Some(File('a')),
              isCapture = false
            ),
            Some(
              SanMove.PawnMove(
                Position(File('e'), Rank('6')),
                Check.NoCheck,
                None
              )
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
            SanMove.FigureMove(
              Figure.Rook,
              Position(File('a'), Rank('6')),
              Check.SimpleCheck,
              None,
              None,
              isCapture = false
            ),
            Some(
              SanMove.FigureMove(
                Figure.King,
                Position(File('c'), Rank('5')),
                Check.NoCheck,
                None,
                None,
                isCapture = false
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
            SanMove.FigureMove(
              Figure.Rook,
              Position(File('a'), Rank('6')),
              Check.NoCheck,
              None,
              None,
              isCapture = false
            ),
            Some(
              SanMove.FigureMove(
                Figure.King,
                Position(File('c'), Rank('5')),
                Check.Checkmate,
                None,
                None,
                isCapture = false
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
            SanMove.FigureMove(
              Figure.Rook,
              Position(File('a'), Rank('6')),
              Check.NoCheck,
              None,
              None,
              isCapture = false
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
            SanMove.FigureMove(
              Figure.Bishop,
              Position(File('f'), Rank('3')),
              Check.NoCheck,
              None,
              None,
              isCapture = true
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
            SanMove.PawnCapture(
              Position(File('f'), Rank('3')),
              File('c'),
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
            SanMove.PawnMove(
              Position(File('a'), Rank('1')),
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
            SanMove.FigureMove(
              Figure.Bishop,
              Position(File('c'), Rank('6')),
              Check.SimpleCheck,
              None,
              None,
              isCapture = true
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
            SanMove.PawnMove(
              Position(File('a'), Rank('1')),
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
            SanMove.KingSideCastle(
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
            SanMove.KingSideCastle(
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
            SanMove.QueenSideCastle(
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
            SanMove.QueenSideCastle(
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
            SanMove.PawnCapture(
              Position(File('b'), Rank('8')),
              File('a'),
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
            SanMove.PawnCapture(
              Position(File('b'), Rank('8')),
              File('a'),
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
              SanMove.PawnMove(
                Position(File('d'), Rank('4')),
                Check.NoCheck,
                None
              ),
              Some(
                SanMove.PawnMove(
                  Position(File('d'), Rank('5')),
                  Check.NoCheck,
                  None
                )
              )
            ),
            Round(
              2,
              SanMove.PawnMove(
                Position(File('f'), Rank('4')),
                Check.NoCheck,
                None
              ),
              Some(
                SanMove.PawnMove(
                  Position(File('e'), Rank('6')),
                  Check.NoCheck,
                  None
                )
              )
            ),
            Round(
              3,
              SanMove.FigureMove(
                Figure.Knight,
                Position(File('f'), Rank('4')),
                Check.NoCheck,
                None,
                None,
                isCapture = false
              ),
              Some(
                SanMove.PawnMove(
                  Position(File('g'), Rank('6')),
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
              SanMove.PawnMove(
                Position(File('d'), Rank('4')),
                Check.NoCheck,
                None
              ),
              Some(
                SanMove.PawnMove(
                  Position(File('d'), Rank('5')),
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
              SanMove
                .PawnMove(Position(File('d'), Rank('4')), Check.NoCheck, None),
              None
            )
          ) -> (GameResult.BlackWins: GameResult))
        )
      )
    }
  }
}
