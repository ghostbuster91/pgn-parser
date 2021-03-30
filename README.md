# pgn-parser

[<img alt="GitHub Workflow" src="https://img.shields.io/github/workflow/status/ghostbuster91/pgn-parser/CI/master?style=for-the-badge" height="24">](https://github.com/ghostbuster91/pgn-parser/actions)
[<img alt="Maven Central" src="https://img.shields.io/maven-central/v/io.github.ghostbuster91.pgnparser/core_2.13?style=for-the-badge" height="24">](https://search.maven.org/artifact/io.github.ghostbuster91.pgnparser/core_2.13)

### parser

Parser can simply parse chess games stored in [PGN](https://en.wikipedia.org/wiki/Portable_Game_Notation) format.

```scala
val inputGame = "1. e4 e5 2. Bc4 Bc5 3. Qh5 Nf6 4. Qxf7# 1-0"
PgnParser.pgnGame.parseAll(inputGame)
res1: Either[cats.parse.Parser.Error, io.github.ghostbuster91.pgnparser.parser.PgnGame] =
Right(
  PgnGame(
    List(),
    List(
      Round(
        1,
        PawnMove(Position('e', '4'), NoCheck, None),
        Some(PawnMove(Position('e', '5'), NoCheck, None))
      ),
      Round(
        2,
        FigureMove(Bishop, Position('c', '4'), NoCheck, None, None, false),
        Some(FigureMove(Bishop, Position('c', '5'), NoCheck, None, None, false))
      ),
      Round(
        3,
        FigureMove(Queen, Position('h', '5'), NoCheck, None, None, false),
        Some(FigureMove(Knight, Position('f', '6'), NoCheck, None, None, false))
      ),
      Round(4, FigureMove(Queen, Position('f', '7'), Checkmate, None, None, true), None)
    ),
    WhiteWins
  )
)
```

In case of malformed input an error will be returned:

```scala
val malformedGame = "1. e4 e5 2. Bc4 Bc5 3. Qh5 Nf6asdasd"
PgnParser.pgnGame.parseAll(malformedGame)
res6: Either[cats.parse.Parser.Error, io.github.ghostbuster91.pgnparser.parser.PgnGame] = Left(
  Error(
    20,
    NonEmptyList(Str(20, "0-1"), List(Str(20, "1-0"), Str(20, "1/2-1/2"), InRange(20, '*', '*')))
  )
)
```

### reader

Reader is an abstraction build on top of parser. Apart from parsing the game, reader also converts it to unambiogious [LAN](<https://en.wikipedia.org/wiki/Algebraic_notation_(chess)#Long_algebraic_notation>) format. It does that by partially implementing chess rules needed to filter out incorrect movements.

```scala
val inputGame = "1. e4 e5 2. Bc4 Bc5 3. Qh5 Nf6 4. Qxf7# 1-0"
new PgnReader().read(inputGame)
res2: Either[io.github.ghostbuster91.pgnparser.core.ParsingException, io.github.ghostbuster91.pgnparser.core.ChessGame] = Right(
  ChessGame(
    Board(
      HashMap(
        Coordinate(1, 0) -> PlayerPiece(Knight, White),
        Coordinate(4, 3) -> PlayerPiece(Pawn, White),
        Coordinate(4, 7) -> PlayerPiece(King, Black),
        Coordinate(3, 7) -> PlayerPiece(Queen, Black),
        Coordinate(7, 1) -> PlayerPiece(Pawn, White),
        Coordinate(4, 0) -> PlayerPiece(King, White),
        Coordinate(1, 1) -> PlayerPiece(Pawn, White),
        Coordinate(0, 1) -> PlayerPiece(Pawn, White),
        Coordinate(0, 0) -> PlayerPiece(Rook, White),
...
```

The whole output is actually to big to be included in the readme as reader outputs the game as a list of state transistions of a chess board. The structer of the result looks as follows:

```scala
case class ChessGame(
    board: Board,
    currentPlayer: Player,
    previousState: Option[(ChessGame, LanMove)]
)
```

Reader does not verify game's correctness. The game is assumed to be valid.
