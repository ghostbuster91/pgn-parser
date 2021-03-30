package io.github.ghostbuster91.pgnparser.parser

import utest._

object PgnParserIntegrationTest extends TestSuite {
  val tests: Tests = Tests {

    val games: List[String] = loadGames

    "parse successfully 10k games" - {
      games.zipWithIndex.foreach { case (pgnGame, idx) =>
        val res = PgnParser.pgnGame.parse(pgnGame)
        res match {
          case Left(value) =>
            println(s"Game $idx failed to parse with error: $value")
            assert(false)
          case Right(_) => // noop
        }
      }
    }
  }

  private def loadGames = {
    val source =
      scala.io.Source.fromResource("lichess_db_10k_lines_stripped_2020-12.pgn")
    try source.getLines().toList
    finally source.close()
  }
}
