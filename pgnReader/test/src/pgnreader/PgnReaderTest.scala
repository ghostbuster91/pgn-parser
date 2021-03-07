package pgnreader

import utest._

object PgnReaderTest extends TestSuite {
  val tests = Tests {
    "should work" - {
      val input =
        """1. d4 d5 2. e4 dxe4 3. f3 exf3 4. Nc3 fxg2 5. Bxg2 Nf6 6. Nf3 e6 7. O-O c6 8. Bg5 Be7 9. Ne5 O-O 10. Qd3 Nbd7 11. Nxd7 Qxd7 12. Bxf6 Bxf6 13. Be4 Qxd4+ 14. Qxd4 Bxd4+ 15. Kh1 Bxc3 16. bxc3 e5 17. Rf3 f5 18. Raf1 Be6 19. Bxf5 Bxf5 20. Rxf5 Rxf5 21. Rxf5 Re8 22. Kg2 g6 23. Rf3 Kg7 24. Kg3 e4 25. Re3 Kf6 26. c4 Ke5 27. c3 Kf5 28. a4 b6 29. h3 Rd8 30. Kf2 Rd3 31. Rxd3 exd3 32. Ke3 Ke5 33. Kxd3 c5 34. Ke3 g5 35. Kf3 h5 36. Ke3 a6 37. Kf3 Kf5 38. Ke3 g4 39. h4 g3 40. Kf3 g2 41. Kxg2 Kg4 42. Kh2 Kxh4 43. Kg2 Kg4 44. Kf2 h4 45. Kg2 h3+ 46. Kh2 Kf3 47. Kxh3 Ke3 48. Kg4 Kd3 49. Kf5 Kxc3 50. Ke6 Kxc4 51. Kd6 Kb4 52. Kc6 Kxa4 53. Kxb6 c4 54. Kc5 c3 55. Kc4 c2 56. Kc3 c1=Q+ 0-1"""

      println(new PgnReader().read(input))
    }
    val games: List[String] = loadGames

    "parse successfully 10k games" - {
      games.zipWithIndex.foreach { case (pgnGame, idx) =>
        val res = new PgnReader().read(pgnGame)
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
