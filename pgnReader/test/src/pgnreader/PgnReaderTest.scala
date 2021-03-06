package pgnreader

import utest._

object PgnReaderTest extends TestSuite {
  val tests = Tests {
    "should work" - {
      val input =
        """1. e4 c5 2. c3 Nc6 3. Nf3 d6 4. d4 cxd4 5. cxd4 g6 6. Bb5 Nf6 7. d5 Qa5+ 8. Nc3 Nxe4 9. Bxc6+ bxc6 10. Bd2 Nxd2 11. Qxd2 Bg7 12. dxc6 13. Bg4 14. Rfe1 e6 15. a3 Rfd8 16. b4 Qc7 17. Rac1 Qxc6 18. Qd3 Bf5 19. Qe3 Qd7 20. Nd4 Bxd4 21. Qxd4 e5 22. Qh4 Be6 23. Ne4 Kg7 24. Nf6 Qe7 25. Nh5+ Kf8 26. Qxe7+ Kxe7 27. Ng3 f5 28. Rc7+ Kf6 29. Rxh7 d5 30. h3 d4 31. Rc7 d3 32. Rd1 e4 33. Nf1 Rdc8 34. Rdc1 Rxc7 35. Rxc7 Rc8 36. Rxc8 Bxc8 37. Nd2 Be6 38. Kf1 g5 39. Ke1 Ke5 40. Kd1 Kd4 41. Kc1 Kc3 42. b5 Bd7 43. a4 Kb4 44. Nb1 Kxa4 45. Nc3+ Kb4 46. Kd2 Bxb5 47. Nd5+ Kc5 48. Ne3 Bd7 49. Nd1 a5 50. Nc3 Kd4 51. Nb1 a4 52. Na3 f4 53. Nb1 e3+ 54. fxe3+ fxe3+ 55. Ke1 d2+ 56. Nxd2 exd2+ 57. Kxd2 a3 58. Kc2 a2 59. Kb2 Be6 60. g4 Ke4 61. Kc3 Kf4 62. Kd2 Kg3 63. Ke3 a1=Q 0-1"""

      println(new PgnReader().read(input))
    }
  }
}
