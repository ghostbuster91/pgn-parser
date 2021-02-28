package pgnparser
import cats.parse.{Parser => P, _}
import cats.syntax.all._
import utest._

object PgnParserTest extends TestSuite {
  val whitespace: P[Unit] = P.charIn(" \t\r\n").void
  val spaceChar: P[Unit] = P.char(' ')
  def parened[T](p: P[T]) = p.between(P.char('['), P.char(']'))
  def quotes[T](p: P[T]) = p.between(P.char('"'), P.char('"'))
  val string = P.charsWhile(c => c >= ' ' && c != '"' && c != '\\')
  val event = parened(
    (P.string("Event"), spaceChar, quotes(string)).tupled.map(_._3)
  )

  val tests = Tests {
    "should parse event property" - {
      val input = """[Event "Rated Blitz game"]"""

      assertMatch(event.parse(input)) { case Right(v) =>
        assert(v == "Rated Blitz game")
      }
    }
  }
}
