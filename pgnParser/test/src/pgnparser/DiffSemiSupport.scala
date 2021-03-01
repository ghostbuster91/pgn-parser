package pgnparser

import cats.parse.{Parser => P, _}
import com.softwaremill.diffx._
import Move.PromotionCapture
import com.softwaremill.diffx.cats._

trait DiffSemiSupport {
  implicit val dFigure: Diff[Figure] = new Diff[Figure] {
    def apply(
        left: Figure,
        right: Figure,
        toIgnore: List[FieldPath]
    ): DiffResult = {
      if (left == right) Identical(left)
      else DiffResultValue(left, right)
    }
  }

  implicit val dChar = new Diff[Char] {
    def apply(
        left: Char,
        right: Char,
        toIgnore: List[FieldPath]
    ): DiffResult = {
      if (left == right) Identical(left)
      else DiffResultValue(left, right)
    }
  }
  implicit val dPosition = Diff.derived[Position]
  implicit val dPromoCapture = Diff.derived[PromotionCapture]
  implicit val dCheck: Diff[Check] = new Diff[Check] {
    def apply(
        left: Check,
        right: Check,
        toIgnore: List[FieldPath]
    ): DiffResult = {
      if (left == right) Identical(left)
      else DiffResultValue(left, right)
    }
  }
  implicit val dBool = new Diff[Boolean] {
    def apply(
        left: Boolean,
        right: Boolean,
        toIgnore: List[FieldPath]
    ): DiffResult = {
      if (left == right) Identical(left)
      else DiffResultValue(left, right)
    }
  }
  implicit val dScore: Diff[Score] = new Diff[Score] {
    def apply(
        left: Score,
        right: Score,
        toIgnore: List[FieldPath]
    ): DiffResult = {
      if (left == right) Identical(left)
      else DiffResultValue(left, right)
    }
  }

  implicit val dSimpleMove: Diff[Move.SimpleMove] =
    Diff.derived[Move.SimpleMove]
  implicit val dPromoMove: Diff[Move.Promotion] = Diff.derived[Move.Promotion]
  implicit val dMove: Diff[Move] = Diff.derived[Move]
  implicit val dRound: Diff[Round] = Diff.derived[Round]
  implicit def dRight[L: Diff, R: Diff] = Diff.derived[Right[L, R]]
  implicit def dLeft[L: Diff, R: Diff] = Diff.derived[Left[L, R]]
  implicit def dEither[L: Diff, R: Diff] = Diff.derived[Either[L, R]]
  implicit val dExpect = Diff.derived[P.Expectation]
  implicit val dError = Diff.derived[P.Error]
  implicit def dTuple2[T1: Diff, T2: Diff] = Diff.derived[Tuple2[T1, T2]]

}
