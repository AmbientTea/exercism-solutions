import ForthError.{ForthError, ForthResult}
import cats.implicits._

sealed trait ForthToken
case class ForthId(value: String) extends ForthToken
case class ForthNumber(value: Int) extends ForthToken

sealed class UnaryOp(op: Int => Either[ForthError, List[Int]])
    extends ForthToken {
  def apply(arg: Int): Either[ForthError, List[Int]] = op(arg)
}
case object Dup extends UnaryOp(arg => Right(List(arg, arg)))
case object Drop extends UnaryOp(arg => Right(Nil))

sealed class BinaryOp(val op: (Int, Int) => Either[ForthError, List[Int]])
    extends ForthToken {
  def apply(arg1: Int, arg2: Int): Either[ForthError, List[Int]] =
    op(arg1, arg2)
}
case object Plus extends BinaryOp((arg1, arg2) => Right(List(arg1 + arg2)))
case object Minus extends BinaryOp((arg1, arg2) => Right(List(arg1 - arg2)))
case object Times extends BinaryOp((arg1, arg2) => Right(List(arg1 * arg2)))
case object Div
    extends BinaryOp({
      case (_, 0)       => Left(ForthError.DivisionByZero)
      case (arg1, arg2) => Right(List(arg1 / arg2))
    })
case object Swap extends BinaryOp((arg1, arg2) => Right(List(arg1, arg2)))
case object Over extends BinaryOp((arg1, arg2) => Right(List(arg1, arg2, arg1)))

case object DefStart extends ForthToken
case object DefEnd extends ForthToken

object ForthToken {
  def fromString(str: String): Either[ForthError, ForthToken] = {
    str.toLowerCase match {
      case ":" => Right(DefStart)
      case ";" => Right(DefEnd)
      case str =>
        str.toIntOption
          .map(ForthNumber)
          .orElse(Some(ForthId(str)))
          .toRight(ForthError.UnknownWord)
    }
  }

  def tokenize(text: String): ForthResult[List[ForthToken]] = {
    text.toLowerCase
      .split("\\s+")
      .toList
      .traverse(ForthToken.fromString)
  }
}
