object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord = Value

  type ForthResult[T] = Either[ForthError.ForthError, T]
}
