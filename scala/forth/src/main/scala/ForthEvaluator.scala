import ForthError.ForthResult
import cats.implicits._

object ForthEvaluator {
  def evalAll(
      tokens: List[ForthToken],
      state: ForthEvaluatorState = ForthEvaluatorState.empty
  ): ForthResult[ForthEvaluatorState] = {
    (tokens, state.stack) match {
      case (Nil, _) => Right(state)

      case (ForthNumber(value) :: tokens, stack) =>
        evalAll(tokens, state.copy(stack = value :: stack))

      case ((op: UnaryOp) :: tokens, arg :: stack) =>
        op(arg).flatMap(newStack =>
          evalAll(tokens, state.copy(stack = newStack ++ stack))
        )
      case ((_: UnaryOp) :: _, _) => Left(ForthError.StackUnderflow)

      case ((op: BinaryOp) :: tokens, arg1 :: arg2 :: stack) =>
        op(arg2, arg1).flatMap(newStack =>
          evalAll(tokens, state.copy(stack = newStack ++ stack))
        )
      case ((_: BinaryOp) :: _, _) => Left(ForthError.StackUnderflow)

      case (DefEnd :: _, _) => Left(ForthError.InvalidWord)
      case (DefStart :: ForthId(id) :: tokens, _) =>
        getDefBody(tokens).flatMap { case (body, rest) =>
          body
            .flatTraverse(expand(_, state))
            .map(state.define(id, _))
            .flatMap(evalAll(rest, _))
        }

      case (ForthId(id) :: tokens, _) =>
        state.getDef(id).flatMap(body => evalAll(body ++ tokens, state))

      case _ => Left(ForthError.InvalidWord)
    }
  }

  private def getDefBody(
      tokens: List[ForthToken]
  ): ForthResult[(List[ForthToken], List[ForthToken])] = {
    val body = tokens.takeWhile(_ != DefEnd)
    val rest = tokens.dropWhile(_ != DefEnd)
    rest match {
      case DefEnd :: rest => Right((body, rest))
      case _              => Left(ForthError.InvalidWord)
    }

  }

  private def expand(
      token: ForthToken,
      state: ForthEvaluatorState
  ): ForthResult[List[ForthToken]] =
    token match {
      case ForthId(id) => state.getDef(id)
      case other       => Right(List(other))
    }

}
