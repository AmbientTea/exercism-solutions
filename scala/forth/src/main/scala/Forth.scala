import ForthError.ForthResult

class Forth {
  def eval(text: String): ForthResult[ForthEvaluatorState] =
    ForthToken.tokenize(text).flatMap(ForthEvaluator.evalAll(_))
}
