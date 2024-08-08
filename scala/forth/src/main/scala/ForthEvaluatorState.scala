import ForthError.ForthResult

case class ForthEvaluatorState(
    stack: List[Int],
    defs: Map[String, List[ForthToken]]
) {
  override def toString: String = stack.reverse.mkString(" ")

  def define(id: String, body: List[ForthToken]): ForthEvaluatorState =
    this.copy(defs = this.defs + (id -> body))

  def getDef(id: String): ForthResult[List[ForthToken]] =
    defs.get(id).toRight(ForthError.UnknownWord)
}

object ForthEvaluatorState {
  final val stdLib: Map[String, List[ForthToken]] = Map(
    "+" -> List(Plus),
    "-" -> List(Minus),
    "*" -> List(Times),
    "/" -> List(Div),
    "dup" -> List(Dup),
    "drop" -> List(Drop),
    "swap" -> List(Swap),
    "over" -> List(Over)
  )
  def empty: ForthEvaluatorState = ForthEvaluatorState(Nil, stdLib)
}
