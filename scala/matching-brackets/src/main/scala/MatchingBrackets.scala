sealed class BracketType(val opening: Char, val closing: Char)
case object RoundBracket extends BracketType('(', ')')
case object SquareBracket extends BracketType('[', ']')
case object CurlyBracket extends BracketType('{', '}')
object BracketType {
  final val allBracketTypes: List[BracketType] =
    List(RoundBracket, SquareBracket, CurlyBracket)
}
sealed trait Bracket
case class Opening(bracketType: BracketType) extends Bracket
case class Closing(bracketType: BracketType) extends Bracket
object Bracket {
  def fromChar(char: Char): Option[Bracket] =
    BracketType.allBracketTypes.collectFirst {
      case bracketType if bracketType.opening == char => Opening(bracketType)
      case bracketType if bracketType.closing == char => Closing(bracketType)
    }
}
object MatchingBrackets {
  def isPaired(brackets: String): Boolean = {
    var stack = List.empty[BracketType]
    for (bracket <- brackets.iterator.flatMap(Bracket.fromChar)) {
      (stack, bracket) match {
        case (_, Opening(bracketType)) =>
          stack = bracketType :: stack
        case (pending :: stackTail, Closing(bracketType))
            if pending == bracketType =>
          stack = stackTail
        case _ => return false
      }
    }
    stack.isEmpty
  }
}
