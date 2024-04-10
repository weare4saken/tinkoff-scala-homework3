import scala.util.parsing.combinator.JavaTokenParsers

object BooleanExpressionParser extends JavaTokenParsers {
  def booleanExpression: Parser[BooleanExpression] = chainl1(expressionTerm, "||" ^^^ Or | "|" ^^^ Or)

  def expressionTerm: Parser[BooleanExpression] = chainl1(expressionFactor, "&&" ^^^ And | "&" ^^^ And)

  def expressionFactor: Parser[BooleanExpression] = notExpressionFactor | "(" ~> booleanExpression <~ ")" | literal | variable

  def notExpressionFactor: Parser[BooleanExpression] = "!" ~> expressionFactor ^^ Not

  def variable: Parser[BooleanExpression] = """[a-zA-Z]\w*""".r ^^ Variable

  def literal: Parser[BooleanExpression] = ("true" ^^^ True) | ("false" ^^^ False)

  def parseBooleanExpression(input: String): Either[String, BooleanExpression] = parseAll(phrase(booleanExpression), input) match {
    case Success(result, _) => Right(result)
    case failure: NoSuccess => Left(failure.msg)
  }
}
