import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BooleanExpressionParserTest extends AnyFlatSpec {
  "parseBooleanExpression" should "parse a variable expression" in {
    val input = "x"
    val expectedResult = Right(Variable("x"))
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result shouldEqual expectedResult
  }

  it should "parse a true literal expression" in {
    val input = "true"
    val expectedResult = Right(True)
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result shouldEqual expectedResult
  }

  it should "parse a false literal expression" in {
    val input = "false"
    val expectedResult = Right(False)
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result shouldEqual expectedResult
  }

  it should "parse a negation expression" in {
    val input = "!x"
    val expectedResult = Right(Not(Variable("x")))
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result shouldEqual expectedResult
  }

  it should "parse an AND expression" in {
    val input = "x && y"
    val expectedResult = Right(And(Variable("x"), Variable("y")))
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result shouldEqual expectedResult
  }

  it should "parse an OR expression" in {
    val input = "x || y"
    val expectedResult = Right(Or(Variable("x"), Variable("y")))
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result shouldEqual expectedResult
  }

  it should "parse a complex expression with parentheses" in {
    val input = "(!x || y) && (x && false)"
    val expectedResult = Right(And(Or(Not(Variable("x")), Variable("y")), And(Variable("x"), False)))
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result shouldEqual expectedResult
  }

  it should "return an error for invalid expressions" in {
    val input = "x || || y"
    val expectedResult = Left("|| expected but || found")
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result === expectedResult
  }

  it should "return an error for invalid boolean expressions" in {
    val input = "!(x && y)) || (x || !y)"
    val expectedResult = Left("string matching regex `\\z' expected but `)' found")
    val result = BooleanExpressionParser.parseBooleanExpression(input)
    result.left.map(_.trim) === expectedResult.left.map(_.trim)
  }
}
