import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BooleanExpressionEvaluatorTest extends AnyFlatSpec {
  "evaluateExpression" should "evaluate a True expression" in {
    val expression = True
    val result = BooleanExpressionEvaluator.evaluateExpression(expression)
    result shouldEqual Right(true)
  }

  it should "evaluate a False expression" in {
    val expression = False
    val result = BooleanExpressionEvaluator.evaluateExpression(expression)
    result shouldEqual Right(false)
  }

  it should "evaluate a Variable expression with defined data" in {
    val expression = Variable("x")
    val data = Map("x" -> true)
    val result = BooleanExpressionEvaluator.evaluateExpression(expression, data)
    result shouldEqual Right(true)
  }

  it should "return an error for a Variable expression with undefined data" in {
    val expression = Variable("x")
    val result = BooleanExpressionEvaluator.evaluateExpression(expression)
    result shouldEqual Left("Variable 'x' is not defined in the data")
  }

  it should "evaluate a Negation expression" in {
    val expression = Not(False)
    val result = BooleanExpressionEvaluator.evaluateExpression(expression)
    result shouldEqual Right(true)
  }

  it should "evaluate an AND expression" in {
    val expression = And(True, False)
    val result = BooleanExpressionEvaluator.evaluateExpression(expression)
    result shouldEqual Right(false)
  }

  it should "evaluate an OR expression" in {
    val expression = Or(True, False)
    val result = BooleanExpressionEvaluator.evaluateExpression(expression)
    result shouldEqual Right(true)
  }

  it should "evaluate an Implication expression" in {
    val expression = Implication(True, False)
    val result = BooleanExpressionEvaluator.evaluateExpression(expression)
    result shouldEqual Right(false)
  }

  it should "evaluate an Equality expression" in {
    val expression = Equality(True, False)
    val result = BooleanExpressionEvaluator.evaluateExpression(expression)
    result shouldEqual Right(false)
  }

  "simplifyExpression" should "simplify a nested NOT expression" in {
    val expression = Not(Not(True))
    val result = BooleanExpressionEvaluator.simplifyExpression(expression)
    result shouldEqual True
  }

  it should "simplify an AND expression with True" in {
    val expression = And(True, Variable("x"))
    val result = BooleanExpressionEvaluator.simplifyExpression(expression)
    result shouldEqual Variable("x")
  }

  it should "simplify an AND expression with False" in {
    val expression = And(Variable("x"), False)
    val result = BooleanExpressionEvaluator.simplifyExpression(expression)
    result shouldEqual False
  }

  "visualizeExpression" should "visualize a True expression" in {
    val expression = True
    val result = BooleanExpressionEvaluator.visualizeExpression(expression)
    result shouldEqual "true"
  }

  it should "visualize a Variable expression" in {
    val expression = Variable("x")
    val result = BooleanExpressionEvaluator.visualizeExpression(expression)
    result shouldEqual "x"
  }

  it should "visualize a Negation expression" in {
    val expression = Not(Variable("x"))
    val result = BooleanExpressionEvaluator.visualizeExpression(expression)
    result === "(!x)"
  }
}
