import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class BooleanExpressionProcessorTest extends AnyFlatSpec {
  "extractVariables" should "return a set containing the variable name for Variable case" in {
    val expression = Variable("x")
    val result = BooleanExpressionProcessor.extractVariables(expression)
    result shouldEqual Set("x")
  }

  it should "return a set of variables for Not case" in {
    val expression = Not(Variable("x"))
    val result = BooleanExpressionProcessor.extractVariables(expression)
    result shouldEqual Set("x")
  }

  it should "return a set of variables for And case" in {
    val expression = And(Variable("x"), Variable("y"))
    val result = BooleanExpressionProcessor.extractVariables(expression)
    result shouldEqual Set("x", "y")
  }

  it should "return a set of variables for Or case" in {
    val expression = Or(Variable("x"), Variable("y"))
    val result = BooleanExpressionProcessor.extractVariables(expression)
    result shouldEqual Set("x", "y")
  }

  it should "return a set of variables for Implication case" in {
    val expression = Implication(Variable("x"), Variable("y"))
    val result = BooleanExpressionProcessor.extractVariables(expression)
    result shouldEqual Set("x", "y")
  }

  it should "return a set of variables for Equality case" in {
    val expression = Equality(Variable("x"), Variable("y"))
    val result = BooleanExpressionProcessor.extractVariables(expression)
    result shouldEqual Set("x", "y")
  }

  it should "return an empty set for any other case" in {
    val expression = True
    val result = BooleanExpressionProcessor.extractVariables(expression)
    result shouldEqual Set.empty
  }

  it should "extract variables from boolean expressions correctly" in {
    val expression = Implication(And(Variable("x"), Variable("y")), Or(Variable("x"), Variable("z")))
    val expectedResult = Set("x", "y", "z")
    val result = BooleanExpressionProcessor.extractVariables(expression)
    result shouldEqual expectedResult
  }
}
