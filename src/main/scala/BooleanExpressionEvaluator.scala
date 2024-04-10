object BooleanExpressionEvaluator {
  def evaluateExpression(expression: BooleanExpression, data: Map[String, Boolean] = Map.empty): Either[String, Boolean] = {
    val simplifiedExpression = simplifyExpression(expression)
    evaluateSimplifiedExpression(simplifiedExpression, data)
  }

  private def evaluateSimplifiedExpression(expression: BooleanExpression, data: Map[String, Boolean]): Either[String, Boolean] = expression match {
    case True => Right(true)
    case False => Right(false)
    case Variable(name) => data.get(name) match {
      case Some(value) => Right(value)
      case None => Left(s"Variable '$name' is not defined in the data")
    }
    case Not(subExpr) => evaluateSimplifiedExpression(subExpr, data).map(!_)

    case And(left, right) =>
      for {
        leftResult <- evaluateSimplifiedExpression(left, data)
        rightResult <- evaluateSimplifiedExpression(right, data)
      } yield leftResult && rightResult

    case Or(left, right) =>
      for {
        leftResult <- evaluateSimplifiedExpression(left, data)
        rightResult <- evaluateSimplifiedExpression(right, data)
      } yield leftResult || rightResult

    case Implication(left, right) =>
      for {
        leftResult <- evaluateSimplifiedExpression(left, data)
        rightResult <- evaluateSimplifiedExpression(right, data)
      } yield !leftResult || rightResult

    case Equality(left, right) =>
      for {
        leftResult <- evaluateSimplifiedExpression(left, data)
        rightResult <- evaluateSimplifiedExpression(right, data)
      } yield leftResult == rightResult
  }

  def simplifyExpression (expression: BooleanExpression): BooleanExpression = expression match {
    case And(left, True) => simplifyExpression (left)

    case And(True, right) => simplifyExpression (right)

    case Or(left, False) => simplifyExpression (left)

    case Or(False, right) => simplifyExpression (right)

    case Not(Not(e)) => simplifyExpression (e)

    case And(left, right) =>
      val simplifiedLeft = simplifyExpression (left)
      val simplifiedRight = simplifyExpression (right)
      if (simplifiedLeft == False || simplifiedRight == False) False
      else if (simplifiedLeft == True) simplifiedRight
      else if (simplifiedRight == True) simplifiedLeft
      else And(simplifiedLeft, simplifiedRight)

    case Or(left, right) =>
      val simplifiedLeft = simplifyExpression (left)
      val simplifiedRight = simplifyExpression (right)
      if (simplifiedLeft == True || simplifiedRight == True) True
      else if (simplifiedLeft == False) simplifiedRight
      else if (simplifiedRight == False) simplifiedLeft
      else Or(simplifiedLeft, simplifiedRight)

    case Not(e) =>
      val simplifiedExpr = simplifyExpression (e)
      if (simplifiedExpr == True) False
      else if (simplifiedExpr == False) True
      else Not(simplifiedExpr)

    case other => other
  }

  def visualizeExpression (expression: BooleanExpression): String = expression match {
    case True => "true"

    case False => "false"

    case Variable(name) => name

    case Not(e) => s"!(${visualizeExpression (e)})"

    case And(left, right) => s"(${visualizeExpression (left)} & ${visualizeExpression (right)})"

    case Or(left, right) => s"(${visualizeExpression (left)} | ${visualizeExpression (right)})"

    case Implication(left, right) => s"(${visualizeExpression (left)} => ${visualizeExpression (right)})"

    case Equality(left, right) => s"(${visualizeExpression (left)} <=> ${visualizeExpression (right)})"
  }
}
