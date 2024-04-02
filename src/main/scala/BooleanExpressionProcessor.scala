object BooleanExpressionProcessor {
  def extractVariables(expr: BooleanExpression): Set[String] = expr match {
    case Variable(name) => Set(name)

    case Not(e) => extractVariables(e)

    case And(left, right) => extractVariables(left) ++ extractVariables(right)

    case Or(left, right) => extractVariables(left) ++ extractVariables(right)

    case Implication(left, right) => extractVariables(left) ++ extractVariables(right)

    case Equality(left, right) => extractVariables(left) ++ extractVariables(right)

    case _ => Set.empty
  }
}
