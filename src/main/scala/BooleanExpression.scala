sealed trait BooleanExpression

case object True extends BooleanExpression

case object False extends BooleanExpression

case class Variable(name: String) extends BooleanExpression

case class Not(expression: BooleanExpression) extends BooleanExpression

case class And(expressionL: BooleanExpression, expressionR: BooleanExpression) extends BooleanExpression

case class Or(expressionL: BooleanExpression, expressionR: BooleanExpression) extends BooleanExpression

case class Implication(expressionL: BooleanExpression, expressionR: BooleanExpression) extends BooleanExpression

case class Equality(expressionL: BooleanExpression, expressionR: BooleanExpression) extends BooleanExpression
