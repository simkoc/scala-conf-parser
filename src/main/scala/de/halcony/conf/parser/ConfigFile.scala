package de.halcony.conf.parser

sealed trait ConfigFileElement {
  val index: Int
  val lineNumber: Int
}

sealed case class ConfigFile(
    expressions: List[Expression],
    override val index: Int,
    override val lineNumber: Int
) extends ConfigFileElement

sealed trait Expression extends ConfigFileElement

sealed case class NameExpr(value: String, override val index: Int, override val lineNumber: Int)
    extends Expression

sealed case class ListExpr(
    values: List[Expression],
    override val index: Int,
    override val lineNumber: Int
) extends Expression

sealed case class CommentExpr(value: String, override val index: Int, override val lineNumber: Int)
    extends Expression

sealed trait VariableLikeExpression extends Expression {
  def getVariableStringRepresentation: String
}

sealed case class VariableExpr(name: String, override val index: Int, override val lineNumber: Int)
    extends VariableLikeExpression {
  override def getVariableStringRepresentation: String = s"$$$name"
}

sealed case class InlineExpr(value: String, override val index: Int, override val lineNumber: Int)
    extends VariableLikeExpression {
  override def getVariableStringRepresentation: String = value
}

sealed trait StringExpression extends Expression {
  def getString: String
}

sealed case class ScalarExpr(
    value: String,
    override val index: Int,
    override val lineNumber: Int,
    sType: Option[String] = None
) extends StringExpression {
  override def getString: String = value
}

sealed case class MixedString(
    parts: List[Expression],
    override val index: Int,
    override val lineNumber: Int
) extends StringExpression {
  override def getString: String = parts.map {
    case VariableExpr(value, _, _)  => s"$$$value"
    case ScalarExpr(value, _, _, _) => value
    case x                          => s"[[UNKNOWN STRING OF ${x.getClass}"
  }.mkString
}

sealed case class CondCtrlStructure(
    ctype: String,
    condExpr: Expression,
    block: List[Expression],
    elseBlock: List[Expression],
    override val index: Int,
    override val lineNumber: Int
) extends Expression

sealed case class ForeignBlobExpr(
    name: String,
    content: String,
    override val index: Int,
    override val lineNumber: Int
) extends Expression

sealed case class CallExpr(
    name: Expression,
    args: List[Expression],
    override val index: Int,
    override val lineNumber: Int
) extends Expression

sealed case class BlockExpr(
    name: Option[Expression],
    arguments: List[Expression],
    expressions: List[Expression],
    override val index: Int,
    override val lineNumber: Int
) extends Expression
