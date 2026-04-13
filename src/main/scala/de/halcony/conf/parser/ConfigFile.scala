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

sealed case class ScalarExpr(value: String, override val index: Int, override val lineNumber: Int)
  extends Expression

sealed case class InlineExpr(value: String, override val index: Int, override val lineNumber: Int)
  extends Expression

sealed case class CommentExpr(value: String, override val index: Int, override val lineNumber: Int)
  extends Expression

sealed case class VariableExpr(name: String, override val index: Int, override val lineNumber: Int)
  extends Expression

sealed case class MixedString(parts : List[Expression], override val index : Int, override val lineNumber : Int)
  extends Expression

sealed case class ForeignBlobExpr(
    name: String,
    content: String,
    override val index: Int,
    override val lineNumber: Int
) extends Expression

sealed case class ListExpr(
    values: List[Expression],
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
