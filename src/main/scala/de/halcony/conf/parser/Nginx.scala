package de.halcony.conf.parser

import fastparse.*
import MultiLineWhitespace._
import fastparse.internal.Util
import wvlet.log.LogSupport

object Nginx extends LogSupport {

  def process(content: String): Option[ConfigFile] = {
    parse(content, parseFile(using _)) match {
      case Parsed.Success(value, index) => Some(value)
      case failure: Parsed.Failure      =>
        logger.error(failure)
        None
    }
  }

  private[parser] def anyWhitespace[$: P] = P(" " | "\t")

  private[parser] def parseNonBreakCharacter[$: P]: P[String] = P(
    CharsWhile(!Set('\n', ' ', '\t', ';').contains(_)).!
  )

  private[parser] def parseFile[$: P]: P[ConfigFile] =
    P(Start ~ parseExpr.rep ~ End).map(seq => ConfigFile(seq.toList, 0, -1))

  private[parser] def parseExpr[$: P]: P[Expression] = P(
    anyWhitespace.rep ~ (parseComment | parseAssignment | parseBlock)
  )

  private[parser] def parseComment[$: P]: P[CommentExpr] =
    P(Index ~ "#" ~/ CharsWhile(_ != '\n').! ~ "\n".?)
      .map((startIndex, comment) => CommentExpr(comment.trim, startIndex, -1)) // .log

  private[parser] def parseName[$: P]: P[NameExpr] = P(Index ~ parseNonBreakCharacter)
    .map((indexStart, name) => NameExpr(name, indexStart, -1)) // .log

  private[parser] def parseScalar[$: P]: P[ScalarExpr] = P(Index ~ parseNonBreakCharacter)
    .map((startIndex, value) => ScalarExpr(value, startIndex, -1)) // .log

  private[parser] def parseScalarList[$: P]: P[ListExpr] = P(Index ~ (parseScalar ~ " ".?).rep)
    .map((indexStart, listExpr) => ListExpr(listExpr.toList, indexStart, -1)) // .log

  private[parser] def parseAssignment[$: P]: P[AssignmentExpr] =
    P(Index ~ parseName ~ !"{" ~ parseScalarList ~ ";")
      .map((startIndex, name, value) =>
        AssignmentExpr(
          name,
          if value.values.length == 1 then value.values.head else value,
          startIndex,
          -1
        )
      ) // .log

  private[parser] def parseBlock[$: P]: P[BlockExpr] =
    P(Index ~ parseName ~ "{" ~ parseExpr.rep ~ "}")
      .map((indexStart, nameExpr, exprs) =>
        BlockExpr(Some(nameExpr), exprs.toList, indexStart, -1)
      ) // .log

}
