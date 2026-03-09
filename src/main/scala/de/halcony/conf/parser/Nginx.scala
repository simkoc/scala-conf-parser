package de.halcony.conf.parser

import fastparse.*
import MultiLineWhitespace._
import fastparse.internal.Util
import wvlet.log.LogSupport

object Nginx extends LogSupport {

  def process(content: String): Either[ConfigFile, fastparse.Parsed.Failure] = {
    parse(content, parseFile(using _)) match {
      case Parsed.Success(value, index) => Left(value)
      case failure: Parsed.Failure      =>
        logger.error(failure)
        Right(failure)
    }
  }

  private[parser] def anyWhitespace[$: P] = P(" " | "\t")

  private[parser] def parseNonBreakCharacter[$: P]: P[String] = P(
    CharsWhile(!Set('\n', ' ', '\t', ';', '{', '}').contains(_)).!
  )

  private[parser] def parseFile[$: P]: P[ConfigFile] =
    P(Start ~ parseExpr.rep ~ End).map(seq => ConfigFile(seq.toList, 0, -1))

  private[parser] def parseExpr[$: P]: P[Expression] = P(
    anyWhitespace.rep ~ (parseComment | parseBlock | parseBlockWithArgument | parseCall)
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

  private[parser] def parseCall[$: P]: P[CallExpr] =
    P(Index ~ parseName ~ !"{" ~ parseScalarList ~ ";")
      .map((startIndex, name, value) =>
        CallExpr(
          name,
          value.values,
          startIndex,
          -1
        )
      ) // .log

  private[parser] def parseBlock[$: P]: P[BlockExpr] =
    P(Index ~ parseName ~ "{" ~ parseExpr.rep ~ "}")
      .map((indexStart, nameExpr, exprs) =>
        BlockExpr(Some(nameExpr), None, exprs.toList, indexStart, -1)
      ) // .log

  private[parser] def parseBlockWithArgument[$: P]: P[BlockExpr] =
    P(Index ~ parseName ~ "=".? ~ parseScalar ~ "{" ~ parseExpr.rep ~ "}")
      .map((indexStart, blockNameExpr, nameExpr, exprs) =>
        BlockExpr(Some(blockNameExpr), Some(nameExpr), exprs.toList, indexStart, -1)
      ) // .log
}
