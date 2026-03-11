package de.halcony.conf.parser

import fastparse.*
import MultiLineWhitespace.*
import fastparse.internal.Util
import wvlet.log.LogSupport

class Nginx(content: String) extends LogSupport {

  private def createLineIndex: Map[Int, Int] = {
    var currentLine = 1
    var currentChar = 0
    this.content.map { char =>
      try {
        if (char == '\n') {
          currentLine = currentLine + 1
        }
        currentChar -> currentLine
      } finally {
        currentChar = currentChar + 1
      }
    }.toMap
  }

  private val lineIndex: Map[Int, Int] = createLineIndex

  def getLineIndex(charOffset: Int): Int = {
    lineIndex.getOrElse(charOffset, -1)
  }

  def process(): Either[ConfigFile, fastparse.Parsed.Failure] = {
    parse(this.content, parseFile(using _)) match {
      case Parsed.Success(value, index) => Left(value)
      case failure: Parsed.Failure      =>
        logger.error(failure)
        Right(failure)
    }
  }

  private[parser] def anyWhitespace[$: P] = P(" " | "\t")

  private[parser] def parseNonBreakCharacter[$: P]: P[String] = P(
    CharsWhile(!Set('\n', ' ', '\t', ';', '{', '}', '$').contains(_)).!
  )

  private[parser] def parseFile[$: P]: P[ConfigFile] =
    P(Start ~ parseExpr.rep ~ End).map(seq => ConfigFile(seq.toList, 0, getLineIndex(0)))

  private[parser] def parseExpr[$: P]: P[Expression] = P(
    anyWhitespace.rep ~ (parseComment | parseBlockWithArguments | parseCall | parseLuaBLock)
  )

  private[parser] def parseSingleQuoteStringString[$: P]: P[String] =
    P(Index ~ "'" ~ CharsWhile(_ != '\'').! ~ "'").map((startIndex, stringContent) => stringContent)

  private[parser] def parseDoubleQuoteString[$: P]: P[String] =
    P(Index ~ "\"" ~ CharsWhile(_ != '"').! ~ "\"").map((startIndex, stringContent) =>
      stringContent
    )

  private[parser] def parseString[$: P]: P[String] =
    P(parseSingleQuoteStringString | parseDoubleQuoteString)
      .map(stringContent => stringContent)

  private[parser] def parseComment[$: P]: P[CommentExpr] =
    P(Index ~ "#" ~~/ (CharsWhile(_ != '\n') | "").! ~~/ ("\n" | End))
      .map((startIndex, comment) =>
        CommentExpr(comment.trim, startIndex, getLineIndex(startIndex))
      ) // .log

  private[parser] def parseName[$: P]: P[NameExpr] = P(Index ~ parseNonBreakCharacter)
    .map((indexStart, name) => NameExpr(name, indexStart, getLineIndex(indexStart))) // .log

  private[parser] def parseScalar[$: P]: P[ScalarExpr] =
    P(Index ~ (parseString | parseNonBreakCharacter))
      .map((startIndex, value) => ScalarExpr(value, startIndex, getLineIndex(startIndex))) // .log

  private[parser] def parseInlineExpression[$: P]: P[InlineExpr] =
    P(Index ~ "${" ~ parseNonBreakCharacter ~ "}")
      .map((startIndex, content) =>
        InlineExpr(content, startIndex, getLineIndex(startIndex))
      ) // .log

  private[parser] def parseVariable[$: P]: P[VariableExpr] =
    P(Index ~ "$" ~ parseNonBreakCharacter.!)
      .map((startIndex, variableName) =>
        VariableExpr(variableName, startIndex, getLineIndex(startIndex))
      ) // .log

  private[parser] def parseArgumentList[$: P]: P[List[Expression]] = P(
    Index ~ ((parseScalar | parseInlineExpression | parseVariable) ~ " ".?).rep
  ).map((_, values) => values.toList) // .log

  private[parser] def parseCall[$: P]: P[CallExpr] =
    P(Index ~ parseName ~ !"{" ~ parseArgumentList ~ ";")
      .map((startIndex, name, values) =>
        CallExpr(
          name,
          values,
          startIndex,
          getLineIndex(startIndex)
        )
      ) // .log

  private[parser] def luaBlocKName[$: P]: P[String] = P(
    ("init_by_lua_block" | "balancer_by_lua_block").!
  )

  private[parser] def parseLuaBLock[$: P]: P[ForeignBlobExpr] =
    P(Index ~ luaBlocKName ~ "{" ~ CharsWhile(_ != '}').! ~ "}")
      .map((index, luaBlockName, content) =>
        ForeignBlobExpr(luaBlockName, content, index, getLineIndex(index))
      ) // .log

  private[parser] def parseBlock[$: P]: P[BlockExpr] =
    P(Index ~ !luaBlocKName ~ parseName ~ !"${" ~ "{" ~/ parseExpr.rep ~ "}")
      .map((indexStart, nameExpr, exprs) =>
        BlockExpr(Some(nameExpr), List(), exprs.toList, indexStart, getLineIndex(indexStart))
      ) // .log

  private[parser] def parseBlockWithArguments[$: P]: P[BlockExpr] =
    P(
      Index ~ !luaBlocKName ~ parseName ~ "=".? ~ parseArgumentList ~ !"${" ~ "{" ~/ parseExpr.rep ~ "}"
    )
      .map((indexStart, blockNameExpr, argumentList, exprs) =>
        BlockExpr(Some(blockNameExpr), argumentList, exprs.toList, indexStart, indexStart)
      ) // .log
}
