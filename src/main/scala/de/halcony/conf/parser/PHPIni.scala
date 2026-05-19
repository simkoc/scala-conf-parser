package de.halcony.conf.parser

import fastparse.*
import MultiLineWhitespace.*
import fastparse.internal.Util
import wvlet.log.LogSupport

class PHPIni(content: String) extends LogSupport {

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
    CharsWhile(!Set('\n', ' ', '\t', ';', '=', '[', ']', '"', '\'').contains(_)).!
  )

  private[parser] def parseValueWithOperators[$: P]: P[String] = P(
    (parseNonBreakCharacter ~ (anyWhitespace.rep ~ ("&" | "|" | "^" | "~" | "!" | "-") ~ anyWhitespace.rep ~ parseNonBreakCharacter)
      .rep(1))
  ).map { (firstPart, restParts) =>
    // restParts is a Seq[Any] - we need to convert it to string
    s"$firstPart${restParts.mkString}"
  }

  private[parser] def parseValueWithSpaces[$: P]: P[String] = P(
    CharsWhile(!Set('\n', ';').contains(_)).!
  )

  private[parser] def parseFile[$: P]: P[ConfigFile] =
    P(Start ~ parsePHPDirective.rep ~ End).map(seq => ConfigFile(seq.toList, 0, getLineIndex(0)))

  private[parser] def parsePHPDirective[$: P]: P[Expression] = P(
    (parseComment | parseSection | parseDirective) ~ anyWhitespace.rep
  )

  private[parser] def parseComment[$: P]: P[CommentExpr] =
    P(anyWhitespace.rep ~ Index ~ (";" | "#") ~~/ (CharsWhile(_ != '\n') | "").! ~~/ ("\n" | End))
      .map((startIndex, comment) => CommentExpr(comment.trim, startIndex, getLineIndex(startIndex)))

  private[parser] def parseSection[$: P]: P[CommentExpr] = P(
    anyWhitespace.rep ~ Index ~ "[" ~ CharsWhile(_ != ']').! ~ "]" ~ anyWhitespace.rep
  ).map((startIndex, sectionName) => CommentExpr(sectionName, startIndex, getLineIndex(startIndex)))

  private[parser] def parseDirective[$: P]: P[CallExpr] = P(
    anyWhitespace.rep ~ Index ~ parseNonBreakCharacter ~ anyWhitespace.rep ~ Index ~ "=" ~ anyWhitespace.rep ~ parsePHPValue.? ~ anyWhitespace.rep
  ).map((startIndex, directiveName, equalsIndex, valueOption) =>
    CallExpr(
      NameExpr("=", equalsIndex, getLineIndex(equalsIndex)),
      List(
        NameExpr(directiveName, startIndex, getLineIndex(startIndex)),
        valueOption.getOrElse(ScalarExpr("", equalsIndex, getLineIndex(equalsIndex)))
      ),
      startIndex,
      getLineIndex(startIndex)
    )
  )

  private[parser] def parsePHPValue[$: P]: P[Expression] = P(
    Index ~ (parseQuotedString | parseNumber | parseValueWithSpaces | parseValueWithOperators | parseNonBreakCharacter)
  ).map { (startIndex, valueContent) =>
    // For quoted strings, preserve the quotes in the ScalarExpr
    // For other values (constants, etc.), use as-is
    ScalarExpr(valueContent, startIndex, getLineIndex(startIndex))
  }

  private[parser] def parseQuotedString[$: P]: P[String] = P(
    ("\"" ~ CharsWhile(_ != '"').! ~ "\"").map { content => s"\"$content\"" } |
      ("'" ~ CharsWhile(_ != '\'').! ~ "'").map { content => s"'$content'" }
  )

  // Removed parseBooleanValue since we want to allow any unquoted constants
  // via parseNonBreakCharacter

  private[parser] def parseNumber[$: P]: P[String] = P(
    ("-".? ~ CharsWhile(_.isDigit) ~ ("." ~ CharsWhile(_.isDigit)).? ~ CharsWhile(c =>
      c.isLetter && c.isUpper
    )).!
  )

  private[parser] def parseExpression[$: P]: P[String] = P(
    (parseNonBreakCharacter ~ ("&" | "|" | "^" | "~" | "!") ~ parseNonBreakCharacter).!
  )
}
