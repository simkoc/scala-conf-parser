package de.halcony.conf.parser

import fastparse.*
import MultiLineWhitespace.*
import fastparse.internal.Util
import wvlet.log.LogSupport

import scala.collection.immutable.{HashSet, StringOps}
import scala.util.matching.Regex

class Nginx(content: String) extends LogSupport {

  /** Checks if a given file contains indicators that it might not actually
   * be an nginx config even though it is called.conf and looks vaguely familiar
   *
   * @return
   */
  def checkForKnownNotNginxConfEndingOnConf(): Boolean = {
    val indicators: HashSet[Regex] = HashSet(
      "^\\[supervisord\\]".r.unanchored,
      "OWASP ModSecurity Core Rule Set".r.unanchored,
      "Configuration File for JavaScript Lint".r.unanchored,
      "this is merely a common Makefile".r.unanchored,
      "ModSecurity Console receiving URI".r.unanchored,
      "Rule engine initialization".r.unanchored,
      "owasp-modsecurity-crs".r.unanchored,
      "<VirtualHost".r.unanchored,
    )
    !";".r.unanchored.matches(content) || // if there is not a single ; it is most likely fubar anyways
      indicators.exists(regex => regex.unanchored.matches(content))
  }

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
      case failure: Parsed.Failure =>
        logger.error(failure)
        Right(failure)
    }
  }

  private[parser] def anyWhitespace[$: P] = P(" " | "\t")

  private[parser] def parseFile[$: P]: P[ConfigFile] =
    P(Start ~ parseExpr.rep ~ End).map(seq => ConfigFile(seq.toList, 0, getLineIndex(0)))//.log

  private[parser] def parseExpr[$: P]: P[Expression] = P(
    anyWhitespace.rep ~ (parseComment | parseBlockWithArguments | parseCall | parseLuaBLock | parseSetByLuaBlock) // | parseBlock)
  )//.log

  private[parser] def parseComment[$: P]: P[CommentExpr] =
    P(Index ~ "#" ~~/ (CharsWhile(_ != '\n') | "").! ~~/ ("\n" | End))
      .map((startIndex, comment) =>
        CommentExpr(comment.trim, startIndex, getLineIndex(startIndex))
      ) // .log



  private[parser] def parseCall[$: P]: P[CallExpr] =
    P(Index ~ (parseName | parseVariable) ~ parseArgumentList ~ ";")
      .map((startIndex, name, values) =>
        CallExpr(
          name,
          values,
          startIndex,
          getLineIndex(startIndex)
        )
      )//.log

  private[parser] def luaBlockName[$: P]: P[String] = P(
    ("rewrite_by_lua_block" |
      "set_by_lua_block" |
      "log_by_lua_block" |
      "init_by_lua_block" |
      "init_worker_by_lua_block" |
      "balancer_by_lua_block" |
      "content_by_lua_block" |
      "access_by_lua_block" |
      "header_filter_by_lua_block" |
      "ssl_certificate_by_lua_block" |
      "body_filter_by_lua_block").!
  ).map(blockName => blockName)

  private[parser] def parseSetByLuaBlock[$: P]: P[ForeignBlobExpr] = P(
    Index ~ "set_by_lua_block" ~ parseVariable ~ "{" ~ readBlobContainingMatchingBrackets ~ "}"
  ).map((index, variable, blob) =>
    ForeignBlobExpr(
      s"set_by_lua_block ${variable.name}",
      blob,
      index,
      getLineIndex(index)
    )
  )

  private[parser] def readNoBrackets[$: P]: P[String] =
    P(Index ~~ CharsWhile(!Set('{', '}').contains(_)).!).map((index, content) => {
      // println(content)
      content
    }) // .log

  private[parser] def ensureMatchingBrackets[$: P]: P[String] =
    // todo: I do not know how to prevent it but we are swallowing a \n here, but given that this is a blob it does not matter anyways
    P(Index ~ "{".! ~~/ readBlobContainingMatchingBrackets ~/ "}".!).map {
      case (index, open, content, close) =>
        s"$open$content$close"
    } // .log

  private[parser] def readBlobContainingMatchingBrackets[$: P]: P[String] =
    P(Index ~ (!"{" ~~ readNoBrackets | ensureMatchingBrackets).rep).map((index, content) =>
      content.mkString("")
    ) // .log

  private[parser] def parseLuaBLock[$: P]: P[ForeignBlobExpr] =
    P(Index ~ luaBlockName ~ "{" ~ readBlobContainingMatchingBrackets ~ "}")
      .map((index, luaBlockName, content) =>
        ForeignBlobExpr(luaBlockName, content, index, getLineIndex(index))
      ) // .log


  private[parser] def parseExprBlock[$ : P] : P[Seq[Expression]] = P(
    Index ~ !"${" ~ "{" ~ parseExpr.rep ~ "}"
  ).map(
    (index, exprs) => exprs
  )//.log

  private[parser] def parseBlockWithArguments[$: P]: P[BlockExpr] =
    P(
      Index ~ !luaBlockName ~ parseName ~ "=".? ~ parseArgumentList ~ parseExprBlock
    )
      .map((indexStart, blockNameExpr, argumentList, exprs) =>
        BlockExpr(Some(blockNameExpr), argumentList, exprs.toList, indexStart, indexStart)
      )//.log

  private[parser] def parseArgumentList[$: P]: P[List[Expression]] = P(
    Index ~ (parseString | parseInlineExpression | parseVariable).rep)
    .map((_, values) => values.toList)//.log

  private[parser] def parseInlineExpression[$: P]: P[InlineExpr] =
    P(Index ~ "${" ~ CharsWhile(!Set('{', '}').contains(_)).! ~ "}")
      .map((startIndex, content) =>
        InlineExpr(content, startIndex, getLineIndex(startIndex))
      )//.log

  /** There are four types of string
   *  single quote
   *  double quote
   *  no quote (may have variables)
   *  regexp
   *  none of them may start with a $ or {
   *
   * @tparam $ the parser context
   * @return the extracted expression
   */
  private[parser] def parseString[$: P]: P[ScalarExpr] =
    P(!("$" | "{") ~ (parseSingleQuoteStringString | parseDoubleQuoteStringWithEscapedDoubleQuote | parseMixedContentString)).map(
      content => {
        //println(content)
        content
      }
    )//.log

  /** cheap shot of covering all non " and ' strings, assuming that they will always be separated by a space or finished by a ;
   *  We won't be able to capture the different parts and kinds though.
   *
   * @tparam $ the parser contenxt
   * @return the parsed ScalarExpression
   */
  private[parser] def parseMixedContentString[$ : P] : P[ScalarExpr] = {
    P(Index ~ CharsWhile(!Set(' ',';').contains(_)).!).map(
      (index,content) => ScalarExpr(content,index,getLineIndex(index))
    )//.log
  }

  private[parser] def parseSingleQuoteStringString[$: P]: P[ScalarExpr] =
      P(Index ~ "'" ~ CharsWhile(_ != '\'').! ~ "'").map((startIndex, stringContent) => ScalarExpr(stringContent,startIndex,getLineIndex(startIndex)))

  private[parser] def parseDoubleQuoteStringWithEscapedDoubleQuote[$: P]: P[ScalarExpr] =
      P(Index ~ "\"" ~ (!"\"" ~ (!"\\\"" ~ AnyChar.! | "\\\"".!)).rep ~ "\"").map(
        (index, ret) => ScalarExpr(ret.mkString,index,getLineIndex(index))
      ) //.log

  private[parser] def parseVariable[$: P]: P[VariableExpr] =
    P(
      parseDollarVariable | parseDbrackedVariable | parseSbrackedVariable
    ) //.log

  private[parser] def parseDollarVariable[$: P]: P[VariableExpr] =
      P(Index ~ "$" ~ !"{" ~ parseNonBreakCharacters.!)
        .map((startIndex, variableName) =>
          VariableExpr(variableName, startIndex, getLineIndex(startIndex))
        )//.log

  private[parser] def parseDbrackedVariable[$: P]: P[VariableExpr] =
    P(Index ~ "{{" ~~ (!("}}" | ";" | "\"" | "\n") ~~ AnyChar).repX(min=1).! ~~ "}}").map(
      (index, content) => VariableExpr(content, index, getLineIndex(index))
    )//.log

  private[parser] def parseSbrackedVariable[$: P]: P[VariableExpr] =
      P(Index ~ "{" ~~ (!("{" | "}" | ";" | "\"" | "\n") ~~ AnyChar).repX(min=1).! ~~ "}").map(
        (index, content) => {
          //println(content)
          VariableExpr(content, index, getLineIndex(index))
        }
      )//.log

  private[parser] def parseRegexpString[$: P]: P[ScalarExpr] =
    P(Index ~ parseNonBreakCharacters.! ~ "$").map(
      (index, content) => ScalarExpr("$content$$",index,getLineIndex(index))
    )//.log

  private[parser] def parseName[$: P]: P[NameExpr] = P(Index ~ parseNonBreakCharacters)
    .map(
      (indexStart, name) => NameExpr(name, indexStart, getLineIndex(indexStart))
    )//.log

  private[parser] def parseNonBreakCharacters[$: P]: P[String] = P(
    !"$" ~ CharsWhile(!Set('\n', ' ', '\t', ';', '{', '}', '$').contains(_)).!
  )
}