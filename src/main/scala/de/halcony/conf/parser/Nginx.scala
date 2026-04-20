package de.halcony.conf.parser

import fastparse.*
import MultiLineWhitespace.*
import fastparse.internal.Util
import wvlet.log.LogSupport

import scala.collection.immutable.{HashSet, StringOps}
import scala.util.matching.Regex

class Nginx(content: String) extends LogSupport {

  /** Checks if a given file contains indicators that it might not actually be an nginx config even
    * though it is called.conf and looks vaguely familiar
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
      "SecRule".r.unanchored,
      "SecAction".r.unanchored
    )
    !";".r.unanchored.matches(
      content
    ) || // if there is not a single ; it is most likely fubar anyways
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
      case failure: Parsed.Failure      =>
        logger.error(failure)
        Right(failure)
    }
  }

  private[parser] def anyWhitespace[$: P] = P(" " | "\t")

  private[parser] def parseFile[$: P]: P[ConfigFile] =
    P(
      Start ~ parseExpr.rep ~ End
    ).map(seq => ConfigFile(seq.toList, 0, getLineIndex(0))) // .log

  private[parser] def parseExpr[$: P]: P[Expression] = P(
    anyWhitespace.rep ~ (parseComment | parseControlStructure | parseBlock | parseCall)
  ) // .log

  private[parser] def parseControlStructure[$: P]: P[CondCtrlStructure] =
    P(parseIf)

  private[parser] def parseRegexpModifier[$: P]: P[NameExpr] = P(
    Index ~ "!".?.! ~~ ("^~" | "~*" | "~").!
  ).map((index, negated, modifier) =>
    NameExpr(
      s"$negated$modifier",
      index,
      getLineIndex(index)
    )
  )

  private[parser] def parseFullRegexpExpr[$: P]: P[CallExpr] = P(
    Index ~ (parseVariable | parseString | parseName) ~ parseRegexpModifier ~ (parseSingleQuoteString | parseDoubleQuoteString | parseRegex)
  ).map((index, operatedOn, modifier, regex) =>
    CallExpr(
      modifier,
      List(operatedOn, regex),
      index,
      getLineIndex(index)
    )
  ) // .log

  private[parser] def parseFullExactMatch[$: P]: P[CallExpr] = P(
    Index ~ (parseString | parseVariable) ~ ("=" | "^=" | "!=").! ~ (parseString | parseVariable)
  ).map((index, lhs, comp, rhs) =>
    CallExpr(
      NameExpr(comp, lhs.index, lhs.lineNumber),
      List(
        lhs,
        rhs
      ),
      index,
      getLineIndex(index)
    )
  )

  private[parser] def parseIf[$: P]: P[CondCtrlStructure] =
    P(
      Index ~ "if" ~/ "(" ~ (parseVariable ~ ")" | parseFullExactMatch ~ ")" | parseFullRegexpExpr ~ ")") ~/ parseExprBlock
    )
      .map((index, conditionExpr, block) =>
        CondCtrlStructure("if", conditionExpr, block.toList, List(), index, getLineIndex(index))
      ) // .log

  private[parser] def parseComment[$: P]: P[CommentExpr] =
    P(Index ~ "#" ~~/ (CharsWhile(_ != '\n') | "").! ~~/ ("\n" | End))
      .map((startIndex, comment) =>
        CommentExpr(comment.trim, startIndex, getLineIndex(startIndex))
      ) // .log

  private[parser] def parseRewriteCall[$: P]: P[CallExpr] = P(
    Index ~ &("rewrite") ~ parseName ~ parseRegex ~ parseArgumentList ~ ";"
  ).map((index, rewriteName, regexp, variables) =>
    CallExpr(
      rewriteName,
      List(
        regexp
      ) ++ variables,
      index,
      getLineIndex(index)
    )
  )

  private[parser] def parseServerName[$: P]: P[CallExpr] = P(
    Index ~ &("server_name") ~ parseName ~ parseRegex.rep ~ ";"
  ).map((index, serverNameName, regexps) =>
    CallExpr(
      serverNameName,
      regexps.toList,
      index,
      getLineIndex(index)
    )
  )

  private[parser] def parseSpecialCall[$: P]: P[CallExpr] = P(
    parseRewriteCall | parseServerName
  )

  private[parser] def parseGenericCall[$: P]: P[CallExpr] = P(
    Index ~ (parseName | parseVariable) ~ parseArgumentList.? ~ ";"
  ).map((startIndex, name, values) =>
    CallExpr(
      name,
      values.getOrElse(List()),
      startIndex,
      getLineIndex(startIndex)
    )
  ) // .log

  private[parser] def parseCall[$: P]: P[CallExpr] =
    P(parseSpecialCall | parseGenericCall) // .log

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
      s"set_by_lua_block ${variable.getVariableStringRepresentation}",
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

  private[parser] def parseBlockWithArguments[$: P]: P[BlockExpr] =
    P(
      Index ~ !"location" ~ parseName ~ parseArgumentList.? ~ parseExprBlock
    )
      .map((indexStart, blockNameExpr, argumentList, exprs) =>
        BlockExpr(
          Some(blockNameExpr),
          argumentList.getOrElse(List()),
          exprs.toList,
          indexStart,
          indexStart
        )
      ) // .log

  private[parser] def parseExactMatch[$: P]: P[(Expression, Expression)] = P(
    Index ~ &("=") ~ parseName ~ parseString
  ).map((index, exactMatchName, exactMatch) => (exactMatchName, exactMatch))

  private[parser] def parsePrefixMatch[$: P]: P[(Expression, Expression)] = P(
    Index ~ parseString
  ).map((index, prefixMatch) => (NameExpr("", index, getLineIndex(index)), prefixMatch))

  private[parser] def parseLocationBlock[$: P]: P[CondCtrlStructure] = P(
    Index ~ "location" ~ (parseExactMatch | parseRegexExprs | parsePrefixMatch) ~ parseExprBlock
  ).map((index, modMatch, block) =>
    CondCtrlStructure(
      "location",
      CallExpr(
        modMatch._1,
        List(
          NameExpr("location", index, getLineIndex(index)),
          modMatch._2
        ),
        index,
        getLineIndex(index)
      ),
      block.toList,
      List(),
      index,
      getLineIndex(index)
    )
  ) // .log

  private[parser] def parseMapElement[$: P]: P[ListExpr] = P(
    Index ~ (parseVariable | parseString) ~ (parseVariable | parseString) ~ ";"
  ).map((index, lhs, rhs) =>
    ListExpr(
      List(lhs, rhs),
      index,
      getLineIndex(index)
    )
  )

  private[parser] def parseMapBlock[$: P]: P[Expression] = P(
    Index ~ &("map") ~ parseName ~ parseVariable ~ parseVariable ~ "{" ~ parseMapElement.rep ~ "}"
  ).map((index, mapName, lhs, rhs, elements) =>
    CallExpr(
      mapName,
      List(lhs, rhs) ++ elements,
      index,
      getLineIndex(index)
    )
  ) // .log

  private[parser] def parseBlock[$: P]: P[Expression] = P(
    parseLocationBlock | parseLuaBLock | parseMapBlock | parseSetByLuaBlock | parseBlockWithArguments
  ) // .log

  private[parser] def parseExprBlock[$: P]: P[Seq[Expression]] = P(
    Index ~ !"${" ~ "{" ~ parseExpr.rep ~ "}"
  ).map((index, exprs) => exprs) // .log

  // I actually think we can also add regexp again as they seem to need to start with ~ indicator, keep an eye on that pattern
  private[parser] def parseArgumentListElement[$: P]: P[Expression] = P(
    Index ~ (parseString | parseVariable)
  ).map((index, content) => {
    // print(content)
    content
  }) // .log

  private[parser] def parseArgumentListHelper[$: P]: P[List[Expression]] = P(
    Index ~~ (parseArgumentListElement ~~ (" " | "\n" | "\t") ~~ parseArgumentListHelper | parseArgumentListElement)
  ).map { (index, content) =>
    content match {
      case (lhs, rhs)         => List(lhs) ++ rhs
      case single: Expression => List(single)
    }
  } // .log

  private[parser] def parseArgumentList[$: P]: P[List[Expression]] = P(
    Index ~~ parseArgumentListHelper // ~~ &(" " | (!"${" ~~ "{") | ";" | ")")
  ).map((index, content) => content) // .log

  private[parser] def parseInlineExpression[$: P]: P[InlineExpr] =
    P(Index ~~ "${" ~~ CharsWhile(!Set('{', '}').contains(_)).! ~~ "}")
      .map((startIndex, content) =>
        InlineExpr(content, startIndex, getLineIndex(startIndex))
      ) // .log

  /** There are four types of string single quote double quote no quote (may have variables) regexp
    * none of them may start with a $ or {
    *
    * @tparam $
    *   the parser context
    * @return
    *   the extracted expression
    */
  private[parser] def parseString[$: P]: P[StringExpression] =
    P(parseSingleQuoteString | parseDoubleQuoteString | parseMixedContentString).map(content => {
      // println(content)
      content
    }) // .log

  private[parser] def parseMixedContentStringElement[$: P]: P[ScalarExpr] =
    P(
      Index ~~ CharsWhile(!Set(' ', ';', '\n', '(', ')', '{', '}', '$', ';').contains(_)).!
    ).map((index, element) =>
      ScalarExpr(element, index, getLineIndex(index), Some("CharSeq"))
    ) // .log

  private[parser] def parseMixedContentSeq[$: P]: P[List[Expression]] = P(
    parseMixedContentStringElement ~~ parseMixedContentSeq
      | parseVariable ~~ parseMixedContentSeq
      | parseMixedContentStringElement ~~ parseVariable
      | parseVariable ~~ parseVariable
      | parseMixedContentStringElement
  ).map {
    case single: ScalarExpr => List(single)
    case (lhs, rhs)         =>
      lhs match {
        case lhs: ScalarExpr =>
          rhs match {
            case rhs: List[Expression]       => List(lhs) ++ rhs
            case rhs: VariableLikeExpression => List(lhs, rhs)
          }
        case lhs: VariableLikeExpression =>
          rhs match {
            case rhs: List[Expression]       => List(lhs) ++ rhs
            case rhs: VariableLikeExpression => List(lhs, rhs)
          }
      }
  } // .log

  /** cheap shot of covering all non " and ' strings, assuming that they will always be separated by
    * a space or finished by a ; We won't be able to capture the different parts and kinds though.
    *
    * @tparam $
    *   the parser contenxt
    * @return
    *   the parsed ScalarExpression
    */
  private[parser] def parseMixedContentString[$: P]: P[MixedString] = P(
    Index ~~ parseMixedContentSeq
  ).map((index, content) => MixedString(content, index, getLineIndex(index))) // .log

  private[parser] def parseRegexMatchingRoundBrackets[$: P]: P[String] = P(
    Index ~~ "(".! ~~ parseRegexStringAsString ~~ ")".!
  ).map((index, lhs, content, rhs) => s"$lhs$content$rhs")

  private[parser] def parseRegexMatchingCurlyBrackets[$: P]: P[String] = P(
    Index ~~ "{".! ~~ parseRegexStringAsString.! ~~ "}".!
  ).map((index, lhs, content, rhs) => s"$lhs$content$rhs")

  private[parser] def parseRegexStringAsString[$: P]: P[String] =
    P(
      Index ~~ (CharsWhile(
        !Set('{', '}', ' ', '\n', '(', ')', ';').contains(_)
      ).! | parseRegexMatchingRoundBrackets | parseRegexMatchingCurlyBrackets).repX(min = 1)
    ).map((index, content) => {
      s"${content.mkString}"
    })

  private[parser] def parseRegex[$: P]: P[ScalarExpr] = P(
    Index ~ parseRegexStringAsString
  ).map((index, regexpString) =>
    ScalarExpr(regexpString, index, getLineIndex(index), Some("RegEx"))
  )

  private[parser] def parseRegexExprs[$: P]: P[(NameExpr, ScalarExpr)] =
    P(Index ~ &("^=" | "~" | "~*" | "^~") ~ parseName ~ parseRegex).map((index, modifier, regexp) =>
      (modifier, regexp)
    ) // .log

  private[parser] def parseSingleQuoteString[$: P]: P[ScalarExpr] =
    P(
      Index ~ "'" ~~ ("\\" ~~ AnyChar | !"'" ~~ AnyChar).repX.! ~~ "'"
    ).map((startIndex, stringContent) =>
      ScalarExpr(stringContent, startIndex, getLineIndex(startIndex))
    )

  private[parser] def parseDoubleQuoteString[$: P]: P[ScalarExpr] =
    P(Index ~ "\"" ~~ ("\\" ~~ AnyChar | !"\"" ~~ AnyChar).repX.! ~~ "\"").map((index, ret) =>
      ScalarExpr(ret.mkString, index, getLineIndex(index))
    ) // .log

  private[parser] def parseVariable[$: P]: P[VariableLikeExpression] =
    P(
      parseDollarVariable | parseAtVariable | parseInlineExpression | parseDbrackedVariable | parseSbrackedVariable
    ).map(variable => {
      variable
    }) // .log

    /*
     * @variable0123 is a legal variable name
     */
  private[parser] def parseAtVariable[$: P]: P[VariableExpr] =
    P(Index ~~ "@" ~~ CharsWhileIn("a-zA-Z0-9_", min = 1).!)
      .map((startIndex, variableName) =>
        VariableExpr(variableName, startIndex, getLineIndex(startIndex))
      ) // .log

  /*
   * $variable0123 is a legal variable name
   */
  private[parser] def parseDollarVariable[$: P]: P[VariableExpr] =
    P(Index ~~ "$" ~~ !"{" ~~ CharsWhileIn("a-zA-Z0-9_", min = 1).!)
      .map((startIndex, variableName) =>
        VariableExpr(variableName, startIndex, getLineIndex(startIndex))
      ) // .log

  /*
   * {{variable}} is also a legal variable name
   */
  private[parser] def parseDbrackedVariable[$: P]: P[VariableExpr] =
    P(Index ~~ "{{" ~~ CharsWhileIn("a-zA-Z0-9_", min = 1).! ~~ "}}").map((index, content) =>
      VariableExpr(content, index, getLineIndex(index))
    ) // .log

  /*
   * {variable} is also a legal variable name
   */
  private[parser] def parseSbrackedVariable[$: P]: P[VariableExpr] =
    P(Index ~~ "{" ~~ CharsWhileIn("a-zA-Z0-9_", min = 1).! ~~ "}").map((index, content) => {
      // println(content)
      VariableExpr(content, index, getLineIndex(index))
    }) // .log

  private[parser] def parseName[$: P]: P[NameExpr] = P(Index ~ parseNonBreakCharacters)
    .map((indexStart, name) => NameExpr(name, indexStart, getLineIndex(indexStart))) // .log

  private[parser] def parseNonBreakCharacters[$: P]: P[String] = P(
    !"$" ~~ CharsWhile(!Set('\n', ' ', '\t', ';', '{', '}', '$', '/', '(', ')').contains(_)).!
  )
}
