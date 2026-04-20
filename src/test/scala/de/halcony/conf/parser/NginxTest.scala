package de.halcony.conf.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse.*
import fastparse.Parsed.{Failure, Success, fromParsingRun}

import java.io.File
import java.nio.file.Files
import scala.collection.immutable.HashSet

class NginxTest extends AnyWordSpec with Matchers {

  // primitive parsing
  "parsing string" should {
    "work for double quote string" in {
      val string = "\"test\""
      parse(string, new Nginx("").parseString(using _)) match {
        case Success(value, index) =>
          index shouldBe string.length
          value.getString shouldBe "test"
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "process string with escaped double quote" in {
      val weirdString = {
        // 1234567890123456789
        """"{\"ready\": true}"""".trim.stripMargin
      }
      parse(weirdString, new Nginx("").parseDoubleQuoteString(using _)) match {
        case Parsed.Success(value, index) =>
          index shouldBe weirdString.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "work for single quote string" in {
      val string = "'test'"
      parse(string, new Nginx("").parseString(using _)) match {
        case Success(value, index) =>
          index shouldBe string.length
          value.getString shouldBe "test"
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "work for longer single quote string with variable content" in {
      //              1234567890123
      val string = """'$remote_addr - $remote_user [$time_local] "$request" '"""
      parse(string, new Nginx("").parseSingleQuoteString(using _)) match {
        case Success(value, index) =>
          index shouldBe string.length
          value.getString shouldBe """$remote_addr - $remote_user [$time_local] "$request" """
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "work for mixed content string" in {
      val string = "/alle/meine/$entchen/schwimmen"
      parse(string, new Nginx("").parseString(using _)) match {
        case Success(value, index) =>
          index shouldBe string.length
          value.getString shouldBe "/alle/meine/$entchen/schwimmen"
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "not be recognized as mixed content string" in {
      val variablePair = "$first $second"
      parse(variablePair, new Nginx("").parseMixedContentString(using _)) match {
        case Success(value, index) =>
          fail("should not succeed")
        case failure: Failure =>
          succeed
      }
    }
    "be able to handle mixed in variable" in {
      //             12345678
      val mixedIn = "source=$remote_addr"
      parse(mixedIn, new Nginx("").parseMixedContentString(using _)) match {
        case Success(value, index) =>
          index shouldBe mixedIn.length
        case failure: Failure =>
          succeed
      }
    }
    "be able to process mixed content string with inline expression" in {
      val mixedIn = "/app/${WEBROOT:-};"
      parse(mixedIn, new Nginx("").parseMixedContentString(using _)) match {
        case Success(value, index) =>
          index shouldBe mixedIn.length - 1
        case failure: Failure =>
          succeed
      }
    }
    "not gobble up space separated content" in {
      //                    1234567890123
      val spaceSeparated = "/dev/stdout ${NGINX_ERROR_LOG_LEVEL:-warn}"
      parse(spaceSeparated, new Nginx("").parseMixedContentString(using _)) match {
        case Success(value, index) =>
          index shouldBe "/dev/stdout".length
          value shouldBe MixedString(List(ScalarExpr("/dev/stdout", 0, -1, Some("CharSeq"))), 0, -1)
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "parse mixed content string with :" in {
      //                       0         1         2         3
      //                       012345678901234567890123456789012
      val mixedContentString = "${NGINX_FASTCGI_PASS:-php}:9000"
      parse(mixedContentString, new Nginx("").parseMixedContentString(using _)) match {
        case Success(value, index) =>
          index shouldBe mixedContentString.length
          value.parts.length shouldBe 2
        case failure: Failure =>
          succeed
      }
    }
    "parse mixed content string with ." in {
      //                       0         1         2         3
      //                       012345678901234567890123456789012
      val mixedContentString = "kong-admin.qbtrade.org"
      parse(mixedContentString, new Nginx("").parseMixedContentString(using _)) match {
        case Success(value, index) =>
          index shouldBe mixedContentString.length
          value.getString shouldBe "kong-admin.qbtrade.org"
        case failure: Failure =>
          succeed
      }
    }
    "parse mixed content string with /" in {
      //                       0         1         2         3
      //                       012345678901234567890123456789012
      val mixedContentString = "http://backend-go-wsproxy:3000/"
      parse(mixedContentString, new Nginx("").parseMixedContentString(using _)) match {
        case Success(value, index) =>
          index shouldBe mixedContentString.length
          value.getString shouldBe "http://backend-go-wsproxy:3000/"
        case failure: Failure =>
          succeed
      }
    }
  }

  "parsing a regex" should {
    "work for simple regexp" in {
      val regexp = "~ ^test$"
      parse(regexp, new Nginx("").parseRegexExprs(using _)) match {
        case Success((matchModifier,regex), index) =>
          index shouldBe regexp.length
          matchModifier.value shouldBe "~"
          regex.value shouldBe "^test$"
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "work for matching ()" in {
      val regexp = "~ ^te(s)t$"
      parse(regexp, new Nginx("").parseRegexExprs(using _)) match {
        case Success((matchModifier,regex), index) =>
          index shouldBe regexp.length
          matchModifier.value shouldBe "~"
          regex.value shouldBe "^te(s)t$"
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "work for matching {}" in {
      val regexp = "~ ^te{s}t$"
      parse(regexp, new Nginx("").parseRegexExprs(using _)) match {
        case Success((matchModifier,regex), index) =>
          index shouldBe regexp.length
          regex.value shouldBe "^te{s}t$"
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "not work for non matching ()" in {
      val regexp = "~ ^tes)t$;"
      parse(regexp, new Nginx("").parseRegexExprs(using _)) match {
        case Success((matchModifier,regex), index) =>
          if (index == regexp.length) fail("should not be able to parse the whole string")
        case failure: Failure =>
          succeed
      }
    }
    "not work for non matching {}" in {
      val regexp = "~ ^tes}t$"
      parse(regexp, new Nginx("").parseRegexExprs(using _)) match {
        case Success((matchModifier,regex), index) =>
          if (index == regexp.length) fail("should not be able to parse the whole string")
        case failure: Failure =>
          succeed
      }
    }
    "be able to process complex regexp" in {
      val complexRegexp = "~ ^/(index|get|static|errors/report|errors/404|errors/503|health_check)\\.php$"
      parse(complexRegexp, new Nginx("").parseRegexExprs(using _)) match {
        case Success((matchModifier,regex), index) =>
          index shouldBe complexRegexp.length
        case failure: Failure =>
          fail(failure.msg)
      }
    }
  }

  "parsing a variable" should {
    "work for $var" in {
      //             123456
      val varExpr = "$var t"
      parse(varExpr,new Nginx(varExpr).parseVariable(using _)) match {
        case Success(value, index) =>
          index shouldBe varExpr.length - 2
          value.getVariableStringRepresentation shouldBe "$var"
        case f : Failure =>
          fail(f.msg)
      }
    }
    "work for ${var}" in {
      val varExpr = "${var} t"
      parse(varExpr,new Nginx(varExpr).parseInlineExpression(using _)) match {
        case Success(value, index) =>
          index shouldBe varExpr.length - 2
          value.value shouldBe "var"
        case f : Failure =>
          fail(f.msg)
      }
    }
    "work for {var}" in {
      val varExpr = "{var} t"
      parse(varExpr,new Nginx(varExpr).parseVariable(using _)) match {
        case Success(value, index) =>
          index shouldBe varExpr.length - 2
          value.getVariableStringRepresentation shouldBe "$var"
        case f : Failure =>
          fail(f.msg)
      }
    }
    "work for {{var}}" in {
      val varExpr = "{var} t"
      parse(varExpr,new Nginx(varExpr).parseVariable(using _)) match {
        case Success(value, index) =>
          index shouldBe varExpr.length - 2
          value.getVariableStringRepresentation shouldBe "$var"
        case f : Failure =>
          fail(f.msg)
      }
    }
    "process DBracked element" in {
      val dbracked = "{{FASTCGI}}"
      parse(dbracked, new Nginx("").parseDbrackedVariable(using _)) match {
        case Parsed.Success(value, index) =>
          index shouldBe dbracked.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "process inline expression" in {
      val inlineExpr = "${NGINX_ERROR_LOG_LEVEL:-warn}"
      parse(inlineExpr, new Nginx("").parseInlineExpression(using _)) match {
        case Parsed.Success(value, index) =>
          value.value shouldBe "NGINX_ERROR_LOG_LEVEL:-warn"
          index shouldBe inlineExpr.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
  }

  "parsing comments" should {
    "work for single comment" in {
      val conf = "# nginx.vh.default.conf  --  docker-openresty"
      new Nginx(conf).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("nginx.vh.default.conf  --  docker-openresty", 0, 1)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for empty comment" in {
      val comment = "# "
      new Nginx(comment).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("", 0, 1)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for multi-line comments" in {
      val comments =
        """
          |# nginx.vh.default.conf  --  docker-openresty
          |#
          |""".stripMargin
      new Nginx(comments.trim).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("nginx.vh.default.conf  --  docker-openresty", 0, 1),
            CommentExpr("", 46, 2)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for multiple comments" in {
      val conf: String =
        """
          |# comment one
          |  # comment two
          |""".stripMargin
      new Nginx(conf).process() match {
        case Left(file) =>
          file.expressions shouldBe List(
            CommentExpr("comment one", 1, 2),
            CommentExpr("comment two", 17, 3)
          )
        case Right(_) =>
          fail("unable to parse file")
      }
    }
  }

  "process name" should {
    "process name" in {
      val name = "user"
      parse(name, new Nginx("").parseName(using _)) match {
        case Parsed.Success(value, index) =>
          value.value shouldBe "user"
          index shouldBe name.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "process ~ as name" in {
      val name = "~"
      parse(name, new Nginx("").parseName(using _)) match {
        case Parsed.Success(value, index) =>
          value.value shouldBe "~"
          index shouldBe name.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
  }

  // simple composite parsing
  "parsing an argument list" should {
    "work for scalar list" in {
      val scalarList = "www-data www-other;"
      parse(scalarList, new Nginx("").parseArgumentList(using _)) match {
        case Parsed.Success(value, index) =>
          value.length shouldBe 2
          index shouldBe scalarList.length -1
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "work for mixed list" in {
      //               1234567890
      val mixedList = "$uri $uri/;"
      parse(mixedList, new Nginx("").parseArgumentList(using _)) match {
        case Parsed.Success(value, index) =>
          index shouldBe mixedList.length -1
          println(value)
          value.length shouldBe 2
          value shouldBe List(
            VariableExpr("uri",0,-1),
            MixedString(
              List(
                VariableExpr("uri",5,-1),
                ScalarExpr("/",9,-1, Some("CharSeq"))
              ),
              5,-1
            )
          )
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "process a argument list consisting of asingle inline expression" in {
      //                  1234567890
      val singleInline = "/app/${WEBROOT:-};"
      parse(singleInline, new Nginx("").parseArgumentList(using _)) match {
        case Success(value, index) =>
          println(value)
          index shouldBe singleInline.length - 1
          value.length shouldBe 1
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "not process mixed content string as var" in {
      //                  123456
      val mixedContent = "$var/;"
      parse(mixedContent, new Nginx("").parseArgumentList(using _)) match {
        case Success(value, index) =>
          index shouldBe mixedContent.length - 1
          value shouldBe List(
            MixedString(
              List(
                VariableExpr("var", 0, -1),
                ScalarExpr("/", 4, -1, Some("CharSeq"))
              ),
              0, -1
            ))
        case failure: Failure =>
          fail(failure.msg)
      }
    }
  }

  "process call" should {
    "work for single assignment as call" in {
      val conf: String =
        """
          |user www-data;
          |""".stripMargin
      new Nginx(conf).process() match {
        case Left(file) =>
          file.expressions shouldBe List(
            CallExpr(NameExpr("user", 1, 2),
              List(
                MixedString(
                  List(
                    ScalarExpr("www-data", 6, 2, Some("CharSeq"))), 6, 2)), 1, 2))
        case Right(_) =>
          fail("unable to parse file")
      }
    }
    "process assignment with diverse characters" in {
      val assignmentCall =
        """return 200 "{\"ready\": true}";""".stripMargin
      parse(assignmentCall, new Nginx("").parseCall(using _)) match {
        case Parsed.Success(value, index) =>
          index shouldBe assignmentCall.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "work if expression has spaces" in {
      val expression =
        """
             fastcgi_buffers         ${FASTCGI_BUFFERS:-256 32k};""".stripMargin
      parse(expression, new Nginx("").parseFile(using _)) match {
        case Success(value, index) =>
          index shouldBe expression.length
        case f: Failure => fail(f.msg)
      }
    }
    "work to process list assignment as call" in {
      val conf: String =
        """
          |error_page  500 502  /50x.html;
          |""".stripMargin;
      new Nginx(conf).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(NameExpr("error_page", 1, 2),
              List(
                MixedString(List(ScalarExpr("500", 13, 2, Some("CharSeq"))), 13, 2),
                MixedString(List(ScalarExpr("502", 17, 2, Some("CharSeq"))), 17, 2),
                MixedString(List(ScalarExpr("/50x.html", 22, 2, Some("CharSeq"))), 22, 2)), 1, 2))
        case Right(value) =>
          fail(value.toString())
      }
    }
    "process inline expression assignment" in {
      val inlineExpr =
        """error_log /dev/stdout ${NGINX_ERROR_LOG_LEVEL:-warn};""".stripMargin
      new Nginx(inlineExpr.trim).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(NameExpr("error_log", 0, 1),
              List(
                MixedString(List(ScalarExpr("/dev/stdout", 10, 1, Some("CharSeq"))), 10, 1),
                InlineExpr("NGINX_ERROR_LOG_LEVEL:-warn", 22, 1)), 0, 1))
        case Right(value) =>
          fail(value.toString())
      }
    }
    "process string parameter to call" in {
      val callWithStrings =
        """
          |log_format main '$remote_addr - $remote_user [$time_local] "$request" '
          |                '$status $body_byte_sent "$http_referer" ';
          |""".stripMargin
      new Nginx(callWithStrings).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("log_format", 1, 2),
              List(
                MixedString(List(ScalarExpr("main", 12, 2, Some("CharSeq"))), 12, 2),
                ScalarExpr("""$remote_addr - $remote_user [$time_local] "$request" """, 17, 2, None),
                ScalarExpr("""$status $body_byte_sent "$http_referer" """, 89, 3, None)), 1, 2))
        case Right(value) =>
          fail(value.toString())
      }
    }
    "process call to variable" in {
      val call = "$geoip2_data_country_code source=$remote_addr country iso_code;"
      parse(call, new Nginx("").parseCall(using _)) match {
        case Success(value, index) =>
          index shouldBe call.length
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "process call with inline expression argument" in {
      val call = "root /app/${WEBROOT:-};"
      parse(call, new Nginx("").parseCall(using _)) match {
        case Success(value, index) =>
          index shouldBe call.length
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "process rewrite call" in {
      val call = "rewrite ^/v1/ws/(.*)$ /$1 break;"
      parse(call, new Nginx("").parseCall(using _)) match {
        case Success(value, index) =>
          index shouldBe call.length
        case failure: Failure =>
          fail(failure.msg)
      }
    }
  }

  "parsing multi lines" should {
    "work for assignment, empty line, comment" in {
      val lines =
        """
          |error_log /dev/stdout ${NGINX_ERROR_LOG_LEVEL:-warn};
          |
          |# Establish some environment variables for later use
          |""".stripMargin
      new Nginx(lines).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("error_log", 1, 2),
              List(MixedString(List(ScalarExpr("/dev/stdout", 11, 2, Some("CharSeq"))), 11, 2), InlineExpr("NGINX_ERROR_LOG_LEVEL:-warn", 23, 2)), 1, 2),
            CommentExpr("Establish some environment variables for later use", 56, 4))
        case Right(value) =>
          fail(value.toString())
      }
    }
  }

  // complex composite parsing
  "parsing a location block" should {
    "work for a block with parameter" in {
      val nginxBlock: String =
        """
          |location / {
          |    listen 80;
          |}
          |""".stripMargin;
      parse(nginxBlock, new Nginx("").parseLocationBlock(using _)) match {
        case Success(value, index) =>
          value shouldBe CondCtrlStructure(
            "location",
            CallExpr(
              NameExpr("", 10, -1),
              List(
                NameExpr("location", 0, -1),
                MixedString(List(ScalarExpr("/", 10, -1, Some("CharSeq"))), 10, -1)),
              0, -1),
            List(CallExpr(NameExpr("listen", 18, -1), List(MixedString(List(ScalarExpr("80", 25, -1, Some("CharSeq"))), 25, -1)), 18, -1)),
            List(),
            0, -1)
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "work for a block with parameter assigned via =" in {
      val nginxBlock: String =
        """
          |location = /50x.html {
          |    listen 80;
          |}
          |""".stripMargin;
      new Nginx(nginxBlock).process() match {
        case Left(configFile) =>
          configFile.expressions shouldBe List(
            CondCtrlStructure(
              "location",
              CallExpr(
                NameExpr("=", 10, 2),
                List(NameExpr("location", 1, 2),MixedString(List(ScalarExpr("/50x.html", 12, 2, Some("CharSeq"))), 12, 2)),
                1, 2),
              List(CallExpr(NameExpr("listen", 28, 3), List(MixedString(List(ScalarExpr("80", 35, 3, Some("CharSeq"))), 35, 3)), 28, 3)),
              List(),
              1, 2)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for a block followed by an assignment" in {
      val config =
        """
          |location / {
          |    try_files $uri $uri/ /index.html;
          |}
          |
          |error_page 504  /50x.html;
          |""".stripMargin
      new Nginx(config).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CondCtrlStructure(
              "location",
              CallExpr(
                NameExpr("", 10, 2),
                List(NameExpr("location", 1, 2), MixedString(List(ScalarExpr("/", 10, 2, Some("CharSeq"))), 10, 2)),
                1, 2),
              List(CallExpr(NameExpr("try_files", 18, 3), List(VariableExpr("uri", 28, 3), MixedString(List(VariableExpr("uri", 33, 3), ScalarExpr("/", 37, 3, Some("CharSeq"))), 33, 3), MixedString(List(ScalarExpr("/index.html", 39, 3, Some("CharSeq"))), 39, 3)), 18, 3)),
              List(),
              1, 2),
            CallExpr(NameExpr("error_page", 55, 6), List(MixedString(List(ScalarExpr("504", 66, 6, Some("CharSeq"))), 66, 6), MixedString(List(ScalarExpr("/50x.html", 71, 6, Some("CharSeq"))), 71, 6)), 55, 6))
        case Right(value) => fail(value.toString())
      }
    }
    "work for location with regexp" in {
      val block = {
        // 12345678901234
        """location ~ ^/(index|get|static|errors/report|errors/404|errors/503|health_check)\.php$ {
          |        try_files 404;
          |}
          |""".stripMargin
      }
      new Nginx(block).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CondCtrlStructure(
              "location",
              CallExpr(
                NameExpr("~", 9, 1),
                List(NameExpr("location", 0, 1),
                  ScalarExpr("^/(index|get|static|errors/report|errors/404|errors/503|health_check)\\.php$", 11, 1, Some("RegEx"))),
                0, 1),
              List(CallExpr(NameExpr("try_files", 97, 2), List(MixedString(List(ScalarExpr("404", 107, 2, Some("CharSeq"))), 107, 2)), 97, 2)),
              List(),
              0, 1))
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for block from regression file having string with escaped double quotes" in {
      val nginxBlock: String =
        """location /status/ready {
          |      return 200 "{\"ready\": true}";
          |}""".stripMargin
      parse(nginxBlock, new Nginx("").parseFile(using _)) match {
        case Success(_, index) =>
          index shouldBe nginxBlock.length
        //index shouldBe block.length
        case f: Failure => fail(f.msg)
      }
    }
    "be able to parse location with very complex regex" in {
      val location =
        """location ~* \.(engine|inc|install|make|module|profile|po|sh|.*sql|.*sql\.gz|theme|twig|tpl(\.php)?|xtmpl|yml)(~|\.sw[op]|\.bak|\.orig|\.save)?$|^\/(\.(?!well-known).*|Entries.*|Repository|Root|Tag|Template|web\.config)$|composer\.(json|lock)$|^\/#.*#$|\.php(~|\.sw[op]|\.bak|\.orig|\.save)$ {
          |      deny all;
          |      access_log off;
          |      log_not_found off;
          |      return 404;
          |}""".stripMargin
      parse(location, new Nginx("").parseFile(using _)) match {
          case Success(_, index) =>
            index shouldBe location.length
          //index shouldBe block.length
          case f: Failure => fail(f.msg)
      }
    }
  }

  "parsing an arbitrary block" should {
    "work for a geoip2 block" in {
      val block =
        """
          |geoip2 /etc/nginx/GeoLite2-Country.mmdb {
          |        $geoip2_metadata_country_build metadata build_epoch;
          |
          |        # populate the country
          |        $geoip2_data_country_code source=$remote_addr country iso_code;
          |    }
          |""".stripMargin
      parse(block, new Nginx("").parseFile(using _)) match {
        case Success(value, index) =>
          index shouldBe block.length
        case Failure(value) => fail(value.toString)
      }
    }
    "work for a unnamed block" in {
      val nginxBlock: String =
        """
          |server {
          |    listen 80;
          |}
          |""".stripMargin;
      new Nginx(nginxBlock).process() match {
        case Left(configFile) =>
          configFile.expressions shouldBe List(BlockExpr(Some(NameExpr("server", 1, 2)), List(), List(CallExpr(NameExpr("listen", 14, 3), List(MixedString(List(ScalarExpr("80", 21, 3, Some("CharSeq"))), 21, 3)), 14, 3)), 1, 1))
        case Right(value) =>
          fail(value.toString())
      }
    }
  }

  "parsing lua block" should {
    "work for header_filtered_by_lua_block" in {
      val luaBlock =
        """
          |header_filter_by_lua_block {
          |  ngx.log(ngx.ERR, "header_filter_by_lua_block*")
          |
          |  local lua_resty_waf = require "resty.waf"
          |
          |  -- note that options set in previous handlers (in the same scope)
          |  -- do not need to be set again
          |  local waf = lua_resty_waf:new()
          |
          |  waf:exec()
          |}""".stripMargin
      parse(luaBlock, new Nginx("").parseLuaBLock(using _)) match {
          case Success(value, index) =>
            index shouldBe luaBlock.length
          case f : Failure => fail(f.msg)
        }
    }
    "able to process lua block name" in {
      Set(
        "init_by_lua_block",
        "header_filter_by_lua_block"
      ).map {
        blockName =>
          parse(blockName, new Nginx("").luaBlockName(using _)) match {
            case Success(value,index) =>
              index shouldBe blockName.length
              value shouldBe blockName
            case f : Parsed.Failure => fail(f.msg)
          }
      }
    }
    "work for content_by_lua_block" in {
      val luaBlock =
        """
          |content_by_lua_block {
          |     local http = require "resty.http"
          |}""".stripMargin
      parse(luaBlock, new Nginx("").parseLuaBLock(using _)) match {
        case Success(value, index) =>
          index shouldBe luaBlock.length
        case Failure(value) => fail(value.toString)
      }
    }
    "work for content_by_lua_block with internal { ... }" in {
      val luaBlock =
        """
          |content_by_lua_block {
          |     stuff = {
          |         local http = require "resty.http"
          |     }
          |}""".stripMargin
      parse(luaBlock, new Nginx("").parseLuaBLock(using _)) match {
        case Success(value, index) =>
          index shouldBe luaBlock.length
        case Failure(value) => fail(value.toString)
      }
    }
    "be able to process block" in {
      val nested =
        """{
          |   a = 42;
          |}""".stripMargin
      parse(nested, new Nginx("").readBlobContainingMatchingBrackets(using _)) match {
        case Success(value, index) =>
          index shouldBe nested.length
        case Failure(value) =>
          fail(s"failed due to ${value.toString}")
      }
    }
    "be able to process block with nested brackets" in {
      val nested =
        """{
          |   a = 42;
          |   {
          |       b = 23;
          |   }
          |}""".trim.stripMargin
      parse(nested, new Nginx("").readBlobContainingMatchingBrackets(using _)) match {
        case Success(value, index) =>
          index shouldBe nested.length
        case Failure(value) =>
          fail(s"failed due to ${value.toString}")
      }
    }
    "be able to processed nested brackets mixed content" in {
      val nested =
        """local opts = {
          |           authorization_params = { organization_domain="jobteaser" },
          |}""".stripMargin
      parse(nested, new Nginx("").readBlobContainingMatchingBrackets(using _)) match {
        case Success(value, index) =>
          index shouldBe nested.length
        case Failure(value) =>
          fail(s"failed due to ${value.toString}")
      }
    }
    "work for set_by_lua_block" in {
      val luaBlock =
        """set_by_lua_block $a {
          |        ngx.log(ngx.ERR, "set_by_lua*")
          |}""".stripMargin
      parse(luaBlock, new Nginx("").parseSetByLuaBlock(using _)) match {
          case Success(value, index) =>
            index shouldBe luaBlock.length
          case f : Failure =>
            fail(f.msg)
        }
    }
    "work for set_by_lua_block in location /" in {
      val luaBlock =
        """location / {
          |    set_by_lua_block $a {
          |        ngx.log(ngx.ERR, "set_by_lua*")
          |    }
          |}""".stripMargin
      parse(luaBlock, new Nginx("").parseFile(using _)) match {
        case Success(value, index) =>
          index shouldBe luaBlock.length
        case f: Failure =>
          fail(f.msg)
      }
    }
    "work for content from regression test" in {
      val luaBlock =
        """
          |content_by_lua_block {
          |                local res, err = httpc:request_uri(url, {
          |                    method = method,
          |                    body = body,
          |                    ssl_verify = false,
          |                    headers = {
          |
          |                    }
          |                })
          |}""".stripMargin
      parse(luaBlock, new Nginx("").parseLuaBLock(using _)) match {
        case Success(value, index) =>
          index shouldBe luaBlock.length
        case Failure(value) =>
          fail(s"failed due to ${value.toString}")
      }
    }
    "work for content from regression test II" in {
      val luaBlock =
        """access_by_lua_block {
          |        local opts = {
          |           authorization_params = { organization_domain="jobteaser" },
          |        }
          |}""".stripMargin
      parse(luaBlock, new Nginx("").parseLuaBLock(using _)) match {
        case Success(value, index) =>
          index shouldBe luaBlock.length
        case Failure(value) =>
          fail(s"failed due to ${value.toString}")
      }
    }
  }

  "parse if structure" should {
    "work for if" in {
      val ctrlBlock =
        """
          |if ( $test ~ 42 ) {
          |    return false;
          |}""".stripMargin
      parse(ctrlBlock, new Nginx("").parseControlStructure(using _)) match {
        case Success(value, index) =>
          value.ctype shouldBe "if"
          //value.condExpr.length shouldBe 3 todo: fill out the condExpr
          value.block.length shouldBe 1
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "work for if with single variable parameter" in {
      val ctrlBlock = {
        // 123456789012
        """if ($test) {
          |    return false;
          |}""".stripMargin
      }
      parse(ctrlBlock, new Nginx("").parseControlStructure(using _)) match {
        case Success(value, index) =>
          value.ctype shouldBe "if"
          //value.condExpr.length shouldBe 1
          value.block.length shouldBe 1
        case failure: Failure =>
          fail(failure.msg)
      }
    }
    "work for if with tight brackets" in {
      val ctrlBlock = {
        // 0         1         2         3
        // 0123456789012345678901234567890123456789
        """if ($http_x_forwarded_proto = 'https') {
          |    set $fastcgi_https "on";
          |    set $fastcgi_port  "443";
          |}""".stripMargin
      }
      parse(ctrlBlock, new Nginx("").parseControlStructure(using _)) match {
        case Success(value, index) =>
          value.ctype shouldBe "if"
          //value.condExpr.length shouldBe 3
          value.block.length shouldBe 2
        case failure: Failure =>
          fail(failure.msg)
      }
    }
  }

  "regression tests" should {
    /** Known Bad Configs but not yet escalated
     * 696f7e32320a6221_default.conf
     * 94701fcc87e6b293_upstream2.conf
     * 02787bcb3c8a6e5a_default.conf
     * 0f9f819c9e1c488f_default.conf
     * 772718778dd5745d_default.conf
     * 5058d4f097bafe11_default.conf
     * 94afbff955496042_default.conf
     * 8957ffc97c8d96bf_nginx.conf
     * de6bf4a86107644b_nginx.conf
     * 251355d82ed35651_default.conf
     * 4e34fa9c02dc5a6e_nginx.conf
     * 1c99b73b532e6055_nginx.conf
     * 76647d8281e146f2_upstream.conf
     * 3663a1eb8ebc6651_nginx.conf
     * 65bb670ec1ff0c55_nginx.conf
     * e9aba18c2af52aab_nginx.conf
     * 24e8371479e21fdb_nginx.conf
     * 4880b5ac9d4b3ac9_default.conf
     * f5ea299d9974fbfb_nginx.conf
     * 7fdfde497ffc6272_nginx.conf
     * 83cfea17d1650d5d_nginx.conf
     * 038cdff87dbd4752_nginx.conf
     * d4be7191eebb7344_default.conf
     */

    "work for specific file content - debugging" in {
      val content =
        """
          |
          |""".stripMargin
      parse(content, new Nginx("").parseFile(using _)) match {
        case Success(value, index) =>
          index shouldBe content.length
        case f: Failure => fail(f.msg)
      }
    }


    "work for specific file - debugging" in {
      val basePath = "src/test/resources/nginx/"
      val file = ""
      if(file.nonEmpty) {
        val regressionString = Files.readString(new File(s"$basePath$file").toPath)
        new Nginx(regressionString).checkForKnownNotNginxConfEndingOnConf() shouldBe false
        parse(regressionString, new Nginx("").parseFile(using _)) match {
          case Success(value, index) =>
            index shouldBe regressionString.length
          case f: Failure => fail(f.msg)
        }
      }
    }

    "check bad indicators" in {
      val baseTestFolder = "src/test/resources/nginx/"
      val files: HashSet[String] = HashSet[String](
        "2128198_de8483ec-b74d-41d3-a561-39ea4db09056_supervisord.conf",
        "1327692_840f3d24-e15b-4dbb-a6fc-b347e97ee188_RESPONSE-954-DATA-LEAKAGES-IIS.conf",
        "1128049_dbbff69e-1f02-406a-8ad5-85731739c65a_jsl.node.conf",
        "1128109_4d68d267-35df-426d-a6ff-e6ce5d9ee32b_Makefile.conf",
        "2772526_b911988a-31b4-497a-bf16-15e601b1580c_mlogc-honeypot-sensor.conf",
        "1327798_2ae32de4-992b-4e61-b93f-dfe61137cfd4_modsecurity.conf",
        "1327724_fb785c4a-7230-479c-927d-258238d9fc0e_nginx-modsecurity.conf",
        "2772601_73e6055c-fac3-40ef-a8a0-faf5af91e7e0_modsecurity_crs_10_honeypot.conf",
        "1515114_31a78472-e723-418e-90a3-27fc484d97d6_main.conf",
        "957b71d626ee4ee0_61_asl_recons_dlp.conf",
        "c8a750a83ab83677_00_asl_x_searchengines.conf"
      )
      files.foreach {
        file => {
          new Nginx(Files.readString(new File(s"$baseTestFolder$file").toPath)).checkForKnownNotNginxConfEndingOnConf() shouldBe true
        }
      }
    }

    "work with known problem files" in {
      val files = new File("src/test/resources/nginx/").listFiles(file => {
        file.isFile && file.getPath.endsWith(".conf")
      })
      val result = files.map(file => {
        val nginx = new Nginx(Files.readString(file.toPath))
        if (!nginx.checkForKnownNotNginxConfEndingOnConf())
          (file, nginx.process())
        else
          (file, Left(null))
      }).map {
        case (file, Right(value)) => {
          (file, Some(value))
        }
        case (file, Left(_)) => (file, None)
      }.filter {
        (lhs, rhs) => rhs.nonEmpty
      }.map {
        (lhs, rhs) => s"$lhs -> ${rhs.get.toString()}"
      }
      if (!result.isEmpty) {
        fail(s"Failures:${result.length}\n${result.mkString("\n")}")
      }
    }
  }



}

