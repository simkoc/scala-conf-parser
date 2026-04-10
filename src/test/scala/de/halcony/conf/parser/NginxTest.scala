package de.halcony.conf.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse.*
import fastparse.Parsed.{Failure, Success}

import java.io.File
import java.nio.file.Files

class NginxTest extends AnyWordSpec with Matchers {


  "parsing an nginx configuration file single lines" should {
    "work for single comment" in {
      val conf = "# nginx.vh.default.conf  --  docker-openresty"
      new Nginx(conf).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("nginx.vh.default.conf  --  docker-openresty",0,1)
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
            CommentExpr("",0,1)
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
            CommentExpr("nginx.vh.default.conf  --  docker-openresty",0, 1),
            CommentExpr("",46, 2)
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
    "process scalar" in {
      val scalar = "www-data"
      parse(scalar, new Nginx("").parseScalar(using _)) match {
        case Parsed.Success(value, index) =>
          value.value shouldBe "www-data"
          index shouldBe scalar.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "process scalar list" in {
      val scalarList = "www-data www-other"
      parse(scalarList, new Nginx("").parseArgumentList(using _)) match {
        case Parsed.Success(value, index) =>
          value.length shouldBe 2
          index shouldBe scalarList.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
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
    "process for single assignment as call" in {
      val conf: String =
        """
          |user www-data;
          |""".stripMargin
      new Nginx(conf).process() match {
        case Left(file) =>
          file.expressions shouldBe List(
            CallExpr(NameExpr("user", 1, 2), List(ScalarExpr("www-data", 6, 2)), 1, 2),
          )
        case Right(_) =>
          fail("unable to parse file")
      }
    }
    "process list assignment as call" in {
      val conf: String =
        """
          |error_page  500 502  /50x.html;
          |""".stripMargin;
      new Nginx(conf).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("error_page", 1, 2),
              List(ScalarExpr("500", 13, 2), ScalarExpr("502", 17, 2), ScalarExpr("/50x.html", 22, 2)),
              1,
              2
            )
          )
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
            CallExpr(
              NameExpr("error_log",0, 1),
              List(
                ScalarExpr("/dev/stdout",10,1),
                InlineExpr("NGINX_ERROR_LOG_LEVEL:-warn",22,1)
              ),
              0,
              1
            )
          )
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
              NameExpr("log_format",1, 2),
              List(
                ScalarExpr("main",12,2),
                ScalarExpr("$remote_addr - $remote_user [$time_local] \"$request\" ",17,2),
                ScalarExpr("$status $body_byte_sent \"$http_referer\" ",89, 3)
              ),
              1, 2
            )
          )
        case Right(value) =>
          fail(value.toString())
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
              NameExpr("error_log",1, 2),
              List(
                ScalarExpr("/dev/stdout",11, 2),
                InlineExpr("NGINX_ERROR_LOG_LEVEL:-warn",23, 2)
              ),
              1,2
            ),
            CommentExpr("Establish some environment variables for later use",56, 4)
            )
        case Right(value) =>
          fail(value.toString())
      }
    }
  }

  "parsing an nginx block" should {
    "work for a geoip2 block" in {
      val block =
        """
          |geoip2 /etc/nginx/GeoLite2-Country.mmdb {
          |        auto_reload 1h;
          |
          |        $geoip2_metadata_country_build metadata build_epoch;
          |
          |        # populate the country
          |        $geoip2_data_country_code source=$remote_addr country iso_code;
          |        $geoip2_data_country_name source=$remote_addr country names en;
          |
          |        # populate the continent
          |        $geoip2_data_continent_code source=$remote_addr continent code;
          |        $geoip2_data_continent_name source=$remote_addr continent names en;
          |    }
          |""".stripMargin
      parse(block, new Nginx("").parseFile(using _)) match {
          case Success(value, index) =>
            index shouldBe block.length
          case Failure(value) => fail(value.toString)
        }
    }
    "work for a unnamed block" in {
      val nginxBlock : String =
        """
          |server {
          |    listen 80;
          |}
          |""".stripMargin;
      new Nginx(nginxBlock).process() match {
        case Left(configFile) =>
          configFile.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("server",1, 2)),
              List(),
              List(
                CallExpr(
                  NameExpr("listen", 14, 3),
                  List(ScalarExpr("80", 21, 3)),
                  14, 3
                )),
              1,
              1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for a block with parameter" in {
      val nginxBlock: String =
        """
          |location / {
          |    listen 80;
          |}
          |""".stripMargin;
      new Nginx(nginxBlock).process() match {
        case Left(configFile) =>
          configFile.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location",1, 2)),
              List(ScalarExpr("/",10,2)),
              List(
                CallExpr(
                  NameExpr("listen", 18, 3),
                  List(ScalarExpr("80", 25, 3)),
                  18, 3
                )),
              1,
              1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for a block with multiple parameters" in {
      val block =
        """
          |location $first $second third {
          |    error_log 42;
          |}
          |""".stripMargin
      new Nginx(block).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location",1,2)),
              List(
                VariableExpr("first",10,2),
                VariableExpr("second",17,2),
                ScalarExpr("third",25,2)
              ),
              List(
                CallExpr(
                  NameExpr("error_log",37,3),
                  List(
                    ScalarExpr("42",47,3)
                  ),
                  37,
                  3
              )
            ),
              1,
              1
          ))
        case Right(value) =>
          fail(value.toString())
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
            BlockExpr(
              Some(NameExpr("location", 1, 2)),
              List(ScalarExpr("/50x.html", 12, 2)),
              List(
                CallExpr(
                  NameExpr("listen", 28, 3),
                  List(ScalarExpr("80", 35, 3)),
                  28, 3
                )),
              1,
              1
            )
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
            BlockExpr(
              Some(NameExpr("location",1,2)),
              List(ScalarExpr("/",10,2)),
              List(
                CallExpr(
                  NameExpr("try_files",18,3),
                  List(
                    VariableExpr("uri",28,3),
                    VariableExpr("uri/",33,3),
                    ScalarExpr("/index.html",39,3),
                  ),
                  18,
                  3
                )
              ),
              1,
              1
            ),
            CallExpr(
              NameExpr("error_page",55,6),
              List(
                ScalarExpr("504",66,6),
                ScalarExpr("/50x.html",71,6)
              ),
              55,
              6
            )
          )
        case Right(value) => fail(value.toString())
      }
    }
    "work for a block followed by a comment" in {
      val block =
        """
          |location = /50x.html {
          |        root   /usr/local/openresty/nginx/html;
          |}
          |
          |# proxy the PHP scripts to Apache listening on 127.0.0.1:80
          |""".stripMargin
      new Nginx(block).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location",1,2)),
              List(ScalarExpr("/50x.html",12,2)),
              List(
                CallExpr(NameExpr("root",32,3),
                  List(ScalarExpr("/usr/local/openresty/nginx/html",39,3)),
                  32,3
              )),
              1,1
            ),
            CommentExpr("proxy the PHP scripts to Apache listening on 127.0.0.1:80",
              75,6)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for location with regexp" in {
      val block =
        """
          |location ~ ^/(index|get|static|errors/report|errors/404|errors/503|health_check)\.php$ {
          |        try_files 404;
          |}
          |""".stripMargin
      new Nginx(block).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location",1,2)),
              List(
                ScalarExpr("~",10,2),
                ScalarExpr("^/(index|get|static|errors/report|errors/404|errors/503|health_check)\\.php$",12,2)
              ),
              List(
                CallExpr(
                  NameExpr("try_files",98,3),
                  List(ScalarExpr("404",108,3)),
                  98,3)
              ),
              1,1
            )
          )
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

  "work with known problem files" in {
    val files = new File("src/test/resources/nginx/").listFiles(file => {
      file.isFile && file.getPath.endsWith(".conf")
    })
    val result = files.map(file => {
      (file, new Nginx(Files.readString(file.toPath)).process())
    }).map {
      case (file, Right(value)) => (file, Some(value))
      case (file, Left(_)) => (file,None)
    }.filter {
      (lhs, rhs) => rhs.nonEmpty
    }.map{
      (lhs,rhs) => s"$lhs -> ${rhs.get.toString()}"
    }
    if(!result.isEmpty) {
      fail(result.mkString("\n"))
    }
  }



}

