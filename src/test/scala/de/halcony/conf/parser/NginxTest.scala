package de.halcony.conf.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse.*
import fastparse.Parsed.{Failure, Success}

import java.io.File
import java.nio.file.Files
import scala.collection.immutable.HashSet

class NginxTest extends AnyWordSpec with Matchers {

  "parsing a variable" should {
    "work for $var" in {
      val varExpr = "$var t"
      parse(varExpr,new Nginx(varExpr).parseVariable(using _)) match {
        case Success(value, index) =>
          index shouldBe varExpr.length - 2
          value.name shouldBe "var"
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
          value.name shouldBe "var"
        case f : Failure =>
          fail(f.msg)
      }
    }
    "work for {{var}}" in {
      val varExpr = "{var} t"
      parse(varExpr,new Nginx(varExpr).parseVariable(using _)) match {
        case Success(value, index) =>
          index shouldBe varExpr.length - 2
          value.name shouldBe "var"
        case f : Failure =>
          fail(f.msg)
      }
    }
  }

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
    "wor for regexp string" in {
      val regexpString = " /\\.well-known/security\\.txt(\\.sig)?$"
      parse(regexpString, new Nginx("").parseRegexpString(using _)) match {
        case Parsed.Success(value, index) =>
          index shouldBe regexpString.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "work for simple string" in {
      val string = "include /etc/nginx/conf.d/drupal/server_prepend*.conf;"
      parse(string, new Nginx("").parseCall(using _)) match {
        case Parsed.Success(value, index) =>
          index shouldBe string.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
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
      parse(scalar, new Nginx("").parseString(using _)) match {
        case Parsed.Success(value, index) =>
          value match {
            case expr : ScalarExpr =>
              expr.value shouldBe "www-data"
            //case x => fail(s"wrong return type: ${x.getClass.toString}")
          }
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
    "process string with escaped double quote" in {
      val weirdString =
        """
          |"{\"ready\": true}"""".trim.stripMargin
      parse(weirdString, new Nginx("").parseDoubleQuoteStringWithEscapedDoubleQuote(using _)) match {
        case Parsed.Success(value, index) =>
          index shouldBe weirdString.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
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
    "process DBracked element" in {
      val dbracked = "{{FASTCGI}}"
      parse(dbracked, new Nginx("").parseDbrackedVariable(using _)) match {
        case Parsed.Success(value, index) =>
          index shouldBe dbracked.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
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
      val nginxBlock: String =
        """
          |server {
          |    listen 80;
          |}
          |""".stripMargin;
      new Nginx(nginxBlock).process() match {
        case Left(configFile) =>
          configFile.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("server", 1, 2)),
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
    "work for block from regression file having string with escaped double quotes" in {
      val nginxBlock : String =
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
    "work for encountered location with ~* and regexp" in {
      val block =
        """try_files $uri @drupal;""".stripMargin
      parse(block, new Nginx("").parseCall(using _)) match {
        case Success(_, index) =>
        index shouldBe block.length
        case f: Failure => fail(f.msg)
      }
    }
  }

  /*"conditionals" should {
    "not work so far" in {
      val cond =
        """if ($fastcgi_script_name ~ "^(.+?\.php)(/.+)$") {
          |      set $real_script_name $1;
          |      set $path_info $2;
          |    }
          |""".stripMargin
      parse(cond, new Nginx("").parseFile(using _)) match {
        case Success(_, index) =>
          fail("should not work at the moment")
          //index shouldBe cond.length
        //index shouldBe block.length
        case f: Failure => fail(f.msg)
      }
    }
  }*/

  "parsing variable expressions" should {
    "work if expression has spaces" in {
      val expression = """
       fastcgi_buffers         ${FASTCGI_BUFFERS:-256 32k};""".stripMargin
      parse(expression, new Nginx("").parseFile(using _)) match {
        case Success(value, index) =>
          index shouldBe expression.length
        case f : Failure => fail(f.msg)
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


  "regression tests" should {
    "work for specific file content - debugging" in {
      val content =
        """location /index.html {
          |    add_header X-Frame-Options XFrameOptions;
          |}
          |
          |include /etc/nginx/conf.d/unit/deny.conf;""".stripMargin
      parse(content, new Nginx("").parseFile(using _)) match {
        case Success(value, index) =>
          index shouldBe content.length
        case f: Failure => fail(f.msg)
      }
    }


    "work for specific file - debugging" in {
      val basePath = "src/test/resources/nginx/"
      val file = "944285_f0531ad3-97db-4b68-a524-a69a7ce283c0_spa.conf"
      val regressionString = Files.readString(new File(s"$basePath$file").toPath)
      new Nginx(regressionString).checkForKnownNotNginxConfEndingOnConf() shouldBe false
      if(file.nonEmpty) {
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
        "1515114_31a78472-e723-418e-90a3-27fc484d97d6_main.conf"
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

