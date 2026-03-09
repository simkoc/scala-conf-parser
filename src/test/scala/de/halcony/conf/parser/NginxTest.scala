package de.halcony.conf.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse.*
import fastparse.Parsed.Failure.unapply

import java.io.File
import java.nio.file.Files

class NginxTest extends AnyWordSpec with Matchers {


  "parsing an nginx configuration file single lines" should {
    "work for single comment" in {
      val conf = "# nginx.vh.default.conf  --  docker-openresty"
      Nginx.process(conf) match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("nginx.vh.default.conf  --  docker-openresty",0,-1)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "work for empty comment" in {
      val comment = "# "
      Nginx.process(comment) match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("",0,-1)
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
      Nginx.process(comments.trim) match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("nginx.vh.default.conf  --  docker-openresty",0,-1),
            CommentExpr("",46,-1)
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
      Nginx.process(conf) match {
        case Left(file) =>
          file.expressions shouldBe List(
            CommentExpr("comment one", 1, -1),
            CommentExpr("comment two", 17, -1)
          )
        case Right(_) =>
          fail("unable to parse file")
      }
    }
    "process inline expression" in {
      val inlineExpr = "${NGINX_ERROR_LOG_LEVEL:-warn}"
      parse(inlineExpr, Nginx.parseInlineExpression(using _)) match {
        case Parsed.Success(value, index) =>
          value.value shouldBe "NGINX_ERROR_LOG_LEVEL:-warn"
          index shouldBe inlineExpr.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "process scalar" in {
      val scalar = "www-data"
      parse(scalar, Nginx.parseScalar(using _)) match {
        case Parsed.Success(value, index) =>
          value.value shouldBe "www-data"
          index shouldBe scalar.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "process scalar list" in {
      val scalarList = "www-data www-other"
      parse(scalarList, Nginx.parseArgumentList(using _)) match {
        case Parsed.Success(value, index) =>
          value.length shouldBe 2
          index shouldBe scalarList.length
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }
    "process name" in {
      val name = "user"
      parse(name, Nginx.parseName(using _)) match {
        case Parsed.Success(value, index) =>
          value.value shouldBe "user"
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
      Nginx.process(conf) match {
        case Left(file) =>
          file.expressions shouldBe List(
            CallExpr(NameExpr("user", 1, -1), List(ScalarExpr("www-data", 6, -1)), 1, -1),
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
      Nginx.process(conf) match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("error_page", 1, -1),
              List(ScalarExpr("500", 13, -1), ScalarExpr("502", 17, -1), ScalarExpr("/50x.html", 22, -1)),
              1,
              -1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
    "process inline expression assignment" in {
      val inlineExpr =
        """error_log /dev/stdout ${NGINX_ERROR_LOG_LEVEL:-warn};""".stripMargin
      Nginx.process(inlineExpr.trim) match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("error_log",0,-1),
              List(
                ScalarExpr("/dev/stdout",10,-1),
                InlineExpr("NGINX_ERROR_LOG_LEVEL:-warn",22,-1)
              ),
              0,
              -1
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
      Nginx.process(callWithStrings) match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("log_format",1, -1),
              List(
                ScalarExpr("main",12,-1),
                ScalarExpr("$remote_addr - $remote_user [$time_local] \"$request\" ",17,-1),
                ScalarExpr("$status $body_byte_sent \"$http_referer\" ",89,-1)
              ),
              1, -1
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
      Nginx.process(lines) match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("error_log",1,-1),
              List(
                ScalarExpr("/dev/stdout",11,-1),
                InlineExpr("NGINX_ERROR_LOG_LEVEL:-warn",23,-1)
              ),
              1,-1
            ),
            CommentExpr("Establish some environment variables for later use",56,-1)
            )
        case Right(value) =>
          fail(value.toString())
      }
    }
  }

  "parsing an nginx block" should {
    "work for a unnamed block" in {
      val nginxBlock : String =
        """
          |server {
          |    listen 80;
          |}
          |""".stripMargin;
      Nginx.process(nginxBlock) match {
        case Left(configFile) =>
          configFile.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("server",1, -1)),
              List(),
              List(
                CallExpr(
                  NameExpr("listen", 14, -1),
                  List(ScalarExpr("80", 21, -1)),
                  14, -1
                )),
              1,
              -1
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
      Nginx.process(nginxBlock) match {
        case Left(configFile) =>
          configFile.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location",1, -1)),
              List(ScalarExpr("/",10,-1)),
              List(
                CallExpr(
                  NameExpr("listen", 18, -1),
                  List(ScalarExpr("80", 25, -1)),
                  18, -1
                )),
              1,
              -1
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
      Nginx.process(block) match {
        case Left(value) =>
          value.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location",1,-1)),
              List(
                VariableExpr("first",10,-1),
                VariableExpr("second",17,-1),
                ScalarExpr("third",25,-1)
              ),
              List(
                CallExpr(
                  NameExpr("error_log",37,-1),
                  List(
                    ScalarExpr("42",47,-1)
                  ),
                  37,
                  -1
              )
            ),
              1,
              -1
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
      Nginx.process(nginxBlock) match {
        case Left(configFile) =>
          configFile.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location", 1, -1)),
              List(ScalarExpr("/50x.html", 12, -1)),
              List(
                CallExpr(
                  NameExpr("listen", 28, -1),
                  List(ScalarExpr("80", 35, -1)),
                  28, -1
                )),
              1,
              -1
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
      Nginx.process(config) match {
        case Left(value) =>
          value.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location",1,-1)),
              List(ScalarExpr("/",10,-1)),
              List(
                CallExpr(
                  NameExpr("try_files",18,-1),
                  List(
                    VariableExpr("uri",28,-1),
                    VariableExpr("uri/",33,-1),
                    ScalarExpr("/index.html",39,-1),
                  ),
                  18,
                  -1
                )
              ),
              1,
              -1
            ),
            CallExpr(
              NameExpr("error_page",55,-1),
              List(
                ScalarExpr("504",66,-1),
                ScalarExpr("/50x.html",71,-1)
              ),
              55,
              -1
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
      Nginx.process(block) match {
        case Left(value) =>
          value.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("location",1,-1)),
              List(ScalarExpr("/50x.html",12,-1)),
              List(
                CallExpr(NameExpr("root",32,-1),
                  List(ScalarExpr("/usr/local/openresty/nginx/html",39,-1)),
                  32,-1
              )),
              1,-1
            ),
            CommentExpr("proxy the PHP scripts to Apache listening on 127.0.0.1:80",
              75,-1)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
  }

  "work with known problem files" in {
    val files = new File("src/test/resources/nginx/").listFiles(file => {
      file.isFile && file.getPath.endsWith(".conf")
    })
    files.foreach(file => {
      Nginx.process(Files.readString(file.toPath)) match {
        case Left(value) =>
        case Right(value) => fail(s"file ${file.getPath} was not successfully parsed: $value")
      }
    })
  }



}

