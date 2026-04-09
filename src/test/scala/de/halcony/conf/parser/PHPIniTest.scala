package de.halcony.conf.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse.*

import java.io.File
import java.nio.file.Files

class PHPIniTest extends AnyWordSpec with Matchers {

  "parsing a php.ini configuration file" should {
    "work for single comment" in {
      val conf = "; This is a comment"
      new PHPIni(conf).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("This is a comment",0,1)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for empty comment" in {
      val comment = ";"
      new PHPIni(comment).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("",0,1)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for section header" in {
      val section = "[PHP]"
      new PHPIni(section).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("PHP",0,1)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for simple directive" in {
      val directive = "engine = On"
      new PHPIni(directive).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("=",7,1),
              List(
                NameExpr("engine",0,1),
                ScalarExpr("On",9,1)
              ),
              0,
              1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for directive with string value" in {
      val directive = "user_ini.filename = \".user.ini\""
      new PHPIni(directive).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("=",18,1),
              List(
                NameExpr("user_ini.filename",0,1),
                ScalarExpr("\".user.ini\"",20,1)
              ),
              0,
              1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for directive with numeric value" in {
      val directive = "max_execution_time = 30"
      new PHPIni(directive).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("=",19,1),
              List(
                NameExpr("max_execution_time",0,1),
                ScalarExpr("30",21,1)
              ),
              0,
              1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for directive with boolean value" in {
      val directive = "display_errors = Off"
      new PHPIni(directive).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("=",15,1),
              List(
                NameExpr("display_errors",0,1),
                ScalarExpr("Off",17,1)
              ),
              0,
              1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for mixed content" in {
      val mixedContent = """
        |; PHP configuration file
        |[PHP]
        |engine = On
        |display_errors = Off
        |; End of configuration
        |""".stripMargin

      new PHPIni(mixedContent).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CommentExpr("PHP configuration file",1,2),
            CommentExpr("PHP",26,3),
            CallExpr(
              NameExpr("=",39,4),
              List(
                NameExpr("engine",32,4),
                ScalarExpr("On",41,4)
              ),
              32,
              4
            ),
            CallExpr(
              NameExpr("=",59,5),
              List(
                NameExpr("display_errors",44,5),
                ScalarExpr("Off",61,5)
              ),
              44,
              5
            ),
            CommentExpr("End of configuration",65,6)
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for bitwise operators in values" in {
      val bitwiseContent = "error_reporting = E_ALL & ~E_DEPRECATED & ~E_STRICT"
      new PHPIni(bitwiseContent).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("=",16,1),
              List(
                NameExpr("error_reporting",0,1),
                ScalarExpr("E_ALL & ~E_DEPRECATED & ~E_STRICT",18,1)
              ),
              0,
              1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }

    "work for command-line style arguments" in {
      val commandLineContent = "sendmail_path = /usr/sbin/sendmail -t -i"
      new PHPIni(commandLineContent).process() match {
        case Left(value) =>
          value.expressions shouldBe List(
            CallExpr(
              NameExpr("=",14,1),
              List(
                NameExpr("sendmail_path",0,1),
                ScalarExpr("/usr/sbin/sendmail -t -i",16,1)
              ),
              0,
              1
            )
          )
        case Right(value) =>
          fail(value.toString())
      }
    }
  }

  "work with real php.ini file" in {
    val file = new File("src/test/resources/php/php.ini")
    new PHPIni(Files.readString(file.toPath)).process() match {
      case Left(value) =>
        // Just check that it parses successfully
        value should not be null
      case Right(value) =>
        fail(s"file ${file.getPath} was not successfully parsed: $value")
    }
  }

  "work with real world php.ini files" in {
    val files = new File("src/test/resources/php/").listFiles(file => {
      file.isFile && file.getPath.endsWith(".ini")
    })
    files.foreach(file => {
      new PHPIni(Files.readString(file.toPath)).process() match {
        case Left(value) =>
          // Just check that it parses successfully
          value should not be null
        case Right(value) =>
          fail(s"file ${file.getPath} was not successfully parsed: $value")
      }
    })
  }
}