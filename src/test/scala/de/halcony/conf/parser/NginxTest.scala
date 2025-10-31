package de.halcony.conf.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import fastparse.*

class NginxTest extends AnyWordSpec with Matchers {


  "parsing an nginx configuration file" should {
    "work for single comment" in {
      val conf = "# comment"
      parse(conf, Nginx.parseComment(using _)) match {
        case Parsed.Success(value, index) =>
          value.value shouldBe "comment"
        case failure: Parsed.Failure =>
          fail(failure.toString())
      }
    }

    "work for multiple comments" in {
      val conf : String =
        """
          |# comment one
          |  # comment two
          |""".stripMargin
      Nginx.process(conf) match {
        case Some(file) =>
          file.expressions shouldBe List(
            CommentExpr("comment one",1,-1),
            CommentExpr("comment two",17,-1)
          )
        case None =>
          fail("unable to parse file")
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
      parse(scalarList, Nginx.parseScalarList(using _)) match {
        case Parsed.Success(value, index) =>
          value.values.length shouldBe 2
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
    "work for single assignment" in {
      val conf : String =
        """
          |user www-data;
          |""".stripMargin
      Nginx.process(conf) match {
        case Some(file) =>
          file.expressions shouldBe List(
            AssignmentExpr(NameExpr("user",1,-1), ScalarExpr("www-data",6,-1),  1,-1),
          )
        case None =>
          fail("unable to parse file")
      }
    }
    "work for single assignment list" in {
      val conf: String =
        """
          |user www-data www-other;
          |""".stripMargin
      Nginx.process(conf) match {
        case Some(file) =>
          file.expressions shouldBe List(
            AssignmentExpr(NameExpr("user", 1, -1),
              ListExpr(
                List(ScalarExpr("www-data", 6, -1),ScalarExpr("www-other",15,-1)),6,-1),
              1,-1),
          )
        case None =>
          fail("unable to parse file")
      }
    }
    "work with named block" in {
      val block =
        """
          |http {
          |    # comment
          |    assign ment;
          |}
          |""".stripMargin
      Nginx.process(block) match {
        case Some(value) =>
          value.expressions shouldBe List(
            BlockExpr(
              Some(NameExpr("http",1,-1)),
              List(
                CommentExpr("comment",12,-1),
                AssignmentExpr(NameExpr("assign",26,-1),ScalarExpr("ment",33,-1),26,-1)
              ),
              1,-1
            )
          )
        case None =>
          fail("unable to parse file")
      }
    }
  }



}

