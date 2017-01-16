/*
  Copyright 2013-2017 indeni Ltd.
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at
      http://www.apache.org/licenses/LICENSE-2.0
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
 */

package com.indeni.rsql

import org.scalatest.FunSuite

class RsqlParserSuite extends FunSuite {

  val parser = new RsqlParser()
  import parser._
  import RsqlParser._

  test("True token should match a 'true'") {
    val result = parser.parseAll(parser.TrueToken, "true")
    assert(result.successful)
    result match {
      case Success(str, next) => assert(str == "true")
      case _ => fail()
    }
  }

  test("False token should match a 'false'") {
    val result = parser.parseAll(parser.FalseToken, "false")
    assert(result.successful)
    result match {
      case Success(str, next) => assert(str == "false")
      case _ => fail()
    }
  }

  test("Boolean expression should parse a 'true'") {
    val result = parser.parseAll(parser.booleanStr, "true")
    assert(result.successful)
    result match {
      case Success(BooleanExpression(a), next) => assert(a == true)
      case _ => fail()
    }
  }

  test("Boolean expression should parse a 'false'") {
    val result = parser.parseAll(parser.booleanStr, "false")
    assert(result.successful)
    result match {
      case Success(BooleanExpression(a), next) => assert(a == false)
      case _ => fail()
    }
  }

  test("Alpha token should match a single alphabet character") {
    val result = parser.parseAll(parser.Alpha, "a")
    assert(result.successful)
    result match {
      case Success(str, next) => assert(str == "a")
      case _ => fail()
    }
  }

  test("Alpha token should match a single capital alphabet character") {
    val result = parser.parseAll(parser.Alpha, "A")
    assert(result.successful)
    result match {
      case Success(str, next) => assert(str == "A")
      case _ => fail()
    }
  }

  test("Alpha token should not match a more than a single character") {
    val result = parser.parseAll(parser.Alpha, "Ab")
    assert(!result.successful)
  }

  test("opAlt gt") {
    val result = parser.parseAll(parser.opAlt, ">")
    assert(result.successful)
    result match {
      case Success(GtSymbol, _) =>
      case _ => fail()
    }
  }

  test("opAlt gte") {
    val result: parser.ParseResult[RsqlSymbol] = parser.parseAll(parser.opAlt, ">=")
    assert(result.successful)
    result match {
      case Success(GtEqSymbol, _) =>
      case _ => fail()
    }
  }

  test("opAlt lt") {
    val result: parser.ParseResult[RsqlSymbol] = parser.parseAll(parser.opAlt, "<")
    assert(result.successful)
    result match {
      case Success(LtSymbol, _) =>
      case _ => fail()
    }
  }

  test("opAlt lte") {
    val result: parser.ParseResult[RsqlSymbol] = parser.parseAll(parser.opAlt, "<=")
    assert(result.successful)
    result match {
      case Success(LtEqSymbol, _) =>
      case _ => fail()
    }
  }

  test("opFiql EqSymbol") {
    val result = parser.parseAll(parser.opFiql, "==")
    assert(result.successful)
    result match {
      case Success(EqSymbol, _) =>
      case _ => fail()
    }
  }

  test("opFiql InSymbol") {
    val result = parser.parseAll(parser.opFiql, "=in=")
    assert(result.successful)
    result match {
      case Success(InSymbol, _) =>
      case _ => fail()
    }
  }

  test("opFiql OutSymbol") {
    val result = parser.parseAll(parser.opFiql, "=out=")
    assert(result.successful)
    result match {
      case Success(OutSymbol, _) =>
      case _ => fail()
    }
  }

  test("opFiql LtSymbol") {
    val result = parser.parseAll(parser.opFiql, "=lt=")
    assert(result.successful)
    result match {
      case Success(LtSymbol, _) =>
      case _ => fail()
    }
  }

  test("opFiql LtEqSymbol") {
    val result = parser.parseAll(parser.opFiql, "=le=")
    assert(result.successful)
    result match {
      case Success(LtEqSymbol, _) =>
      case _ => fail()
    }
  }

  test("opFiql GtSymbol") {
    val result = parser.parseAll(parser.opFiql, "=gt=")
    assert(result.successful)
    result match {
      case Success(GtSymbol, _) =>
      case _ => fail()
    }
  }

  test("opFiql GtEqSymbol") {
    val result = parser.parseAll(parser.opFiql, "=ge=")
    assert(result.successful)
    result match {
      case Success(GtEqSymbol, _) =>
      case _ => fail()
    }
  }

  test("operator should match either alt of fiql") {
    assert(parser.parseAll(parser.operator, "=ge=").successful)
    assert(parser.parseAll(parser.operator, ">=").successful)
  }

  test("selector should match strings with no reserved tokens") {
    assert(parser.parseAll(parser.selector, "foobar").successful)
    assert(parser.parseAll(parser.selector, "foo.bar").successful)
    assert(parser.parseAll(parser.selector, "foo-bar").successful)
    assert(parser.parseAll(parser.selector, "foo_bar").successful)
    parser.parseAll(parser.selector, "foo_bar") match {
      case Success(ConstExpression(s), next) => assert(s == "foo_bar")
      case _ => fail()
    }

    assert(!parser.parseAll(parser.selector, "foo~bar").successful)
    assert(!parser.parseAll(parser.selector, "foo>bar").successful)
    assert(!parser.parseAll(parser.selector, "foo=bar").successful)
  }

  test("singleQuoted") {
    val result: parser.ParseResult[RsqlSymbol] = parser.parseAll(parser.singleQuoted, "'foo bar'")
    assert(result.successful)
    result match {
      case Success(ConstExpression(s), next) => assert(s == "foo bar")
      case _ => fail()
    }
  }

  test("singleQuoted escaped") {
    val result: parser.ParseResult[RsqlSymbol] = parser.parseAll(parser.singleQuoted, "'foo \\'bar'")
    assert(result.successful)
    result match {
      case Success(ConstExpression(s), next) => assert(s == "foo 'bar")
      case _ => fail()
    }
  }

  test("singleQuoted escaped abuse test") {
    def assertParser(in: String, out: String) = {
      parser.parseAll(parser.input, s"sel==$in") match {
        case Success(ComparisonExpression(s, op, args), next) => assert(args == ConstExpression(out))
        case _ => fail()
      }
    }

    assertParser("'10\\' 15\"'", "10' 15\"")
    assertParser("'10\\' 15\\\"'", "10' 15\"")
    assertParser("'w\\\\ \\'Flyn\\n\\''", "w\\ 'Flynn'")
    assertParser("'\\\\(^_^)/'", "\\(^_^)/")
  }


  test("singleQuoted escaping the quotes") {
    val result: parser.ParseResult[RsqlSymbol] = parser.parseAll(parser.singleQuoted, "'foo \\'bar'")
    assert(result.successful)
    result match {
      case Success(ConstExpression(s), next) => assert(s == "foo 'bar")
      case _ => fail()
    }
  }

  test("doubleQuoted") {
    val result: parser.ParseResult[RsqlSymbol] = parser.parseAll(parser.doubleQuoted, "\"foo bar\"")
    assert(result.successful)
    result match {
      case Success(ConstExpression(s), next) => assert(s == "foo bar")
      case _ => fail()
    }
  }

  test("doubleQuoted escaped abuse test") {
    def assertParser(in: String, out: String) = {
      parser.parseAll(parser.input, s"sel==$in") match {
        case Success(ComparisonExpression(s, op, args), next) => assert(args == ConstExpression(out))
        case e =>
          println(e)
          fail()
      }
    }

    assertParser("\"10' 15\\\"\"",    "10' 15\"")
    assertParser("\"10\\' 15\\\"\"",  "10' 15\"")
    assertParser("\"w\\\\ \\\"Flyn\\n\\\"\"", "w\\ \"Flynn\"")
    assertParser("\"\\\\(^_^)/\"", "\\(^_^)/")
  }

  test("comparison eq") {
    val result = parser.parseAll(parser.comparison, "name==\"Kill Bill\"")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("name"), EqSymbol, ConstExpression("Kill Bill")), _) =>
      case _ => fail()
    }
  }

  test("comparison eq quotes") {
    val result = parser.parseAll(parser.comparison, "name=='Kill Bill'")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("name"), EqSymbol, ConstExpression("Kill Bill")), _) =>
      case _ => fail()
    }
  }

  test("comparison not eq") {
    val result = parser.parseAll(parser.comparison, "name!='Kill Bill'")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("name"), NotEqSymbol, ConstExpression("Kill Bill")), _) =>
      case _ => fail()
    }
  }

  test("comparison gt") {
    val result = parser.parseAll(parser.comparison, "age=gt=20")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("age"), GtSymbol, ConstExpression("20")), _) =>
      case _ => fail()
    }
  }

  test("comparison gt alt") {
    val result = parser.parseAll(parser.comparison, "age > 20")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("age"), GtSymbol, ConstExpression("20")), _) =>
      case _ => fail()
    }
  }

  test("comparison ge") {
    val result = parser.parseAll(parser.comparison, "age=ge=20")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("age"), GtEqSymbol, ConstExpression("20")), _) =>
      case _ => fail()
    }
  }

  test("comparison ge alt") {
    val result = parser.parseAll(parser.comparison, "age >= 20")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("age"), GtEqSymbol, ConstExpression("20")), _) =>
      case _ => fail()
    }
  }

  test("comparison In multiple values") {
    val result = parser.parseAll(parser.comparison, "genres=in=(sci-fi,action)")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("genres"),
      InSymbol,
      ListValuesExpression(
      Seq(ConstExpression("sci-fi"), ConstExpression("action")))),
      _) =>
      case _ => fail()
    }
  }

  test("comparison In single value") {
    val result = parser.parseAll(parser.comparison, "genres=in=(sci-fi)")
    assert(result.successful)
    result match {
      case Success(ComparisonExpression(ConstExpression("genres"),
      InSymbol,
      ListValuesExpression(
      Seq(ConstExpression("sci-fi")))),
      _) =>
      case _ => fail()
    }
  }

  test("simple AND") {
    val result = parser.parseAll(parser.and, "name==\"Kill Bill\";year=gt=2003")
    assert(result.successful)
    result match {
      case Success(r, next) =>
        r match {
          case AndExpression(ComparisonExpression(ConstExpression("name"), EqSymbol, ConstExpression("Kill Bill")),
          ComparisonExpression(ConstExpression("year"), GtSymbol, ConstExpression("2003"))) =>
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("simple AND alt syntax") {
    val result = parser.parseAll(parser.and, "name==\"Kill Bill\" and year>2003")
    assert(result.successful)
    result match {
      case Success(r, next) =>
        r match {
          case AndExpression(ComparisonExpression(ConstExpression("name"), EqSymbol, ConstExpression("Kill Bill")),
          ComparisonExpression(ConstExpression("year"), GtSymbol, ConstExpression("2003"))) =>
          case _ => fail()
        }
      case _ => fail()
    }
  }

  test("simple OR") {
    val result = parser.parseAll(parser.or, "name==\"Kill Bill\",year=gt=2003,resolved==false")
    assert(result.successful)
    result match {
      case Success(r, next) =>
        r match {
          case OrExpression(ComparisonExpression(ConstExpression("name"), EqSymbol, ConstExpression("Kill Bill")),
          ComparisonExpression(ConstExpression("year"), GtSymbol, ConstExpression("2003")),
          ComparisonExpression(ConstExpression("resolved"), EqSymbol, BooleanExpression(false))) =>
          case e =>
            fail(e.toString)
        }
      case _ => fail()
    }
  }

  test("simple OR alt syntax") {
    val result = parser.parseAll(parser.or, "name==\"Kill Bill\" or year>=2003")
    assert(result.successful)
    result match {
      case Success(r, next) =>
        r match {
          case OrExpression(ComparisonExpression(ConstExpression("name"), EqSymbol, ConstExpression("Kill Bill")),
          ComparisonExpression(ConstExpression("year"), GtEqSymbol, ConstExpression("2003"))) =>
          case e =>
            fail(e.toString)
        }
      case _ => fail()
    }
  }

  test("complex query") {
    val result =
      parser.parseAll(parser.or, "genres=in=(sci-fi,action);genres=out=(romance,animated),director==Que*Tarantino")
    assert(result.successful)
    result match {
      case Success(OrExpression(
      AndExpression(ComparisonExpression(ConstExpression("genres"),
      InSymbol,
      ListValuesExpression(
      Seq(ConstExpression("sci-fi"), ConstExpression("action")))),
      ComparisonExpression(
      ConstExpression("genres"),
      OutSymbol,
      ListValuesExpression(
      Seq(ConstExpression("romance"), ConstExpression("animated"))))),
      ComparisonExpression(ConstExpression("director"), EqSymbol, ConstExpression("Que*Tarantino"))),
      next) =>
      case _ => fail()
    }
  }

  test("complex query alt") {
    val result =
      parser.parseAll(parser.or,
        "genres=in=(sci-fi,action) and genres=out=(romance,animated) or director==Que*Tarantino")
    assert(result.successful)
    result match {
      case Success(OrExpression(
      AndExpression(ComparisonExpression(ConstExpression("genres"),
      InSymbol,
      ListValuesExpression(
      Seq(ConstExpression("sci-fi"), ConstExpression("action")))),
      ComparisonExpression(
      ConstExpression("genres"),
      OutSymbol,
      ListValuesExpression(
      Seq(ConstExpression("romance"), ConstExpression("animated"))))),
      ComparisonExpression(ConstExpression("director"), EqSymbol, ConstExpression("Que*Tarantino"))),
      next) =>
      case _ => fail()
    }
  }

  test("group") {
    val result = parser.parseAll(parser.group, "(director=='Christopher Nolan' or actor==*Bale)")
    assert(result.successful)
    result match {
      case Success(OrExpression(
      ComparisonExpression(ConstExpression("director"), EqSymbol, ConstExpression("Christopher Nolan")),
      ComparisonExpression(ConstExpression("actor"), EqSymbol, ConstExpression("*Bale"))),
      next) =>
      case _ => fail()
    }
  }

  test("without groups - AND takes precedence") {
    val result = parser.parseAll(parser.input,
      "name=='Kill Bill' and director=='Christopher Nolan' or actor==*Bale and year>=2000")
    assert(result.successful)
    result match {
      case Success(OrExpression(AndExpression(_, _), AndExpression(_, _)), next) =>
      case _ => fail()
    }

  }

  test("group or takes precedence") {
    val result = parser
      .parseAll(parser.input, "name=='Kill Bill' and (director=='Christopher Nolan' or actor==*Bale) and year>=2000")
    assert(result.successful)
    result match {
      case Success(AndExpression(ComparisonExpression(_, _, _), OrExpression(_, _), ComparisonExpression(_, _, _)),
      next) =>
      case _ => fail()
    }
  }

  test("and and and") {
    assert(parser.parseAll(parser.input, "name=='Kill Bill';year>=2000;foo!=bar;bar==baz").successful)
    assert(parser.parseAll(parser.input, "name=='Kill Bill' and year>=2000 and foo!=bar and bar==baz").successful)
  }

  test("and or and") {
    assert(parser.parseAll(parser.input, "name=='Kill Bill';year>=2000,foo!=bar;bar==baz").successful)
    assert(parser.parseAll(parser.input, "name=='Kill Bill' and year>=2000 or foo!=bar and bar==baz").successful)
  }
}
