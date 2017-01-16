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

import scala.StringContext.InvalidEscapeException
import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

/**
  * RSQL parser based on https://github.com/jirutka/rsql-parser/
  */
class RsqlParser extends RegexParsers {

  import RsqlParser._
  val AndToken: Parser[String] = ";" | "and"
  val OrToken: Parser[String] = "," | "or"
  val LParenToken: Parser[String] = "("
  val RParenToken: Parser[String] = ")"
  val GtToken: Parser[String] = ">"
  val LtToken: Parser[String] = "<"
  val TrueToken: Parser[String] = "true"
  val FalseToken: Parser[String] = "false"
  val EqToken: Parser[String] = "="
  val NotToken: Parser[String] = "!"
  val Alpha: Parser[String] = """[a-zA-Z]""".r ^^ { _.toString }
  val AnyChar: Parser[String] = ".".r ^^ { _.toString }
  val SingleQuoteToken: Parser[String] = "'"
  val DoubleQuoteToken: Parser[String] = "\""
  val EmptyString: Parser[String] = ""
  val UnreservedChars: Parser[String] = """[^\\'"()<>=;,!~ ]""".r

  def input: Parser[RsqlCondition] = phrase(or)

  def and: Parser[RsqlCondition] = constraint ~ rep(AndToken ~> constraint) ^^ {
    case a ~ (x :: xs) => AndExpression(a :: x :: xs: _*)
    case a ~ Nil => a
  }

  def or: Parser[RsqlCondition] = and ~ rep(OrToken ~> and) ^^ {
    case a ~ (x :: xs) => OrExpression(a :: x :: xs: _*)
    case a ~ Nil => a
  }
  def group: Parser[RsqlCondition] = LParenToken ~> or <~ RParenToken

  def constraint: Parser[RsqlCondition] = group | comparison

  def comparison: Parser[RsqlCondition] = selector ~ operator ~ arguments ^^ {
    case s ~ op ~ arg => ComparisonExpression(s, op, arg)
  }

  def selector: Parser[ConstExpression] = rep1(UnreservedChars) ^^ { c => ConstExpression(c.mkString) }
  def operator: Parser[RsqlOperator] = opFiql | op2Fiql | opAlt
  def opFiql: Parser[RsqlOperator] = EqToken ~> (rep(Alpha) ^^ { _.mkString }) <~ EqToken ^^ {
    case "" => EqSymbol
    case "in" => InSymbol
    case "out" => OutSymbol
    case "lt" => LtSymbol
    case "le" => LtEqSymbol
    case "gt" => GtSymbol
    case "ge" => GtEqSymbol
  }

  def op2Fiql: Parser[RsqlOperator] = NotToken <~ EqToken ^^ (_ => NotEqSymbol)
  def opAlt: Parser[RsqlOperator] = (GtToken | LtToken) ~ opt(EqToken) ^^ {
    case ">" ~ None => GtSymbol
    case ">" ~ Some("=") => GtEqSymbol
    case "<" ~ None => LtSymbol
    case "<" ~ Some("=") => LtEqSymbol
  }

  def valueList: Parser[RsqlValueSymbol] = value ~ rep("," ~> value) ^^ {
    case v ~ Nil => ListValuesExpression(v :: Nil)
    case x ~ xs => ListValuesExpression(x :: xs)
  }
  def arguments: Parser[RsqlValueSymbol] = (LParenToken ~> valueList <~ RParenToken) | value
  def value: Parser[RsqlValueSymbol] = booleanStr | unreservedEscaped | doubleQuoted | singleQuoted
  def booleanStr: Parser[BooleanExpression] = (TrueToken | FalseToken) ^^ {
    case "true" => BooleanExpression(true)
    case "false" => BooleanExpression(false)
    case _ => BooleanExpression(false)
  }

  def unreservedEscaped: Parser[RsqlValueSymbol] = """[^\\'"()<>=;,!~ ]+""".r ^^ { l => ConstExpression(l.mkString)}
  def singleQuoted: Parser[RsqlValueSymbol] =
    SingleQuoteToken ~> ("""([^'\\]|(\\.)\s*)+""".r ^^ (l => ConstExpression(unescape(l.mkString))) | EmptyString ^^ (l => ConstExpression(l))) <~ SingleQuoteToken
  def doubleQuoted: Parser[RsqlValueSymbol] =
    DoubleQuoteToken ~> ("""([^\"\\]|(\\.)\s*)+""".r ^^ (l => ConstExpression(unescape(l.mkString))) | EmptyString ^^ (l => ConstExpression(l)))<~ DoubleQuoteToken


  private def unescape(str: String): String = {
    import java.lang.{StringBuilder => JLSBuilder}
    val len = str.length

    // replace escapes with given first escape
    def replace(first: Int): String = {
      val b = new JLSBuilder

      // append replacement starting at index `i`, with `next` backslash
      @tailrec def loop(i: Int, next: Int): String = {
        if (next >= 0) {
          //require(str(next) == '\\')
          if (next > i) b.append(str, i, next)
          var idx = next + 1
          if (idx >= len) throw new InvalidEscapeException(str, next)
          val c = str.charAt(idx) match {
            case '"' => '"'
            case '\'' => '\''
            case '\\' => '\\'
            case ch => ch
          }
          idx += 1 // advance
          b append c
          loop(idx, str.indexOf('\\', idx))
        } else {
          if (i < len) b.append(str, i, len)
          b.toString
        }
      }

      loop(0, first)
    }

    str indexOf '\\' match {
      case -1 => str
      case i => replace(i)
    }
  }

}

object RsqlParser {
  def apply(input: String): Option[RsqlCondition]  = {
    val parser: RsqlParser = new RsqlParser()
    parser.parseAll(parser.input, input) match {
      case parser.Success(result:RsqlCondition , _) => Some(result)
      case _: parser.NoSuccess => None
    }
  }

  sealed abstract class RsqlSymbol
  sealed abstract class RsqlOperator extends RsqlSymbol
  case object GtSymbol extends RsqlOperator
  case object GtEqSymbol extends RsqlOperator
  case object LtSymbol extends RsqlOperator
  case object LtEqSymbol extends RsqlOperator
  case object EqSymbol extends RsqlOperator
  case object NotEqSymbol extends RsqlOperator
  case object InSymbol extends RsqlOperator
  case object OutSymbol extends RsqlOperator
  sealed abstract class RsqlValueSymbol extends RsqlSymbol
  case class ConstExpression(value: String) extends RsqlValueSymbol
  case class BooleanExpression(value: Boolean) extends RsqlValueSymbol
  case class EscConstExpression(value: String) extends RsqlValueSymbol
  case class ListValuesExpression(values: Seq[RsqlValueSymbol]) extends RsqlValueSymbol

  sealed abstract class RsqlCondition extends RsqlSymbol
  case class ComparisonExpression(selector: ConstExpression, operator: RsqlOperator, arguments: RsqlValueSymbol) extends RsqlCondition
  case class AndExpression(expressions: RsqlCondition*) extends RsqlCondition
  case class OrExpression(expressions: RsqlCondition*) extends RsqlCondition
}
