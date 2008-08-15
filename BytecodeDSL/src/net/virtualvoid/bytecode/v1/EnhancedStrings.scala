package net.virtualvoid.bytecode.v1

/*
 * Proposed syntaxes:
 * val ia:Array[int] = Array(1,2,3,4,5)
 *  str("blub (%,{this})",ia) == "blub (1,2,3,4,5)"
 *
 * val strs:Array[String] = Array("a","ab","abc","abcd")
 * str("(%,{this|s})",strs) == "(1,2,3,4)"
 * == str("(%,this)",strs)
 * == "(" + strs.map(_.length).join(",") + ")"
 *
 * Table{
 * String name;
 * Column[] columns;
 * }
 * Column{
 * String name;
 * String type;
 * String[] specs;
 * }
 *
 * str("create table #name (#columns[#name #type #specs,*],*)",t)
 *
 * "create table" + t.name + "(" +
 * t.columns.map(c=> c.name + " " + c.type + " " + c.specs.join(",")).join(",")
 * + ")"
 *
 * Grammar:
 *   exp ::= # idLetter idLetter*
 *   char ::= alles ausser #
 *   lit ::= char char*
 *   str ::= (lit|exp)*
 */

/*trait StrComponent{
  def eval(recv:AnyRef):String
}
case class Literal(str:String) extends StrComponent{
  def eval(recv:AnyRef) = str
}
case class Exp(evaluator:AnyRef=>String) extends StrComponent{
  def eval(recv:AnyRef) = evaluator(recv)
}
case class MyString(components:StrComponent*) extends StrComponent{
  def eval(recv:AnyRef) = components.map(_.eval(recv)).reduceLeft(_+_)
}*/

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax._

class StrLexer extends Lexical with RegexParsers{
  override type Elem = Char

  trait StrToken extends Token
  case class Literal(str:String) extends StrToken{
    def chars = str
  }
  case class ExpLiteral(identifier:String) extends StrToken{
    def chars = identifier
  }

  //type Token = StrToken

  val expStartChar = '#'

  def char = "[^#]".r
  def idChar = letter
  //def num = digit ~ rep(digit) ^^ {case first ~ rest => Literal(first :: rest mkString "")}
  def lit:Parser[Token] = char ~ rep(char) ^^ {case first ~ rest => Literal(first :: rest reduceLeft (_+_))}
  def exp:Parser[Token] = expStartChar ~> idChar ~ rep(idChar) ^^ {case first ~ rest => ExpLiteral(first :: rest mkString "")}

  import scala.util.parsing.input.CharArrayReader.EofCh
  override def token:Parser[Token] = (EofCh ^^^ EOF
     | exp | lit )

  override def whitespace = rep('`')//failure("blub")//rep('`')
  override def skipWhitespace = false
}
class StrParser extends TokenParsers{
  type Tokens = StrLexer
  override val lexical = new StrLexer
  import lexical._

  def value:Parser[StrToken] = elem("token",_.isInstanceOf[StrToken]) ^^ {case s:StrToken => s}
  def values = rep(value)
}
object TestParser extends StrParser {
  //type Input = Reader[StrToken]
  def main(args:Array[String]):Unit = {
    val scanner:Input = new lexical.Scanner(args(0)).asInstanceOf[Input]
    //System.out.println(scanner.first)
    System.out.println(phrase(values)(scanner))
  }
}
