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
  case class Exp(identifier:String) extends StrToken{
    def chars = identifier
  }
  case class SpliceExp(exp:Exp,sep:String) extends StrToken{
    def chars = exp.chars + ":" + sep
  }

  //type Token = StrToken

  val expStartChar = '#'

  def char = "[^#]".r
  def idChar = letter
  //def num = digit ~ rep(digit) ^^ {case first ~ rest => Literal(first :: rest mkString "")}
  def lit:Parser[Token] = char ~ rep(char) ^^ {case first ~ rest => Literal(first :: rest reduceLeft (_+_))}
  def id = idChar ~ rep(idChar)
  def exp:Parser[Exp] = expStartChar ~>
    (id | "{" ~> id <~ "}") ^^ {case first ~ rest => Exp(first :: rest mkString "")}

  def sepChars = "[^*]*".r
  def spliceExp = exp ~ sepChars <~ "*" ^^ {case exp ~ separator => SpliceExp(exp,separator)}

  import scala.util.parsing.input.CharArrayReader.EofCh
  override def token:Parser[Token] = (EofCh ^^^ EOF
     | spliceExp | exp | lit )

  override def whitespace = rep('`')
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
    import lexical.StrToken
    def test(s:List[StrToken],input:String) = {
      val scanner:Input = new lexical.Scanner(input).asInstanceOf[Input]
      val output = phrase(values)(scanner)
      System.out.println(output.get.toString+" should be "+s.toString)
      assert(s.toString == output.get.toString)
    }
    import lexical._
    test(List(Literal("blub"),Exp("gustav"),Literal(" blubber")),"blub#gustav blubber")
    test(List(Literal("blub"),Exp("gustav"),Literal(" blubber")),"blub#gustav,* blubber")
  }
}
