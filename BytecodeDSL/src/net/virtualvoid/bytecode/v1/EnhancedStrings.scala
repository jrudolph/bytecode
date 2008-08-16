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
  case class SpliceExp(exp:Exp,inner:StrToken,sep:String) extends StrToken{
    def chars = exp.chars + ":" + sep
  }
  case class Components(cmps:StrToken*) extends StrToken{
    def chars = cmps.map(_.chars).reduceLeft(_+_)
  }
  
  implicit def extendParser[T](x:Parser[T]):EParser[T] = EParser[T](x)

  val expStartChar = '#'

  def char = "[^#\\]\\[]".r
  def idChar = "\\w".r
  def lit:Parser[StrToken] = char ~ rep(char) ^^ {case first ~ rest => Literal(first :: rest reduceLeft (_+_))}
  def id = idChar ~ rep(idChar)
  def exp:Parser[Exp] = expStartChar ~>
    (id | "{" ~> id <~ "}") ^^ {case first ~ rest => Exp(first :: rest mkString "")}

  def sepChars = "[^)]*".r
  def spliceExp = exp ~ opt(inners) ~ ("*" ~> opt(extendParser('(') ~!> sepChars <~! ')')) ^^ {case exp ~ x ~ separator => SpliceExp(exp,x.getOrElse(Exp("this")),separator.getOrElse(""))}
  
  def innerExp:Parser[StrToken] = spliceExp | exp | lit
  def inners = '[' ~> rep(innerExp) <~ ']' ^^ {x=> Components(x:_*)}

  import scala.util.parsing.input.CharArrayReader.EofCh
  override def token:Parser[Token] = (EofCh ^^^ EOF
     | innerExp )

  override def whitespace = rep('`')
  override def skipWhitespace = false
  
  
  case class EParser[T](oldThis:Parser[T]){
    def ~!> [U](p: => Parser[U]): Parser[U] 
      = OnceParser{ (for(a <- oldThis; b <- commit(p)) yield b).named("~!>") }
      
    def <~! [U](p: => Parser[U]): Parser[T] 
      = OnceParser{ (for(a <- oldThis; b <- commit(p)) yield a).named("<~!") }
  }
}
class StrParser extends TokenParsers{
  type Tokens = StrLexer
  override val lexical = new StrLexer
  import lexical._

  def value:Parser[StrToken] = elem("token",_.isInstanceOf[StrToken]) ^^ {case s:StrToken => s}
  def values = rep(value)
}
object TestParser extends StrParser {
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
    test(List(SpliceExp(Exp("gustav"),Exp("this"),"")),"#gustav*")
    test(List(SpliceExp(Exp("gustav"),Components(List(Literal("wurst")):_*),"")),"#gustav[wurst]*")
    
    test(List(SpliceExp(Exp("gustav"),Components(List(Exp("ab")):_*),"")),"#gustav[#ab]*")
    test(List(SpliceExp(Exp("gustav"),Components(List(Exp("ab")):_*),",")),"#gustav[#ab]*(,)")
    test(List(Literal("blub"),SpliceExp(Exp("gustav"),Exp("this"),","),Literal(" blubber")),"blub#gustav*(,) blubber")
  }
}
