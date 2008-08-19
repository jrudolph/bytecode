package net.virtualvoid.string

/*
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
 * str("create table #name (#columns[#name #type #specs(,)*\n](,)*)",t)
 *
 * "create table" + t.name + "(" +
 * t.columns.map(c=> c.name + " " + c.type + " " + c.specs.join(",")).join(",")
 * + ")"
 *
 */

// The Visible API

trait IObjectFormatterFactory {
  def format(format:String,o:AnyRef):String
  def formatter[T<:AnyRef](format:String):IObjectFormatter[T]
}

trait IObjectFormatter[T<:AnyRef] {
  def format(o:T):String
}

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax._

object Java{
  implicit def it2it[T](it:java.lang.Iterable[T]):Iterable[T] = new Iterable[T]{
    def elements = new Iterator[T]{
      val innerIt = it.iterator
      def hasNext = innerIt.hasNext
      def next = innerIt.next
    }
  }
}

class StrLexer extends Lexical with RegexParsers{
  override type Elem = Char

  trait StrToken extends Token {
    def eval(o:AnyRef):AnyRef
  }
  case class Literal(str:String) extends StrToken{
    def chars = str
    def eval(o:AnyRef):String = str
  }
  case class Exp(identifier:String) extends StrToken{
    def chars = identifier
    
    import java.lang.reflect.{Method}
    import java.lang.NoSuchMethodException
    def method(c:Class[_],name:String):Option[Method] =
     try {
       val res = c.getMethod(name,null)
       res.setAccessible(true)
       Some(res)
     }catch{
     case _:NoSuchMethodException => None
     }
    def capitalize(s:String):String = s.substring(0,1).toUpperCase + s.substring(1)
    def eval(o:AnyRef) = identifier match {
    case "this" => o
    case _ => Array("get"+capitalize(identifier),identifier).flatMap(method(o.getClass,_).toList).first.invoke(o,null)
    }
  }
  case class ParentExp(inner:Exp,parent:String) extends Exp(parent){
    override def eval(o:AnyRef) = inner.eval(super.eval(o))
  }
  case class SpliceExp(exp:Exp,sep:String,inner:StrToken*) extends StrToken{
    def chars = exp.chars + ":" + sep
    def realEval(l:Iterable[AnyRef]):String = l.map(e => inner.map(_.eval(e)) mkString "") mkString sep
    import Java.it2it
    def eval(o:AnyRef) = exp.eval(o) match{
      // array or collection or similar
    case l : java.lang.Iterable[AnyRef] => realEval(l)
    case l : Seq[AnyRef] => realEval(l)
    }
  }
  
  implicit def extendParser[T](x:Parser[T]):EParser[T] = EParser[T](x)

  val expStartChar = '#'

  def char = "[^#\\]\\[]".r
  def idChar = "\\w".r
  def lit:Parser[StrToken] = char ~ rep(char) ^^ {case first ~ rest => Literal(first :: rest reduceLeft (_+_))}
  
  def idPart:Parser[String] = idChar ~ rep(idChar) ^^ {case first ~ rest => first :: rest mkString ""} 
  def id:Parser[Exp] = idPart ~ opt("." ~> id) ^^ {case str ~ Some(inner) => ParentExp(inner,str)
                                                   case str ~ None => Exp(str) 
                                                  }  

  def exp:Parser[Exp] = expStartChar ~>
    (id | extendParser("{") ~!> id <~! "}")

  def sepChars = "[^)]*".r
  def spliceExp = exp ~ opt(inners) ~ opt(extendParser('(') ~!> sepChars <~! ')') <~ "*" ^^ {case exp ~ x ~ separator => SpliceExp(exp,separator.getOrElse(""),x.getOrElse(List(Exp("this"))):_*)}
  
  def innerExp:Parser[StrToken] = spliceExp | exp | lit
  def inners = '[' ~> rep(innerExp) <~ ']'

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

trait IAccount{
  def getBank():String
  def getAccountNo():String
}
trait IPerson{
  def getFirstName():String
  def getLastName():String
  def getAccounts():java.util.List[IAccount]
}

object ObjectFormatter extends IObjectFormatterFactory{
  object Parser extends StrParser{
    def parse(input:String):List[lexical.StrToken] = {
      val scanner:Input = new lexical.Scanner(input).asInstanceOf[Input]
      val output = phrase(values)(scanner)
      output.get
    }
  }
  
  def formatter[T<:AnyRef](fm:String):IObjectFormatter[T] = new IObjectFormatter[T]{
    val parsed = Parser.parse(fm)
    def format(o:T) = parsed.map(_.eval(o)) mkString ""
  }
  
  def format(format:String,o:AnyRef) = formatter(format).format(o)
}

object TestParser extends StrParser {
  def parse(input:String):List[lexical.StrToken] = {
    val scanner:Input = new lexical.Scanner(input).asInstanceOf[Input]
    val output = phrase(values)(scanner)
    output.get
  }
  def eval(s:String,o:AnyRef):String =
    parse(s).map(_.eval(o)) mkString ""
  
  def main(args:Array[String]):Unit = {
    case class Account(nummer:String,bank:String) extends IAccount{
      def getBank:String = bank
      def getAccountNo:String = nummer
    }
    object Peter extends IPerson{
      def getFirstName:String = "Peter"
      def getLastName:String = "Paul"
      def getAccounts():java.util.List[IAccount] = 
        java.util.Arrays.asList(Array(Account("234234","Rich Bank Berlin"),Account("3424234","Park Bank")))
    }    
    
    import lexical.StrToken
    def test(s:List[StrToken],input:String) = {
      val scanner:Input = new lexical.Scanner(input).asInstanceOf[Input]
      val output = phrase(values)(scanner)
      System.out.println(output.get.toString+" should be "+s.toString)
      assert(s.toString == output.get.toString)
    }
    import lexical._
       
    System.out.println(eval("Name: #firstName #lastName\nAccounts:\n#accounts[#accountNo at #bank](\n)*",Peter))
    System.out.println(eval("#this[#this has the length #length and consists of the chars #toCharArray['#this'](,)*](\n)*",Array("test","long string")))
    
    test(List(Literal("blub"),Exp("gustav"),Literal(" blubber")),"blub#gustav blubber")
    test(List(ParentExp(Exp("wurst"),"test")),"#{test.wurst}")
    test(List(ParentExp(Exp("wurst"),"test")),"#test.wurst")
    test(List(SpliceExp(Exp("gustav"),"",List(Exp("this")):_*)),"#gustav*")
    test(List(SpliceExp(Exp("gustav"),"",List(Literal("wurst")):_*)),"#gustav[wurst]*")
    
    test(List(SpliceExp(Exp("gustav"),"",List(Exp("ab")):_*)),"#gustav[#ab]*")
    test(List(SpliceExp(Exp("gustav"),",",List(Exp("ab")):_*)),"#gustav[#ab](,)*")
    test(List(Literal("blub"),SpliceExp(Exp("gustav"),",",List(Exp("this")):_*),Literal(" blubber")),"blub#gustav(,)* blubber")
    
    test(List(Literal("blub"), SpliceExp(Exp("gustav"),",",List(Literal("abc "), SpliceExp(Exp("av"),"",List(Literal("wurst")):_*)):_*), Literal(" blubber"))
              ,"blub#gustav[abc #av[wurst]*](,)* blubber")
  }
}
