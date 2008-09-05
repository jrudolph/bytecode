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
 * val t:Table
 * ObjectFormatter.format("create table #name (#columns[#name #type #specs(,)*\n](,)*)",t)
 * is short for
 * "create table" + t.name + "(" +
 * t.columns.map(c=> c.name + " " + c.type + " " + c.specs.join(",")).join(",")
 * + ")"
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
  case class SpliceExp(exp:Exp,sep:String,inner:List[StrToken]) extends StrToken{
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
  def spliceExp = exp ~ opt(inners) ~ opt(extendParser('(') ~!> sepChars <~! ')') <~ "*" ^^ {case exp ~ x ~ separator => SpliceExp(exp,separator.getOrElse(""),x.getOrElse(List(Exp("this"))))}
  
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
trait StrParser extends TokenParsers{
  type Tokens = StrLexer
  override val lexical = new StrLexer
  import lexical._

  def value:Parser[StrToken] = elem("token",_.isInstanceOf[StrToken]) ^^ {case s:StrToken => s}
  def values = rep(value)
  
  def parse(input:String):List[lexical.StrToken] = {
      val scanner:Input = new lexical.Scanner(input).asInstanceOf[Input]
      val output = phrase(values)(scanner)
      output.get
    }
}

object ObjectFormatter extends IObjectFormatterFactory{
  val parser = new StrParser{}
  
  def formatter[T<:AnyRef](fm:String):IObjectFormatter[T] = new IObjectFormatter[T]{
    val parsed = parser.parse(fm)
    def format(o:T) = parsed.map(_.eval(o)) mkString ""
  }
  
  def format(format:String,o:AnyRef) = formatter(format).format(o)
}
