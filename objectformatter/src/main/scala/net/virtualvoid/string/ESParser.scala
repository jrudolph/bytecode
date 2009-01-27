package net.virtualvoid.string

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

object AST{
  //tokens
  trait StrToken {
    def eval(o:AnyRef):AnyRef
  }
  case class StrTokens(toks:Seq[StrToken]) extends StrToken{
    def chars = ""
    def eval(o:AnyRef):String = toks.map(_.eval(o)) mkString "" 
  }
  case class Literal(str:String) extends StrToken{
    def chars = str
    def eval(o:AnyRef):String = str
  }
  case class ConvertedExpression(exp:Exp,conv:Conversion) extends StrToken{
    def eval(o:AnyRef):AnyRef = conv.eval(exp.eval(o))
  }
  
  // expressions
  case class Exp(identifier:String){
    def chars = identifier

    import java.lang.reflect.{Method}
    import java.lang.NoSuchMethodException
    def findMethod(c:Class[_],name:String):Option[Method] =
     try {
       val res = c.getMethod(name,null)
       res.setAccessible(true)
       Some(res)
     }catch{
     case _:NoSuchMethodException => None
     }
    def method(cl:Class[_]):Method = 
      Array("get"+capitalize(identifier),identifier)
        .flatMap(findMethod(cl,_).toList).firstOption
        .getOrElse(throw new java.lang.Error("couldn't find method " + identifier + " in class "+cl.getName))
    def returnType(callingCl:Class[_]):Class[_] = method(callingCl).getReturnType
    def genericReturnType(callingCl:Class[_]):java.lang.reflect.Type = method(callingCl).getGenericReturnType
    def capitalize(s:String):String = s.substring(0,1).toUpperCase + s.substring(1)
    def eval(o:AnyRef) = method(o.getClass).invoke(o,null)
  }
    case object ThisExp extends Exp(""){
    override def eval(o:AnyRef) = o
    override def returnType(callingCl:Class[_]):Class[_] = callingCl
    override def genericReturnType(callingCl:Class[_]):java.lang.reflect.Type =
      throw new java.lang.Error("No generic type information available for "+callingCl.getName+
                                  " since it is erased. #this can't be used in conditional or expand expressions")
  }
  case class ParentExp(inner:Exp,parent:String) extends Exp(parent){
    override def eval(o:AnyRef) = inner.eval(super.eval(o))
  }
  
  // conversions
  trait Conversion{
    def eval(o:AnyRef):String
  }
  case object ToStringConversion extends Conversion{
    def eval(o:AnyRef):String = o.toString
  }
  case class Conditional(thenToks:StrTokens,elseToks:StrTokens) extends Conversion{
    def eval(o:AnyRef) = o match {
      case java.lang.Boolean.TRUE => thenToks.eval(o)
      case java.lang.Boolean.FALSE => elseToks.eval(o)
      case x:Option[AnyRef] => x.map(thenToks.eval).getOrElse(elseToks.eval(o))
    }
  }
  case class DateConversion(format:String) extends Conversion{
    val df = new java.text.SimpleDateFormat(format)
    def eval(o:AnyRef) = df.format(o match {
      case cal:java.util.Calendar => cal.getTime
      case date:java.util.Date => date
    })
    def chars = ""
  }
  case class Expand(sep:String,inner:StrTokens) extends Conversion{
    def realEval(l:Iterable[AnyRef]):String = l.map(inner.eval(_)) mkString sep
    import Java.it2it
    def eval(o:AnyRef) = o match{
      // array or collection or similar
    case l : java.lang.Iterable[AnyRef] => realEval(l)
    case l : Seq[AnyRef] => realEval(l)
    }
  }
}

object EnhancedStringFormatParser extends RegexParsers{
  import AST._
  
  override type Elem = Char
  type Tokens = StrToken

  implicit def extendParser[T](x:Parser[T]):EParser[T] = EParser[T](x)

  def escapedByDoubling(char:String):Parser[String] = char ~ char ^^ (x=>char)

  val expStartChar = '#'

  def char = "[^#\\]|\\[]".r | escapedByDoubling("[") | escapedByDoubling("]") | escapedByDoubling("#") | escapedByDoubling("|")
  def idChar = "\\w".r
  def lit:Parser[StrToken] = char ~ rep(char) ^^ {case first ~ rest => Literal(first :: rest reduceLeft (_+_))}

  def idPart:Parser[String] = idChar ~ rep(idChar) ^^ {case first ~ rest => first :: rest mkString ""}
  def id:Parser[Exp] =
    "this" 					^^ {str => ThisExp} |
    idPart ~ opt("." ~> id) ^^ {case str ~ Some(inner) => ParentExp(inner,str)
                                case str ~ None => Exp(str)}

  def exp:Parser[Exp] = expStartChar ~>
    (id | extendParser("{") ~!> id <~! "}")

  def sepChars = "[^}]*".r
  def expansion = opt('[' ~> tokens <~ ']') ~ opt(extendParser('{') ~!> sepChars <~! '}') <~ "*" ^^ {case x ~ separator => Expand(separator.getOrElse(""),x.getOrElse(StrTokens(List(ConvertedExpression(ThisExp,ToStringConversion)))))}
  
  def dateConverter:Parser[String] = extendParser("->date[") ~!> "[^\\]]*".r <~ "]" 
  def dateConversion = dateConverter ^^ {case format => DateConversion(format)}
  
  def clauses = extendParser("?[") ~!> 
    (tokens ~ "|" ~ tokens <~ "]")
  def conditional = clauses ^^ {case (ifs ~ sep ~ thens) => Conditional(ifs,thens)}
  
  def conversion = conditional | dateConversion | expansion

  def convertedExpression = exp ~ opt(conversion) ^^ {case exp ~ conv => ConvertedExpression(exp,conv.getOrElse(ToStringConversion))}
  
  def formatPart:Parser[StrToken] = convertedExpression | lit 
  
  def tokens:Parser[StrTokens] = rep(formatPart) ^^ {case toks => StrTokens(toks)}
  
  override val skipWhitespace = false

  case class EParser[T](oldThis:Parser[T]){
    def ~!> [U](p: => Parser[U]): Parser[U]
      = OnceParser{ (for(a <- oldThis; b <- commit(p)) yield b).named("~!>") }

    def <~! [U](p: => Parser[U]): Parser[T]
      = OnceParser{ (for(a <- oldThis; b <- commit(p)) yield a).named("<~!") }
  }
  
  def parse(input:String):StrTokens = 
    phrase(tokens)(new scala.util.parsing.input.CharArrayReader(input.toCharArray)) match {
      case Success(res,_) => res
      case x:NoSuccess => error(x.msg)
    }
}