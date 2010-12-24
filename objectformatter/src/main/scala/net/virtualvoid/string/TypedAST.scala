package net.virtualvoid.string

import net.virtualvoid.bytecode._
import Methods._

object TypedAST {
  trait Exp[-T,+U] extends (T => U){
	def eval(o:T):U
	override def apply(o:T):U = eval(o) 
  }
  case object ThisExp extends Exp[Any,Any]{
    override def eval(o:Any) = o
  }
  case class ParentExp[T<:AnyRef,U<:AnyRef,V](inner:Exp[U,V],parent:Exp[T,U]) extends Exp[T,V]{
    override def eval(o:T) = inner.eval(parent.eval(o))
  }
  case class MethodHandleExp[-T,+U](m:Method1[T,U]) extends Exp[T,U] {
    override def eval(o:T) = m.method.invoke(o).asInstanceOf[U]
  }
  
  trait FormatElement[-T] extends (T => String){
    def format(o:T):String
    override def apply(o:T):String = format(o)
  }  
  case class Literal(str:String) extends FormatElement[Any]{
    def format(o:Any):String = str
  }
  abstract class Conversion[T,U](exp:Exp[T,U]) extends FormatElement[T]{
    def convert(o:U):String
    def format(o:T):String = convert(exp.eval(o))
  }
  case class ToStringConversion[T<:AnyRef](exp:Exp[T,AnyRef]) extends Conversion[T,AnyRef](exp){
    def convert(o:AnyRef) = o.toString
  }/*
  case class Conditional(condition:Exp,thenToks:FormatElementList,elseToks:FormatElementList) extends FormatElement{
    def chars =""
    def format(o:AnyRef) = condition.eval(o) match {
      case java.lang.Boolean.TRUE => thenToks.format(o)
      case java.lang.Boolean.FALSE => elseToks.format(o)
      case x:Option[AnyRef] => x.map(thenToks.format).getOrElse(elseToks.format(o))
      case null => elseToks.format(o)
      case x => thenToks.format(x)
    }
  }
  case class DateConversion(exp:Exp,format:String) extends FormatElement{
    val df = new java.text.SimpleDateFormat(format)
    def format(o:AnyRef) = df.format(exp.eval(o) match {
      case cal:java.util.Calendar => cal.getTime
      case date:java.util.Date => date
    })
    def chars = ""
  }*/
  case class ConditionalOption[T<:AnyRef,U<:AnyRef](exp:Exp[T,Option[U]],thenToks:FormatElementList[U],elseToks:FormatElementList[T]) extends FormatElement[T]{
    override def format(o:T):String = exp(o) match{
      case Some(x) => thenToks.format(x)
      case None => elseToks.format(o)
    }
  }
  abstract class Expand[T,C[_],U](exp:Exp[T,C[U]],sep:String,inner:FormatElementList[U]) extends Conversion[T,C[U]](exp){
    def asIterable(c:C[U]):Iterable[U]
    override def convert(i:C[U]) = asIterable(i) map (inner.format _) mkString sep
  }
  case class ExpandArray[T,U<:AnyRef](exp:Exp[T,Array[U]],sep:String,inner:FormatElementList[U]) extends Expand[T,Array,U](exp,sep,inner){
    override def asIterable(i:Array[U]) = i
  }
  case class ExpandJavaIterable[T,U](exp:Exp[T,java.lang.Iterable[U]],sep:String,inner:FormatElementList[U]) extends Expand[T,java.lang.Iterable,U](exp,sep,inner){
    override def asIterable(i:java.lang.Iterable[U]) = IterableHelper.java2scala(i)
  }
  case class FormatElementList[T](elements:Seq[FormatElement[T]]){
    def format(o:T):String = elements.map(_.format(o)) mkString "" 
  }
  
  class TypingException(msg:String) extends RuntimeException(msg)
  
  /**
   * checks that `this` can be (up)casted to the desired return type
   */
  def typedThisExp[T,U](cl:Class[T],retCl:Class[U]):Exp[T,U] =
    if (retCl.isAssignableFrom(cl))
      ThisExp.asInstanceOf[Exp[T,U]] // we checked it
    else
      throw new TypingException("#this has not the expected type "+retCl)
  
  /**
   * checks that outer expression fits nicely around the inner expression and then builds
   * a typed ParentExp
   */
  def typedParentExp[T<:AnyRef,U<:AnyRef,V](inner:AST.Exp,innerMethod:java.lang.reflect.Method,cl:Class[T],innerType:Class[U],retCl:Class[V]):Exp[T,V] =
    ParentExp[T,U,V](typedExp(inner,innerType,retCl),MethodHandleExp(dynMethod(innerMethod,cl,innerType)))
  
  def typedExp[T<:AnyRef,U](exp:AST.Exp,cl:Class[T],retClass:Class[U]):Exp[T,U] = exp match {
    case AST.ThisExp => typedThisExp(cl,retClass)
    case p@AST.ParentExp(inner,parent) => {
      val innerMethod = p.method(cl)
      val innerType:Class[AnyRef] = innerMethod.getReturnType.asInstanceOf[Class[AnyRef]] //TODO: check that here
      typedParentExp(inner,innerMethod,cl,innerType,retClass)
    }
    case e:AST.Exp => MethodHandleExp(dynMethod[T,U](e.method(cl),cl,retClass))
  }
  
  def typedExpandArray[T<:AnyRef,E<:AnyRef](exp:AST.Exp,sep:String,inner:AST.FormatElementList,cl:Class[T],eleClass:Class[E])
  		:ExpandArray[T,E]
    = ExpandArray(typedExp(exp,cl,classOfArray(eleClass)),sep,typed(inner,eleClass))
    
  def typedConditionalOption[T<:AnyRef,U<:AnyRef](c:AST.Conditional,cl:Class[T],eleClass:Class[U]):ConditionalOption[T,U] = {
    val AST.Conditional(exp,thenB,elseB) = c
    ConditionalOption(typedExp(exp,cl,classOf[Option[U]]),typed(thenB,eleClass),typed(elseB,cl))
  }
  
  def typed[T<:AnyRef](ast:AST.FormatElementList,cl:Class[T]):FormatElementList[T] = FormatElementList[T](
    ast.elements.map {
      case AST.Literal(str) => Literal(str)
      case AST.ToStringConversion(exp) => ToStringConversion(typedExp(exp,cl,classOf[AnyRef]))
      case AST.Expand(exp,sep,inner) => {
        val eleType:Class[AnyRef] = exp.returnType(cl).getComponentType.asInstanceOf[Class[AnyRef]]
        if (eleType == null)
          throw new RuntimeException(exp+" must evaluate into an array but is "+exp.returnType(cl))
        
        typedExpandArray(exp,sep,inner,cl,eleType)
      }
      case c@AST.Conditional(exp,_,_) => {
        if (classOf[Option[_]].isAssignableFrom(exp.returnType(cl))){
          val eleType = elementType(exp.genericReturnType(cl),classOf[Option[_]])
          typedConditionalOption(c,cl,eleType)
        }
        else
          throw new RuntimeException("Only Option conditionals supported")
      }
    }
  )
  
  def classOfArray[T<:AnyRef](eleClass:Class[T]):Class[Array[T]] = Class.forName("[L"+eleClass.getName+";").asInstanceOf[Class[Array[T]]]
  def elementType(it:java.lang.reflect.Type,of:Class[_])
  	:Class[_ <: AnyRef] = {
    TypeHelper.genericInstanceType(it,of,Array()) match{
      case Some(cl:java.lang.Class[AnyRef]) => cl
      case _ => throw new java.lang.Error("Can't get element type of "+it)
    }
  }
}
case class Person(name:String,town:String)
object IterableHelper {
  implicit def java2scala[T](it:java.lang.Iterable[T]):Iterable[T] = new Iterable[T]{
    override def iterator:Iterator[T] = new Iterator[T]{
      val underlying = it.iterator
      override def hasNext = underlying.hasNext
      override def next = underlying.next
    }
  }
}

/*
import net.virtualvoid.string._
val ast = EnhancedStringFormatParser parse "#this[#name lives in #town]{,}*"
TypedAST.typed(ast,classOf[Array[Person]])
 
 */
