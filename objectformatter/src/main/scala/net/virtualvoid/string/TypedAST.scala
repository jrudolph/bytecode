package net.virtualvoid.string

import _root_.net.virtualvoid.bytecode.Bytecode._

object TypedAST {
  trait Exp[-T,+U] extends (T => U){
	def eval(o:T):U
	override def apply(o:T):U = eval(o) 
  }
  case class ThisExp[T]() extends Exp[T,T]{
    override def eval(o:T) = o
  }
  case class ParentExp[T,U,V](inner:Exp[U,V],parent:Exp[T,U]) extends Exp[T,V]{
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
  case class ToStringConversion[T](exp:Exp[T,AnyRef]) extends Conversion[T,AnyRef](exp){
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
  abstract class Expand[T,C[_],U](exp:Exp[T,C[U]],sep:String,inner:FormatElementList[U]) extends Conversion[T,C[U]](exp){
    def asIterable(c:C[U]):Iterable[U]
    override def convert(i:C[U]) = asIterable(i) map (inner.format _) mkString ""
  }
  case class ExpandArray[T,U](exp:Exp[T,Array[U]],sep:String,inner:FormatElementList[U]) extends Expand[T,Array,U](exp,sep,inner){
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
      ThisExp[T].asInstanceOf[Exp[T,U]] // we checked it
    else
      throw new TypingException("#this has not the expected type "+retCl)
  
  /**
   * checks that outer expression fits nicely around the inner expression and then builds
   * a typed ParentExp
   */
  def typedParentExp[T,U,V](inner:AST.Exp,innerMethod:java.lang.reflect.Method,cl:Class[T],innerType:Class[U],retCl:Class[V]):Exp[T,V] =
    ParentExp[T,U,V](typedExp(inner,innerType,retCl),MethodHandleExp(methodHandle(innerMethod,cl,innerType)))
  
  def typedExp[T,U](exp:AST.Exp,cl:Class[T],retClass:Class[U]):Exp[T,U] = exp match {
    case AST.ThisExp => typedThisExp(cl,retClass)
    case p@AST.ParentExp(inner,parent) => {
      val innerMethod = p.method(cl)
      val innerType = innerMethod.getReturnType
      typedParentExp(inner,innerMethod,cl,innerType,retClass)
    }
    case e:AST.Exp => MethodHandleExp(methodHandle[T,U](e.method(cl),cl,retClass))
  }
  
  def typedExpandArray[T,E<:AnyRef](exp:AST.Exp,sep:String,inner:AST.FormatElementList,cl:Class[T],eleClass:Class[E])
  		:ExpandArray[T,E]
    = ExpandArray(typedExp(exp,cl,classOfArray(eleClass)),sep,typed(inner,eleClass))
  
  def typed[T](ast:AST.FormatElementList,cl:Class[T]):FormatElementList[T] = FormatElementList[T](
    ast.elements.map {
      case AST.Literal(str) => Literal(str)
      case AST.ToStringConversion(exp) => ToStringConversion(typedExp(exp,cl,classOf[AnyRef]))
      case AST.Expand(exp,sep,inner) => {
        val eleType:Class[AnyRef] = exp.returnType(cl).getComponentType.asInstanceOf[Class[AnyRef]]
        typedExpandArray(exp,sep,inner,cl,eleType)
      }
    }
  )
  
  def classOfArray[T<:AnyRef](eleClass:Class[T]):Class[Array[T]] = Class.forName("[L"+eleClass.getName+";").asInstanceOf[Class[Array[T]]]
}

object IterableHelper {
  implicit def java2scala[T](it:java.lang.Iterable[T]):Iterable[T] = new Iterable[T]{
    override def elements:Iterator[T] = new Iterator[T]{
      val underlying = it.iterator
      override def hasNext = underlying.hasNext
      override def next = underlying.next
    }
  }
}
