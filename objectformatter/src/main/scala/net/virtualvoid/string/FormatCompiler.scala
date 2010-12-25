package net.virtualvoid.string

import java.lang.{StringBuilder,String=>jString}

object Compiler{
  import net.virtualvoid.bytecode._
  import net.virtualvoid.bytecode.backend.ASM
  import Bytecode._
  import Bytecode.Instructions._
  import Methods._

  val parser = EnhancedStringFormatParser
  import TypedAST._

  lazy val append = method2((_:StringBuilder).append(_:String))
  // looks good, but doesn't help us at all, because foldIterable has to work with erased types and uses manifests anyway
  //def iteratorM[U <: AnyRef]: Method1[java.lang.Iterable[U], java.util.Iterator[U]] = method1((_: java.lang.Iterable[U]).iterator)
  lazy val iteratorM = method1((_: java.lang.Iterable[AnyRef]).iterator)
  lazy val hasNextM = method1((_: java.util.Iterator[AnyRef]).hasNext)
   
  def compileExp[R<:List,T<:AnyRef,Ret: NoUnit](exp:Exp[T,Ret])
                                  :F[R**T] => F[R**Ret] =
    exp match {
      case ParentExp(inner,parent) => _ ~ compileExp(parent) ~ compileExp(inner)
      case MethodHandleExp(method) => _ ~ method
      case ThisExp => f => f
    }
    
  def compileFormatElementList[R<:List,T<:AnyRef]
                 (elements:FormatElementList[T],value:ROLocal[T])
                 (f:F[R**StringBuilder]):F[R**StringBuilder] =
    elements.elements.foldLeft(f){(frame,element) => 
      compileElement(element,value)(frame)}

  def compileElement[R<:List,T <: AnyRef]
                     (ele:FormatElement[T],value:ROLocal[T])
                     (f:F[R**StringBuilder])
                     :F[R**StringBuilder]
    = ele match {
      case Literal(str) => 
        f ~ ldc(str) ~ append
      case ToStringConversion(e) =>
        f ~ value.load ~
          compileExp(e) ~ 
          method1((_:AnyRef).toString) ~ 
          append
      case ExpandArray(exp,sep,inner) => {
		  import Bytecode.RichOperations.foldArray
		  
		  f ~
		    value.load ~
		    compileExp(exp) ~
		    dup ~
		    withLocal(array =>
		      _ ~
			    arraylength ~
			    bipush(1) ~
			    isub ~
			    withLocal(lastIndex =>
			        foldArray(array)(index =>
			          _ ~  
			            withLocal(innerValue => compileFormatElementList(inner,innerValue)) ~
			            lastIndex.load ~
			            index.load ~
			            isub ~
			            ifne2(
			              _ ~
			                ldc(sep) ~
			                append
			              , nop
			            )
			        )
			    )
            )
      }
      case ExpandJavaIterable(exp, sep, inner, eleType) => {
	  import Bytecode.RichOperations.foldIterator
          f ~
            value.load ~
            compileExp(exp) ~
            iteratorM ~
            swap() ~
            foldIterator[R,AnyRef,StringBuilder](
              it => 
              	_ ~ withLocal(innerValue => compileFormatElementList(inner, innerValue)) ~
              	  it.load ~
              	  hasNextM ~
              	  ifne2(
              	  	_ ~
              	  	  ldc(sep) ~
              	  	  append
                    ,f=>f
              	  )
            )(scala.reflect.Manifest.classType(eleType),cat1AnyRef)
      }
      case ConditionalOption(exp,then,elseB) => {
        f ~
            value.load ~
            compileExp(exp) ~
            dup ~
            method1((_:Option[AnyRef]).isDefined) ~
            ifeq2(
              _ ~ pop /*None*/ ~ compileFormatElementList(elseB,value),
              _ ~ 
                method1((_:Option[AnyRef]).get) ~
                withLocal(newValue => compileFormatElementList(then,newValue)))
      }
      case ConditionalBoolean(exp, thenB, elseB) => {
        f ~
          value.load ~
          compileExp(exp) ~
          ifeq2(
            _ ~ compileFormatElementList(elseB, value),
            _ ~ compileFormatElementList(thenB, value))            
      }
      case ConditionalAnyRef(exp, thenB, elseB) => {
        f ~
          value.load ~
          compileExp(exp) ~
          dup ~
          ifnull(
            _ ~ pop /* null */ ~ compileFormatElementList(elseB, value),
            _ ~ withLocal(innerValue => compileFormatElementList(thenB, innerValue)))
      }
      /*case DateConversion(exp,format) => {
        val retType = exp.returnType(cl)
        
        val DateClass:Class[java.util.Date] = classOf[java.util.Date]
        val CalendarClass:Class[java.util.Calendar] = classOf[java.util.Calendar]
        
        f ~ newInstance(classOf[java.text.SimpleDateFormat]) ~
          dup ~
          ldc(format) ~ 
          invokemethod2(_.applyPattern(_)) ~ pop_unit ~
          value.load ~
          (f => 
            retType match {
              case x if DateClass.isAssignableFrom(x)     => 
                f ~ compileGetExp(exp,cl,DateClass)
              case x if CalendarClass.isAssignableFrom(x) => 
                f ~                
                  compileGetExp(exp,cl,CalendarClass) ~ 
                  invokemethod1(_.getTime)
              case _ => throw new java.lang.Error(
                "Expected date- or calendar- typed property. "+
                cl+" can't be converted.") 
            }
          ) ~
          invokemethod2(_.format(_)) ~
          invokemethod2(_.append(_))
      }*/
    }
  def compile[T<:AnyRef](format:String,cl:Class[T]):T=>jString = {
    val elements:FormatElementList[T] = typed(parser.parse(format),cl)
    ASM.compile(cl)(value =>
      _ ~ 
        newInstance(classOf[StringBuilder]) ~
        compileFormatElementList(elements,value) ~
        method1((_:StringBuilder).toString)
    )
  }
}

object FormatCompiler extends IObjectFormatterFactory {
  def formatter[T<:AnyRef](clazz:Class[T],fmt:String) = Compiler.compile[T](fmt,clazz)
}
