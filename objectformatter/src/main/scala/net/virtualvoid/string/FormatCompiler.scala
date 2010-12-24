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

  val append = method2((_:StringBuilder).append(_:String))
   
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
      /*case Expand(exp,sep,inner) => {
        import Bytecode.RichOperations.foldIterator
          
        val retType = exp.returnType(cl)

        if (classOf[java.lang.Iterable[_]].isAssignableFrom(retType)){
          val eleType:Class[AnyRef] = elementType(exp.genericReturnType(cl)
                                                  ,classOf[java.lang.Iterable[_]])
                                                  .asInstanceOf[Class[AnyRef]]
          
          f ~
            value.load ~
            compileGetExp(exp,cl,classOf[java.lang.Iterable[AnyRef]]) ~
            invokemethod1(_.iterator) ~
            swap() ~
            foldIterator[R,AnyRef,StringBuilder](
              it => 
              	_ ~ withLocal(innerValue => compileFormatElementList(inner,eleType,innerValue)) ~
              	  it.load ~
              	  invokemethod1(_.hasNext) ~
              	  ifne2(
              	  	_ ~
              	  	  ldc(sep) ~
              	  	  invokemethod2(_.append(_))
                    ,f=>f
              	  )
            )(scala.reflect.Manifest.classType(eleType),cat1AnyRef)
        }
        else if (retType.isArray){
          val eleType:Class[AnyRef] = retType.getComponentType.asInstanceOf[Class[AnyRef]]

          if (eleType.isPrimitive)
            throw new java.lang.Error("can't handle primitive arrays right now");

          import Bytecode.RichOperations.foldArray
          
          f ~
            value.load ~
            compileGetExp(exp,cl,retType.asInstanceOf[Class[Array[AnyRef]]]) ~
            dup ~
            arraylength ~
            bipush(1) ~
            isub ~
            withLocal(lastIndex =>
              _ ~
	            withLocal(array => 
	              foldArray(array)(index =>
	               	_ ~ withLocal(innerValue => compileFormatElementList(inner,eleType,innerValue)) ~
	               	  lastIndex.load ~
	               	  index.load ~
	               	  isub ~
	               	  ifne2(
                        _ ~
                          ldc(sep) ~
                          invokemethod2(_.append(_))
                        , f=>f)))	              
            )
        }
        else
          throw new java.lang.Error("can only iterate over "+
                                      "iterables and arrays right now")
      }*/
      /*case Conditional(inner,thens,elses) => {
        val retType = inner.returnType(cl)

        if (retType == java.lang.Boolean.TYPE || 
              classOf[java.lang.Boolean].isAssignableFrom(retType)){
          f ~ 
            value.load ~
            (if (retType == java.lang.Boolean.TYPE)
               compileGetExp(inner,cl,classOf[Boolean])
             else 
               _ ~ compileGetExp(inner,cl,classOf[java.lang.Boolean]) 
                 ~ invokemethod1(_.booleanValue)
            ) ~
            ifeq2(
              compileFormatElementList(elses,cl,value),
              compileFormatElementList(thens,cl,value))
        }
        else if (classOf[Option[AnyRef]].isAssignableFrom(retType)){
          val eleType = elementType(inner.genericReturnType(cl)
                                    ,classOf[Option[_]])
            .asInstanceOf[Class[AnyRef]]
          f ~
            value.load ~
            compileGetExp(inner,cl,classOf[Option[AnyRef]]) ~
            dup ~
            invokemethod1(_.isDefined) ~
            ifeq2(
              _ ~ pop ~ compileFormatElementList(elses,cl,value),
              _ ~ 
                checkcast(classOf[Some[AnyRef]]) ~
                invokemethod1(_.get) ~
                withLocal(newValue => compileFormatElementList(thens,eleType,newValue)))
        }
        else
          f ~
            value.load ~
            compileGetExp(inner,cl,classOf[AnyRef]) ~
            dup ~
            ifnull(
            	_ ~ pop ~ compileFormatElementList(elses,cl,value),
                _ ~ withLocal{newValue => compileFormatElementList(thens,retType.asInstanceOf[Class[AnyRef]],newValue)})
      }
      case DateConversion(exp,format) => {
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
