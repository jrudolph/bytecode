package net.virtualvoid.string

import java.lang.{StringBuilder,String=>jString}

object Compiler{
  import net.virtualvoid.bytecode.Bytecode
  import net.virtualvoid.bytecode.ASMCompiler
  import Bytecode._
  import Bytecode.Operations._
  import Bytecode.Implicits._

  val parser = EnhancedStringFormatParser
  import AST._
  
  def elementType(it:java.lang.reflect.Type,of:Class[_]):Class[_ <: AnyRef] = {
    TypeHelper.genericInstanceType(it,of,Array()) match{
      case Some(cl:java.lang.Class[AnyRef]) => cl
      case _ => throw new java.lang.Error("Can't get element type of "+it)
    }
  }

  def compileGetExp[R<:List,LR<:List,T,Ret](exp:Exp,cl:Class[T],retType:Class[Ret])(f:F[R**T,LR]):F[R**Ret,LR] = exp match{
    case p@ParentExp(inner,parent) =>{
      val m = p.method(cl)
      f ~ dynMethod(m,classOf[AnyRef]) ~ 
       compileGetExp(inner,m.getReturnType.asInstanceOf[Class[Object]],retType)
    }
    case ThisExp =>
      f ~ checkcast(retType) // TODO: don't know why we need this, examine it
    case e:Exp => {
      f ~ dynMethod(e.method(cl),retType)
    }
  }

  def withLocal0Saved[R<:List,LR<:List,L0,T](f:F[R**L0**StringBuilder,LR**T] => F[R**L0**StringBuilder,LR**T]):F[R**StringBuilder**T,LR**L0] => F[R**StringBuilder,LR**L0] =
    _ ~
      local[_0,L0].load() ~
      swap ~
      local[_0,T].store() ~
      swap ~
      f ~
      swap ~
      local[_0,L0].store[R**StringBuilder,LR**T]()
    
  def compileFormatElementList[R<:List,LR<:List,T<:java.lang.Object](elements:FormatElementList,cl:Class[T])(f:F[R**StringBuilder,LR**T]):F[R**StringBuilder,LR**T] =
    elements.elements.foldLeft(f){(frame,element) => compileElement(element,cl)(frame)}

  def compileElement[R<:List,LR<:List,T<:java.lang.Object](ele:FormatElement,cl:Class[T])(f:F[R**StringBuilder,LR**T]):F[R**StringBuilder,LR**T]
    = ele match {
      case Literal(str) => 
        f ~ ldc(str) ~ method2(_.append(_))
      case ToStringConversion(e) =>
        f ~ local[_0,T].load() ~
          compileGetExp(e,cl,classOf[AnyRef]) ~ 
          method(_.toString) ~ 
          method2(_.append(_))
      case Expand(exp,sep,inner) => {
        val retType = exp.returnType(cl)

        if (classOf[java.lang.Iterable[_]].isAssignableFrom(retType)){
          val eleType:Class[AnyRef] = elementType(exp.genericReturnType(cl),classOf[java.lang.Iterable[_]]).asInstanceOf[Class[AnyRef]]
          
          f ~ local[_0,T].load() ~
            withLocal0Saved { f =>
	          val jmpTarget =
	            f ~ 
                 swap ~
                 dup_x1 ~
	             compileGetExp(exp,cl,classOf[java.lang.Iterable[AnyRef]]) ~
	             method(_.iterator) ~
	             local[_0,java.util.Iterator[AnyRef]].store() ~
	             target
	          
	          jmpTarget ~
	             local[_0,java.util.Iterator[AnyRef]].load() ~
	             method(_.hasNext) ~
	             ifeq(f =>
	               f ~
	                local[_0,java.util.Iterator[AnyRef]].load() ~
	                swap ~
	                local[_0,java.util.Iterator[AnyRef]].load() ~
	                method(_.next) ~
	                checkcast(eleType) ~
	                local[_0,AnyRef].store() ~
	                compileFormatElementList(inner,eleType) ~
	                swap ~
	                dup ~
	                local[_0,java.util.Iterator[AnyRef]].store() ~
	                method(_.hasNext) ~
	                ifeq(f =>
	                   f~ldc(sep:jString) ~
	                    method2(_.append(_)) ~
	                    jmp(jmpTarget)) ~ //todo: introduce ifeq(thenCode,elseTarget)
	                jmp(jmpTarget))
          }
        }
        else if (retType.isArray){
          val eleType:Class[AnyRef] = retType.getComponentType.asInstanceOf[Class[AnyRef]]

          if (eleType.isPrimitive)
            throw new java.lang.Error("can't handle primitive arrays right now");

          import Bytecode.RichOperations.foldArray
          
          def swapTopWithLocal0[S<:List,L<:List,ST,LT]:F[S**ST,L**LT] => F[S**LT,L**ST] = 
            _ ~
            local[_0,LT].load() ~
            swap ~
            local[_0,ST].store[S**LT,L**LT]()
          
          f ~
             local[_0,T].load() ~
             swap ~
             local[_0,T].load() ~
             compileGetExp(exp,cl,retType.asInstanceOf[Class[Array[AnyRef]]]) ~
             swap ~
             foldArray(// index,sb,ele | array
               _ ~ 
               swapTopWithLocal0 ~ // index,sb,array | ele
               swap ~ // index,array,sb
               compileFormatElementList(inner,eleType) ~ //index,array,sb | ele
               swap ~
               local[_0,Array[AnyRef]].store() ~
               // check if it was latest element or not so we can insert separator
               swap ~ // sb,index
               dup_x1 ~ // index,sb,index
               local[_0,Array[AnyRef]].load() ~ // index,sb,index,array
               arraylength ~ // index,sb,index,length
               bipush(1) ~ // index,sb,index,length,1
               isub ~ // index,sb,index,length-1
               isub ~ // index,sb,index-(length-1)
               ifeq2( // index,sb
                 f=>f, //FIXME: use id func here
                 ldc(sep) ~ method2(_.append(_)) // append separator if we are not at the end of the array
               )
             ) ~
             swap ~
             local[_0,T].store[R**StringBuilder,LR**Array[AnyRef]]()
        }
        else
          throw new java.lang.Error("can only iterate over iterables and arrays right now")
      }
      case Conditional(inner,thens,elses) => {
        val retType = inner.returnType(cl)

        if (retType == java.lang.Boolean.TYPE || classOf[java.lang.Boolean].isAssignableFrom(retType)){
          f ~ 
            local[_0,T].load() ~
            (if (retType == java.lang.Boolean.TYPE)
               compileGetExp(inner,cl,classOf[Boolean])
             else 
               compileGetExp(inner,cl,classOf[java.lang.Boolean]) _ ~ method(_.booleanValue)
            ) ~
            ifeq2(
              compileFormatElementList(elses,cl),
              compileFormatElementList(thens,cl))
        }
        else if (classOf[Option[AnyRef]].isAssignableFrom(retType)){
          val eleType = elementType(inner.genericReturnType(cl),classOf[Option[_]]).asInstanceOf[Class[AnyRef]]
          f ~
            local[_0,T].load() ~
            compileGetExp(inner,cl,classOf[Option[AnyRef]]) ~
            dup ~
            method(_.isDefined) ~
            ifeq2(
              _ ~ pop ~ compileFormatElementList(elses,cl),
              _ ~ 
                checkcast(classOf[Some[AnyRef]]) ~
                method(_.get) ~
                withLocal0Save(compileFormatElementList(thens,eleType)))
        }
        else
          throw new Error("can't use "+retType+" in a conditional")
      }
      case DateConversion(exp,format) => {
        val retType = exp.returnType(cl)
        
        val DateClass:Class[java.util.Date] = classOf[java.util.Date]
        val CalendarClass:Class[java.util.Calendar] = classOf[java.util.Calendar]
        
        f ~ newInstance(classOf[java.text.SimpleDateFormat]) ~
          dup ~
          ldc(format) ~ 
          method2(_.applyPattern(_)) ~ pop_unit ~
          local[_0,T].load() ~
          (f => 
            retType match {
              case x if DateClass.isAssignableFrom(x)     => f ~ 
                                                               compileGetExp(exp,cl,DateClass)
              case x if CalendarClass.isAssignableFrom(x) => f ~                
                                                               compileGetExp(exp,cl,CalendarClass) ~ 
                                                               method(_.getTime)
              case _ => throw new java.lang.Error("Expected date- or calendar- typed property. "+
                                                    cl+" can't be converted.") 
            }
          ) ~
          method2(_.format(_)) ~
          method2(_.append(_))
      }
    }
  def compile[T<:AnyRef](format:String,cl:Class[T]):T=>jString = {
    val elements:FormatElementList = parser.parse(format)
    ASMCompiler.compile(cl)(
      _ 
      ~ local[_0,T].store()
      ~ newInstance(classOf[StringBuilder])
      ~ compileFormatElementList(elements,cl)
      ~ method(_.toString)
     )
  }
}

object FormatCompiler extends IObjectFormatterFactory{
  def formatter[T<:AnyRef](clazz:Class[T],fmt:String) = new IObjectFormatter[T]{
    val compiler = Compiler.compile[T](fmt,clazz)
    def format(o:T):String = compiler(o)
  }
}