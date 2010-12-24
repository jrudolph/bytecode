package net.virtualvoid.string

import java.lang.{StringBuilder,String=>jString}

object Compiler{
  import net.virtualvoid.bytecode._
  import net.virtualvoid.bytecode.backend.ASM
  import Bytecode._
  import Bytecode.Instructions._
  import Bytecode.Implicits._

  val parser = EnhancedStringFormatParser
  import AST._
  
  def elementType(it:java.lang.reflect.Type,of:Class[_])
  	:Class[_ <: AnyRef] = {
    TypeHelper.genericInstanceType(it,of,Array()) match{
      case Some(cl:java.lang.Class[AnyRef]) => cl
      case _ => throw new java.lang.Error("Can't get element type of "+it)
    }
  }

  def compileGetExp[R<:List,LR<:List,T,Ret: NoUnit](exp:Exp
                                            ,cl:Class[T]
                                            ,retType:Class[Ret])
                                           (f:F[R**T]):F[R**Ret] = 
  exp match {
    case p@ParentExp(inner,parent) =>{
      val m = Methods.dynMethod[T,Object](p.method(cl),cl,classOf[Object])
      f ~ 
        m ~ 
        compileGetExp(inner,m.method.getReturnType.asInstanceOf[Class[Object]],retType)
    }
    case ThisExp =>
      f ~ 
        checkcast(retType) // TODO: don't know why we need this, examine it
    case e:Exp => {
      val m = Methods.dynMethod[T,Ret](e.method(cl),cl,retType)
      f ~ m
    }
  }
    
  def compileFormatElementList[R<:List,LR<:List,T<:java.lang.Object]
                 (elements:FormatElementList,cl:Class[T],value:Local[T])
                 (f:F[R**StringBuilder]):F[R**StringBuilder] =
    elements.elements.foldLeft(f){(frame,element) => 
      compileElement(element,cl,value)(frame)}

  def id[X]:X=>X = x=>x

  lazy val appendM = Methods.method2[StringBuilder, String, StringBuilder](_.append(_))
  lazy val toStringM = Methods.method1[AnyRef, String](_.toString)
  
  def compileElement[R<:List,LR<:List,T<:java.lang.Object]
                     (ele:FormatElement,cl:Class[T],value:Local[T])
                     (f:F[R**StringBuilder])
                     :F[R**StringBuilder]
    = ele match {
      case Literal(str) => 
        f ~ ldc(str) ~ appendM
      case ToStringConversion(e) =>
        f ~ value.load ~
          compileGetExp(e,cl,classOf[AnyRef]) ~ 
          toStringM ~ 
          appendM
      case Expand(exp,sep,inner) => {
        import Bytecode.RichOperations.foldIterator
          
        val retType = exp.returnType(cl)

        if (classOf[java.lang.Iterable[_]].isAssignableFrom(retType)){
          val eleType:Class[AnyRef] = elementType(exp.genericReturnType(cl)
                                                  ,classOf[java.lang.Iterable[_]])
                                                  .asInstanceOf[Class[AnyRef]]
          
          f ~
            value.load ~
            compileGetExp(exp,cl,classOf[java.lang.Iterable[AnyRef]]) ~
            Methods.method1((_: java.lang.Iterable[AnyRef]).iterator) ~
            swap() ~
            foldIterator[R,AnyRef,StringBuilder](
              it => 
              	_ ~ withLocal(innerValue => compileFormatElementList(inner,eleType,innerValue)) ~
              	  it.load ~
              	  Methods.method1((_: java.util.Iterator[AnyRef]).hasNext) ~
              	  ifne2(
              	  	_ ~
              	  	  ldc(sep) ~
              	  	  appendM
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
                          appendM
                        , f=>f)))	              
            )
        }
        else
          throw new java.lang.Error("can only iterate over "+
                                      "iterables and arrays right now")
      }
      case Conditional(inner,thens,elses) => {
        val retType = inner.returnType(cl)

        if (retType == java.lang.Boolean.TYPE || 
              classOf[java.lang.Boolean].isAssignableFrom(retType)){
          f ~ 
            value.load ~
            (if (retType == java.lang.Boolean.TYPE)
               compileGetExp(inner,cl,classOf[Boolean])
             else 
               _ ~ compileGetExp(inner,cl,classOf[java.lang.Boolean]) 
                 ~ Methods.method1((_: java.lang.Boolean).booleanValue)
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
            Methods.method1((_: Option[AnyRef]).isDefined) ~
            ifeq2(
              _ ~ pop ~ compileFormatElementList(elses,cl,value),
              _ ~ 
                checkcast(classOf[Some[AnyRef]]) ~
                Methods.method1((_: Some[AnyRef]).get) ~
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
          Methods.method2((_: java.text.SimpleDateFormat).applyPattern(_: String)) ~
          value.load ~
          (f => 
            retType match {
              case x if DateClass.isAssignableFrom(x)     => 
                f ~ compileGetExp(exp,cl,DateClass)
              case x if CalendarClass.isAssignableFrom(x) => 
                f ~                
                  compileGetExp(exp,cl,CalendarClass) ~ 
                  Methods.method1((_: java.util.Calendar).getTime)
              case _ => throw new java.lang.Error(
                "Expected date- or calendar- typed property. "+
                cl+" can't be converted.") 
            }
          ) ~
          Methods.method2((_: java.text.SimpleDateFormat).format(_: java.util.Date)) ~
          appendM
      }
    }
  def compile[T<:AnyRef](format:String,cl:Class[T]):T=>jString = {
    val elements:FormatElementList = parser.parse(format)
    ASM.compile(cl)(value =>
      _ ~ 
        newInstance(classOf[StringBuilder]) ~
        compileFormatElementList(elements,cl,value) ~
        toStringM
    )
  }
}

object FormatCompiler extends IObjectFormatterFactory {
  def formatter[T<:AnyRef](clazz:Class[T],fmt:String) = Compiler.compile[T](fmt,clazz)
}
