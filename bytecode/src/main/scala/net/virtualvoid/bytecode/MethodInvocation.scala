package net.virtualvoid.bytecode

import _root_.java.lang.reflect.Method
import _root_.scala.reflect.Manifest

abstract class AbstractMethodHandle(val method:Method) extends MethodHandle {
  def numParams:Int
    
  protected def normalCall[X<:List,R<:List,U]:F[X]=>F[R**U] = _.invokemethod(this)
  protected def unitCall[X<:List,Y<:List]:F[X]=>F[Y] = { f => 
    val nextF = f.invokemethod(this)
    nextF.pop_unit_int(nextF.stack.rest)
  }
}
trait Method1[-T,+U] extends MethodHandle {
  override val numParams = 1
  def invoke[R <: List, T1X <: T,UX >: U: NoUnit]():F[R**T1X] => F[R**UX] = normalCall
  def invokeUnit[R <: List, T1X <: T]()(implicit x: IsUnit[U]):F[R**T1X] => F[R] = unitCall
}
trait Method2[-T1,-T2,+U] extends MethodHandle {
  override val numParams = 2
  def invoke[R <: List, T1X <: T1, T2X <: T2,UX >: U: NoUnit]():F[R**T1X**T2X] => F[R**UX] = normalCall
  def invokeUnit[R<:List,T1X<:T1,T2X<:T2]()(implicit x: IsUnit[U]):F[R**T1X**T2X] => F[R] = unitCall
}

object Methods {
  /** checks type information and returns a statically and dynamically safe handle
  */
  def dynMethod[T,U](m:Method)(implicit p1:Manifest[T],r:Manifest[U]):Method1[T,U] =
    checkMethod(m,r.erasure,p1.erasure)(new AbstractMethodHandle(_) with Method1[T,U])
  def dynMethod[T,U](m:Method,p1:Class[T],r:Class[U]):Method1[T,U] =
    dynMethod[T,U](m)(Manifest.classType(p1),Manifest.classType(r))
  
  def method1[T,U](code:scala.reflect.Code[T=>U]):Method1[T,U] =
    new AbstractMethodHandle(CodeTools.methodFromTree(code.tree)) with Method1[T,U]
  
  def dynMethod[T1,T2,U](m:Method)(implicit p1:Manifest[T1],p2:Manifest[T1],r:Manifest[U]):Method2[T1,T2,U] =
    checkMethod(m,r.erasure,p1.erasure,p2.erasure)(new AbstractMethodHandle(_) with Method2[T1,T2,U])
  def dynMethod[T1,T2,U](m:Method,p1:Class[T1],p2:Class[T2],r:Class[U]):Method2[T1,T2,U] =
    dynMethod[T1,T2,U](m)(Manifest.classType(p1),Manifest.classType(p2),Manifest.classType(r))
  
  def method2[T1,T2,U](code:scala.reflect.Code[(T1,T2)=>U]):Method2[T1,T2,U] = 
    new AbstractMethodHandle(CodeTools.methodFromCode(code)) with Method2[T1,T2,U]

  private def checkMethod[X](m:Method,retClazz:Class[_],paramClasses:Class[_]*)(f:Method=>X):X = {
    val params = if (CodeTools.static_?(m)) m.getParameterTypes() else (Array(m.getDeclaringClass) ++ m.getParameterTypes)
      
    def check(assertMsg:String)(condition:Boolean) = {
      if (!condition)
        throw new RuntimeException(assertMsg+"("+m.toString+")")
    }
  
    check("Method must have exactly "+paramClasses.length+" parameter")(params.length == paramClasses.length)
    check("Method's return type must be a subtype of "+retClazz)(retClazz.isAssignableFrom(m.getReturnType))
    for (i <- 0 until paramClasses.length)
      check("Method's "+i+". parameter must be a supertype of "+paramClasses(i))(
        params(i).isAssignableFrom(paramClasses(i)))
  
    f(m)
  }
}

trait MethodImplicits {
  implicit def normalCall1[R<:List, T, U: NoUnit](m:Method1[T,U]):F[R**T]=>F[R**U] = m.invoke()
  implicit def unitCall1[R<:List,T](m:Method1[T,Unit]):F[R**T]=>F[R] = m.invokeUnit()
  
  implicit def normalCall2[R <: List, T1, T2, U: NoUnit](m:Method2[T1,T2,U]):F[R**T1**T2]=>F[R**U] = m.invoke()
  implicit def unitCall2[R<:List,T1,T2](m:Method2[T1,T2,Unit]):F[R**T1**T2]=>F[R] = m.invokeUnit()
}
