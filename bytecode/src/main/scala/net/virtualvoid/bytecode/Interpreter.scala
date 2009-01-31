package net.virtualvoid.bytecode

import Bytecode._

import java.lang.{String=>jString}

object Interpreter extends ByteletCompiler{
    case class IF[ST<:List,LT<:List](stack:ST,locals:LT) extends F[ST,LT]{
      import CodeTools._
      
      def notImplemented(what:String) = new java.lang.Error(what + " not implemented in Interpreter")
      
      def bipush(i1:Int):F[ST**Int,LT] = IF(stack ** i1,locals)
      def ldc(str:jString):F[ST**jString,LT] = IF(stack ** str,locals)
      def target:BackwardTarget[ST,LT] = throw notImplemented("target")
      def jmp(t:Target[ST,LT]):Nothing = throw notImplemented("jmp")
      
      def forwardTarget[ST<:List,LT<:List]:ForwardTarget[ST,LT] = throw notImplemented("forwardTarget")
      def targetHere(t:ForwardTarget[ST,LT]):F[ST,LT] = throw notImplemented("targetHere")

      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1+i2),locals)
      def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1-i2),locals)
      def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1*i2),locals)
      def pop_int[R<:List](rest:R):F[R,LT] = IF(rest,locals)
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT] = IF(rest**top**top,locals)
      def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2,LT] = IF(rest**t1**t2,locals)
      def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1,LT] = IF(rest**t1**t2**t1,locals)
      def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT] = 
        IF(rest ** invokeMethod(methodFromTree(code.tree),top).asInstanceOf[U],locals)
      def method_int[R<:List,T,U](rest:R,top:T,method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT] =
        IF(rest ** method.invoke(top).asInstanceOf[U],locals)
      def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT] = 
        IF(rest ** invokeMethod(methodFromCode(code),top2,top1).asInstanceOf[U],locals)
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT] = IF(rest**top.asInstanceOf[U],locals)
      def ifeq_int[R<:List](rest:R,top:JVMInt,inner:F[R,LT] => Nothing):F[R,LT] = throw notImplemented("ifeq_int")
      def ifeq2_int[R<:List,ST2<:List,LT2<:List](rest:R,top:JVMInt,then:F[R,LT]=>F[ST2,LT2],elseB:F[R,LT]=>F[ST2,LT2]):F[ST2,LT2] =
        if (top == 0) then(IF(rest,locals)) else elseB(IF(rest,locals))
      
      import java.lang.reflect.{Array => jArray}
      def aload_int[R<:List,T](rest:R,array:AnyRef,i:Int):F[R**T,LT] = {
        IF(rest**jArray.get(array,i).asInstanceOf[T],locals)
      }
      def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT] = {
        jArray.set(array,index,t)
        IF(rest,locals)
      }
      def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT] = 
        IF(rest**jArray.getLength(array),locals)
      
      def pop_unit_int[R<:List](rest:R):F[R,LT] = IF(rest,locals)      

      def get[T](i:Int,l:List):T = l match{
        case N => throw new Error("not possible")
        case Cons(r,t:T) => if (i == 0) t else get(i-1,r)
      }
      def store[T](i:Int,l:List,t:T):List = l match {
        case N => if (i == 0) Cons(N,t) else Cons(store(i-1,N,t),N)
        case Cons(r,old:T) => if (i == 0) Cons(r,t) else Cons(store(i-1,r,t),old)
      }

      def loadI[T](i:Int):F[ST**T,LT] = IF(stack**get(i,locals),locals)
      def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT] =
        IF(rest,store(i,locals,top).asInstanceOf[NewLT])
      
      def newInstance[T](cl:Class[T]):F[ST**T,LT] = 
        IF(stack**cl.newInstance,locals)
      
      def tailRecursive_int[ST2<:List,LT2<:List]
        (func: (F[ST,LT] => F[ST2,LT2]) => (F[ST,LT]=>F[ST2,LT2]))(fr:F[ST,LT]):F[ST2,LT2] =
          // classical y combinator in strict languages
          func(tailRecursive_int(func)_)(fr)
        
      def lazyVal_int[T<:AnyRef](tpe:Class[T],init:F[Nil,Nil] => F[Nil**T,Nil]):F[ST**T,LT] = throw new Error("not implemented")
    }

    def compile[T,U](cl:Class[T])(code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U =
      t => code(IF((N:Nil)**t,N)).stack.top
  }