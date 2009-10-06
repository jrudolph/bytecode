package net.virtualvoid.bytecode

import Bytecode._

import java.lang.{String=>jString}

object Interpreter extends ByteletCompiler {
    case class IF[+ST<:List](stack:ST) extends F[ST]{
      import CodeTools._
      
      def notImplemented(what:String) = 
        new java.lang.Error(what + " not implemented in Interpreter")
      
      def bipush[ST2>:ST](i1:Int):F[ST2**Int] = IF(stack ** i1)
      def ldc[ST2>:ST](str:jString):F[ST2**jString] = IF(stack ** str)
      /*def target:BackwardTarget[ST,LT] = throw notImplemented("target")
      def jmp(t:Target[ST,LT]):Nothing = throw notImplemented("jmp")
      
      def forwardTarget[ST<:List,LT<:List]:ForwardTarget[ST,LT] = 
        throw notImplemented("forwardTarget")
      def targetHere(t:ForwardTarget[ST,LT]):F[ST,LT] = 
        throw notImplemented("targetHere")*/

      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int] = IF(rest ** (i1+i2))
      def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int] = IF(rest ** (i1-i2))
      def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int] = IF(rest ** (i1*i2))
      def pop_int[R<:List](rest:R):F[R] = IF(rest)
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T] = 
    	  IF(rest**top**top)
      def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2] = 
    	  IF(rest**t1**t2)
      def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1] = 
        IF(rest**t1**t2**t1)
      def method1_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U] = 
        IF(rest ** invokeMethod(methodFromTree(code.tree),top).asInstanceOf[U])
      def methodDyn_int[R<:List,T,U](rest:R
                                   ,top:T
                                   ,handle:MethodHandle)
                                   :F[R**U] =
        IF(rest**handle.method.invoke(top).asInstanceOf[U])                                     
      def method2_int[R<:List,T2,T1,U](rest:R
                                      ,top2:T2
                                      ,top1:T1
                                      ,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U] = 
        IF(rest ** invokeMethod(methodFromCode(code),top2,top1).asInstanceOf[U])
      
      def methodDyn_int[R<:List,T1,T2,U](rest:R
                                        ,p1:T1
                                        ,p2:T2
                                        ,handle:MethodHandle)
                                        :F[R**U] =
        IF(rest ** invokeMethod(handle.method,p1,p2).asInstanceOf[U])


      def getstatic_int[ST2>:ST,T](code:scala.reflect.Code[()=>T]):F[ST2**T] = 
        IF(stack ** fieldFromTree(code.tree).get(null).asInstanceOf[T])

      def putstatic_int[R<:List,T](rest:R,top:T,code:scala.reflect.Code[T=>Unit]):F[R] = {
        fieldFromTree(code.tree).set(null,top)
        IF(rest)
      }
      
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U] = 
        IF(rest**top.asInstanceOf[U])
      def ifne_int[R<:List](rest:R,top:JVMInt,inner:F[R] => Nothing):F[R] = 
        throw notImplemented("ifeq_int")
      def ifne2_int[R<:List,ST2<:List](rest:R
                                                 ,top:JVMInt
                                                 ,then:F[R]=>F[ST2]
                                                 ,elseB:F[R]=>F[ST2]):F[ST2] =
        if (top != 0) then(IF(rest)) else elseB(IF(rest))
      
      import org.objectweb.asm.Opcodes._
      def isConditionTrue(cond:Int,value:Any):Boolean = cond match {
        case IFNULL => value.asInstanceOf[AnyRef] eq null
        case IFNONNULL => value.asInstanceOf[AnyRef] ne null
        case IFNE => value != 0
        case IFEQ => value == 0
      }
      def conditional[R<:List,T,ST2<:List](cond:Int,rest:R,top:T
    	              					  ,thenB:F[R]=>F[ST2]
    									  ,elseB:F[R]=>F[ST2]):F[ST2] = 
        if (isConditionTrue(cond,top))
          thenB(IF(rest))
        else
          elseB(IF(rest))
      
      import java.lang.reflect.{Array => jArray}
      def aload_int[R<:List,T](rest:R,array:AnyRef,i:Int):F[R**T] = {
        IF(rest**jArray.get(array,i).asInstanceOf[T])
      }
      def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R] = {
        jArray.set(array,index,t)
        IF(rest)
      }
      def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int] = 
        IF(rest**jArray.getLength(array))
      
      def pop_unit_int[R<:List](rest:R):F[R] = IF(rest)      

      def get[T](i:Int,l:List):T = l match{
        case N => throw new Error("not possible")
        case Cons(r,t:T) => if (i == 0) t else get(i-1,r)
      }
      def store[T](i:Int,l:List,t:T):List = l match {
        case N => if (i == 0) Cons(N,t) else Cons(store(i-1,N,t),N)
        case Cons(r,old:T) => if (i == 0) Cons(r,t) else Cons(store(i-1,r,t),old)
      }
      
      def newInstance[T,ST2>:ST](cl:Class[T]):F[ST2**T] = 
        IF(stack**cl.newInstance)
      
      def tailRecursive_int[ST1>:ST<:List,ST2<:List]
        (func: (F[ST1] => F[ST2]) => (F[ST1]=>F[ST2]))
        	(fr:F[ST1]):F[ST2] =
          // classical y combinator in strict languages
          func(tailRecursive_int(func)_)(fr)
      
      def withLocal_int[T,ST<:List,ST2<:List](top:T,rest:ST,code:Local[T]=>F[ST]=>F[ST2]):F[ST2] =
        code(local(top))(IF(rest))
      
      def withTargetHere_int[X](code:Target[ST] => F[ST] => X):X = 
        code(new Target[ST]{
          def jmp[ST2>:ST<:List]:F[ST2] => Nothing = f => throw ResultException(code(this)(f.asInstanceOf[F[ST]]))
        })(this)
      
      case class ResultException(res:Any) extends RuntimeException
      def executeAndExtract(block: () => Nothing):Any = {
        try{
          block()
          throw new RuntimeException("Expected control-flow exception")
        }catch{
          case x:ResultException => x.res
        }
      }
      def conditionalImperative[R<:List,T,ST2<:List](cond:Int,rest:R,top:T
    											  ,thenB:F[R]=>Nothing):F[R] =
        if(!isConditionTrue(cond,top)) // this is called with inverted conditions
          executeAndExtract(() => thenB(IF(rest))).asInstanceOf[F[R]]
        else
          IF(rest)
    }

    def local[T](initial:T):Local[T] = new Local[T] {
      var value = initial
      def load[ST<:List,T2>:T]:F[ST] => F[ST**T2] = f => IF(f.stack**value)
      def store[ST<:List]:F[ST**T] => F[ST] = f => {
    	  value = f.stack.top
    	  IF(f.stack.rest)
      }
    } 
    
    def ret[U](func:F[Nil**U] => Nothing):Return[U] = new Return[U]{def jmp:F[Nil**U]=>Nothing = func}
    
    def compile[T<:AnyRef,U<:AnyRef](cl:Class[T],retCl:Class[U])(
                       code: Local[T] => Return[U] => F[Nil] => Nothing   
	  ):T=>U = t => realCompile(cl,retCl)(code)(t)
    
    def realCompile[T<:AnyRef,U<:AnyRef](cl:Class[T],retCl:Class[U])(
                       code: Local[T] => Return[U] => F[Nil] => Nothing   
	  )(t:T):U =
        code(local(t))(ret(f => return f.stack.top))(IF(N))
    
    def compile[T1<:AnyRef,T2<:AnyRef,U<:AnyRef](cl1:Class[T1],cl2:Class[T2],retCl:Class[U])(
	    code: (Local[T1],Local[T2]) => Return[U] => F[Nil] => Nothing
	  ):(T1,T2) => U = 
        (t1,t2) => realCompile(cl1,cl2,retCl)(code)(t1,t2)
    def realCompile[T1<:AnyRef,T2<:AnyRef,U<:AnyRef](cl1:Class[T1],cl2:Class[T2],retCl:Class[U])(
	    code: (Local[T1],Local[T2]) => Return[U] => F[Nil] => Nothing
	  )(t1:T1,t2:T2):U = 
        code(local(t1),local(t2))(ret(f => return f.stack.top))(IF(N))
}