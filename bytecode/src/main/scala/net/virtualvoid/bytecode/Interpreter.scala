package net.virtualvoid.bytecode

import Bytecode._

import java.lang.{String=>jString}

object Interpreter extends ByteletCompiler {
    case class IF[+ST<:List](stack:ST) extends F[ST]{
      import CodeTools._
      
      def popN(n:Int):(scala.List[AnyRef],List) = {
        def inner(i:Int,cur:(scala.List[AnyRef],List)):(scala.List[AnyRef],List) =
          if (i > 0) {
            val Cons(rest,top:AnyRef) = cur._2
            inner(i - 1,(top :: cur._1,rest))
          }
          else
            cur
        inner(n,(scala.Nil,stack))
      }
      
      def notImplemented(what:String) = 
        new java.lang.Error(what + " not implemented in Interpreter")
      
      def bipush[ST2>:ST<:List](i1:Int):F[ST2**Int] = IF(stack ** i1)
      def ldc[ST2>:ST<:List](str:jString):F[ST2**jString] = IF(stack ** str)

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
      
      override def invokemethod[R<:List,U](handle:MethodHandle)
                                   :F[R**U] = {
        val (args,rest) = popN(handle.numParams)
        IF(rest.asInstanceOf[R] ** invokeMethod(handle.method,args:_*).asInstanceOf[U])
      }

      def getstatic_int[ST2>:ST<:List,T](code:scala.reflect.Code[()=>T]):F[ST2**T] = 
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

      def newInstance[T,ST2>:ST<:List](cl:Class[T]):F[ST2**T] = 
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
      def load[ST<:List]:F[ST] => F[ST**T] = f => IF(f.stack**value)
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