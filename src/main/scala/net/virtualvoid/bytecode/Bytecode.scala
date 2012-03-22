package net.virtualvoid.bytecode

object Bytecode{
  import java.lang.{ String  => jString,
                     Boolean => jBoolean }

  
  case class JVMInt(v:Int){
    override def equals(o:Any) = v.equals(o)
  }
  
  object Instructions extends InstantiationInstructions 
                         with BranchingInstructions {
    def withLocal[T,ST<:Stack,ST2<:Stack](code:Local[T]=>F[ST]=>F[ST2]):F[ST**T]=>F[ST2] =
      f => f.withLocal_int[T,ST,ST2](f.stack.top,f.stack.rest,code)
    
    def iop[R<:Stack](func:(F[R**Int**Int],R,Int,Int)=>F[R**Int])
      :F[R**Int**Int] => F[R**Int] = f => 
      	func(f,f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def iadd[R<:Stack] =
      iop[R](_.iadd_int(_,_,_))
    def imul[R<:Stack] =
      iop[R](_.imul_int(_,_,_))
    def isub[R<:Stack] =
      iop[R](_.isub_int(_,_,_))
                                    
    def getstatic[R<:Stack,T](code:scala.reflect.Code[()=>T])
        :F[R] => F[R**T] =
        f => f.getstatic_int(code)
    def putstatic[R<:Stack,T](code:scala.reflect.Code[T=>Unit])
    	:F[R**T] => F[R] =
        f => f.putstatic_int(f.stack.rest,f.stack.top,code)
    
    def nop[R<:Stack]:F[R] => F[R] = f => f
    def pop[R <: Stack, T]: F[R**T] => F[R] = f => f.pop_int(f.stack.rest)
    def dup[R <: Stack, T]: F[R**T] => F[R**T**T] =
      f => f.dup_int(f.stack.rest,f.stack.top)
    def dup_x1[R<:Stack,T2,T1]:F[R**T2**T1] => F[R**T1**T2**T1] =
      f => f.dup_x1_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def swap[R <: Stack, T2 <% Category1, T1 <% Category1](): F[R**T2**T1] => F[R**T1**T2] =
      f => f.swap_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def checkcast[T<:AnyRef,U,R<:Stack](cl:Class[U]):F[R**T]=>F[R**U] =
      f => f.checkcast_int(f.stack.rest,f.stack.top)(cl)
    
    def bipush[R<:Stack](i:Int):F[R]=>F[R**Int] = _.bipush(i)
    def ldc[R<:Stack](str:String):F[R]=>F[R**String] = _.ldc(str)
    
    def aload[R<:Stack,T]:F[R**Array[T]**Int] => F[R**T] =
      f => f.aload_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def astore[R<:Stack,T]:F[R**Array[T]**Int**T] => F[R] =
      f => f.astore_int(f.stack.rest.rest.rest
                       ,f.stack.rest.rest.top
                       ,f.stack.rest.top
                       ,f.stack.top)
    def arraylength[R<:Stack,T]:F[R**Array[T]] => F[R**Int] =
      f => f.arraylength_int(f.stack.rest,f.stack.top)

    implicit def int2JVMInt(i:Int) = JVMInt(i)
    implicit def bool2JVMInt(b:Boolean) = JVMInt(if (b) 1 else 0)
           
    def newInstance[ST<:Stack,T](cl:Class[T]) =
      (f:F[ST]) => f.newInstance(cl)

    import org.objectweb.asm.Opcodes._
    def ifne2[R<:Stack,ST2<:Stack,T]
              (thenB:F[R]=>F[ST2]
              ,elseB:F[R]=>F[ST2])
              (implicit conv:T => JVMInt):F[R**T]=>F[ST2] =
      f => f.conditional(IFNE,f.stack.rest,conv(f.stack.top),thenB,elseB)
    def ifeq2[R<:Stack,ST2<:Stack,T]
              (thenB:F[R]=>F[ST2],elseB:F[R]=>F[ST2])
              (implicit conv:T => JVMInt)
              	:F[R**T]=>F[ST2] = 
      f => f.conditional(IFEQ,f.stack.rest,conv(f.stack.top),thenB,elseB)
               
    def ifnull[R<:Stack,ST2<:Stack,T<:AnyRef](thenB:F[R]=>F[ST2]
    									   ,elseB:F[R]=>F[ST2])
    									   :F[R**T]=>F[ST2] =
      f => f.conditional(IFNULL,f.stack.rest,f.stack.top,thenB,elseB)
                
    def ifnonnull[R<:Stack,ST2<:Stack,T<:AnyRef](thenB:F[R]=>F[ST2]
    									   ,elseB:F[R]=>F[ST2])
    									   :F[R**T]=>F[ST2] =
      f => f.conditional(IFNONNULL,f.stack.rest,f.stack.top,thenB,elseB)
      
    def iterate[ST<:Stack,ST2<:Stack]
      (func: (F[ST] => F[ST2]) => (F[ST]=>F[ST2]))
      	(fr:F[ST]):F[ST2] =
        fr.tailRecursive_int(func)(fr)

    def withTargetHere[ST<:Stack,X](func: Target[ST] => F[ST] => X):F[ST] => X =
      f => f.withTargetHere_int(func)

    def ifne[T<%JVMInt,R<:Stack](thenB:F[R] => Nothing):F[R**T] => F[R] =
      f => f.conditionalImperative(IFEQ,f.stack.rest,f.stack.top,thenB)
  }

  object Implicits{
    implicit def richFunc[ST1<:Stack,ST2<:Stack]
                          (func:F[ST1] => F[ST2]):RichFunc[ST1,ST2] = 
                            new RichFunc[ST1,ST2]{
      def apply(f:F[ST1]):F[ST2] = func(f)
    }
  }
  
  object RichOperations{
    import Instructions._
    import Implicits._
     /* def foldArray(array,func,start)
       * 	let f(i,u) = 
       *         if (i<array.length)
       * 			f(i+1,func(u,ar[i]))
       *         else
       *            u
       *    f(0,start)
      */
    def foldArray[R<:Stack,T,U]
      (array:LocalR[Array[T]])
      (func:Local[Int]=>F[R**U**T]=>F[R**U])
	  :F[R**U] => F[R**U] =
	    _ ~
		  bipush(0) ~
		  withLocal{ index =>
		    iterate[R**U,R**U]{self =>
		      _ ~
		      index.load ~
		      array.load ~
		      arraylength ~
		      isub ~
		      ifeq2(nop,
				_ ~
				array.load ~
				index.load ~
				aload ~
				func(index) ~
				index.load ~
				bipush(1) ~ 
				iadd ~
				index.store ~
				self
		      )
		    }
		  }
    
    def foldIterable[R <: Stack, T <: AnyRef: Manifest, U <% Category1]
        (iterable: LocalR[java.lang.Iterable[T]])
        (func: LocalR[java.util.Iterator[T]] => F[R**U**T] => F[R**U])
      : F[R**U] => F[R**U] = {
      _ ~
        iterable.load ~
        Methods.method((_: java.lang.Iterable[T]).iterator) ~
        withLocal { it => foldIterator(it)(func(it)) }
    }

    def foldIterator[R <: Stack, T <:AnyRef: Manifest, U <% Category1]
                    (iterator: LocalR[java.util.Iterator[T]])
                    (func: F[R**U**T] => F[R**U])
          : F[R**U] => F[R**U] = {
            import Methods.method
            val mf = implicitly[Manifest[T]]
            _ ~
                  iterate[R**U,R**U]( self =>
                    _ ~
	                  iterator.load ~
	                  method((_:java.util.Iterator[T]).hasNext).invoke() ~
	                  ifne2(
	                    _ ~
	                      iterator.load ~
	                      method((_:java.util.Iterator[T]).next).invoke() ~
	                      checkcast(mf.erasure.asInstanceOf[Class[T]]) ~
	                      func ~
	                      self
	                    ,f=>f
	                  )
                   )
          }
  }
  
  trait RichFunc[ST1<:Stack,ST2<:Stack]
      extends (F[ST1] => F[ST2]){ first =>
    def ~[ST3<:Stack](second:F[ST2]=>F[ST3])
    	:RichFunc[ST1,ST3] = new RichFunc[ST1,ST3]{
      def apply(f:F[ST1]):F[ST3] = second(first(f))
    }
  }
}
