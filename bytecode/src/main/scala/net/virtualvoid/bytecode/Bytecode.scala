package net.virtualvoid.bytecode

object Bytecode{
  import java.lang.{String => jString,
                    Boolean => jBoolean
  }

  trait List
  trait Nil extends List
  object N extends Nil
  
  case class Cons[+R<:List,+T](rest:R,top:T) extends List
  
  // define an infix operator shortcut for the cons type
  type ** [x<:List,y] = Cons[x,y]

  // define the same for values
  trait Consable[T<:List]{
    def **[U](next:U): T**U
  }
  implicit def conser[T<:List](t:T) = new Consable[T]{
    def **[U](next:U): T**U = Cons(t,next)
  }
  
  case class JVMInt(v:Int){
    override def equals(o:Any) = v.equals(o)
  }
  
  /*
   * Define type and implicits for so-called Category 1 data types:
   * types which are of 32-bit size.
   * See §3.11.1 of the JVM specs
   * http://java.sun.com/docs/books/jvms/second_edition/html/Overview.doc.html#37906
   */
  trait Category1
  implicit val cat1Boolean:Boolean => Category1 = null
  implicit val cat1Byte:Byte => Category1 = null
  implicit val cat1Character:Character => Category1 = null
  implicit val cat1Short:Short => Category1 = null
  implicit val cat1Int:Int => Category1 = null
  implicit val cat1Float:Float => Category1 = null
  implicit val cat1AnyRef:AnyRef => Category1 = null
  
  trait F[+ST<:List]{
    def depth = -1
    def frame = this
    
    def stack:ST

    def bipush[ST2>:ST](i1:Int):F[ST2**Int]
    def ldc[ST2>:ST](str:jString):F[ST2**jString]

    def ~[X](f:F[ST]=>X):X = f(this)
    
    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def pop_int[R<:List](rest:R):F[R]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T]
    def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2]
    def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1]
    def method1_int[R<:List,T,U](rest:R
                                ,top:T
                                ,code:scala.reflect.Code[T=>U])
                                :F[R**U]
    def method1Dyn_int[R<:List,T,U](rest:R
                                   ,top:T
                                   ,method:java.lang.reflect.Method
                                   ,resCl:Class[U])
                                   :F[R**U]
    def method2_int[R<:List,T2,T1,U](rest:R
                                    ,top2:T2
                                    ,top1:T1
                                    ,code:scala.reflect.Code[(T2,T1)=>U])
                                    :F[R**U]
    def getstatic_int[ST2>:ST,T](code:scala.reflect.Code[()=>T]):F[ST2**T]
    def putstatic_int[R<:List,T](rest:R,top:T,code:scala.reflect.Code[T=>Unit]):F[R]
    
    def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U]
    def conditional[R<:List,T,ST2<:List](cond:Int,rest:R,top:T
    									,thenB:F[R]=>F[ST2]
    									,elseB:F[R]=>F[ST2]):F[ST2]
    def aload_int[R<:List,T](rest:R,array:AnyRef/*Array[T]*/,i:Int):F[R**T]
    def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R]
    def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int]
    
    def tailRecursive_int[ST1>:ST<:List,ST2<:List]
        (func: (F[ST1] => F[ST2]) => (F[ST1]=>F[ST2]))
        	(fr:F[ST1]):F[ST2]
    
    def pop_unit_int[R<:List](rest:R):F[R]

    def newInstance[T,ST2>:ST](cl:Class[T]):F[ST2**T]
    
    def withLocal_int[T,ST<:List,ST2<:List](top:T,rest:ST,code:Local[T]=>F[ST]=>F[ST2]):F[ST2]
  }
  
  trait Local[T]{
    def load[ST<:List]:F[ST] => F[ST**T]
    def store[ST<:List]:F[ST**T] => F[ST]
  }
  
  object Instructions {
    def withLocal[T,ST<:List,ST2<:List](code:Local[T]=>F[ST]=>F[ST2]):F[ST**T]=>F[ST2] = 
      f => f.withLocal_int[T,ST,ST2](f.stack.top,f.stack.rest,code)
    
    def iop[R<:List](func:(F[R**Int**Int],R,Int,Int)=>F[R**Int])
      :F[R**Int**Int] => F[R**Int] = f => 
      	func(f,f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def iadd[R<:List] = 
      iop[R](_.iadd_int(_,_,_))
    def imul[R<:List] = 
      iop[R](_.imul_int(_,_,_))
    def isub[R<:List] = 
      iop[R](_.isub_int(_,_,_))
    
    def invokemethod1[T,U,R<:List](code:scala.reflect.Code[T=>U])
    	:F[R**T] => F[R**U] = 
      f => f.method1_int(f.stack.rest,f.stack.top,code)
    def invokemethod2[T1,T2,U,R<:List](code:scala.reflect.Code[(T1,T2)=>U])
      :F[R**T1**T2] => F[R**U] = 
    	  f => f.method2_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top,code)
    def invokemethod1Dyn[T,U,R<:List](method:java.lang.reflect.Method
                                               ,resT:Class[U])
                                               :F[R**T] => F[R**U] = 
        f => f.method1Dyn_int(f.stack.rest,f.stack.top,method,resT)
                                               
    def getstatic[R<:List,T](code:scala.reflect.Code[()=>T])
        :F[R] => F[R**T] =
        f => f.getstatic_int(code)
    def putstatic[R<:List,T](code:scala.reflect.Code[T=>Unit])
    	:F[R**T] => F[R] =
        f => f.putstatic_int(f.stack.rest,f.stack.top,code)
    
    def pop_unit[R<:List]:F[R**Unit] => F[R] =
      f => f.pop_unit_int(f.stack.rest)
    
    def pop[R<:List,T]:F[R**T]=>F[R] = f=>f.pop_int(f.stack.rest)
    def dup[R<:List,T]:F[R**T]=>F[R**T**T] = 
      f => f.dup_int(f.stack.rest,f.stack.top)
    def dup_x1[R<:List,T2,T1]:F[R**T2**T1] => F[R**T1**T2**T1] = 
      f => f.dup_x1_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def swap[R<:List,T2<%Category1,T1<%Category1]():F[R**T2**T1] => F[R**T1**T2] = 
      f => f.swap_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def checkcast[T,U,R<:List](cl:Class[U]):F[R**T]=>F[R**U] = 
      f => f.checkcast_int(f.stack.rest,f.stack.top)(cl)
    
    def bipush[R<:List](i:Int):F[R]=>F[R**Int] = _.bipush(i)
    def ldc[R<:List](str:String):F[R]=>F[R**String] = _.ldc(str)
    
    def aload[R<:List,T]:F[R**Array[T]**Int] => F[R**T] = 
      f => f.aload_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def astore[R<:List,T]:F[R**Array[T]**Int**T] => F[R] = 
      f => f.astore_int(f.stack.rest.rest.rest
                       ,f.stack.rest.rest.top
                       ,f.stack.rest.top
                       ,f.stack.top)
    def arraylength[R<:List,T]:F[R**Array[T]] => F[R**Int] =
      f => f.arraylength_int(f.stack.rest,f.stack.top)

    implicit def int2JVMInt(i:Int) = JVMInt(i)
    implicit def bool2JVMInt(b:Boolean) = JVMInt(if (b) 1 else 0)
           
    def newInstance[ST<:List,T](cl:Class[T]) = 
      (f:F[ST]) => f.newInstance(cl)

    import org.objectweb.asm.Opcodes._
    def ifne2[R<:List,ST2<:List,T]
              (thenB:F[R]=>F[ST2]
              ,elseB:F[R]=>F[ST2])
              (implicit conv:T => JVMInt):F[R**T]=>F[ST2] =
      f => f.conditional(IFNE,f.stack.rest,conv(f.stack.top),thenB,elseB)
    def ifeq2[R<:List,ST2<:List,T]
              (thenB:F[R]=>F[ST2],elseB:F[R]=>F[ST2])
              (implicit conv:T => JVMInt)
              	:F[R**T]=>F[ST2] = 
      f => f.conditional(IFEQ,f.stack.rest,conv(f.stack.top),thenB,elseB)
               
    def ifnull[R<:List,ST2<:List,T<:AnyRef](thenB:F[R]=>F[ST2]
    									   ,elseB:F[R]=>F[ST2])
    									   :F[R**T]=>F[ST2] =
      f => f.conditional(IFNULL,f.stack.rest,f.stack.top,thenB,elseB)
                
    def ifnonnull[R<:List,ST2<:List,T<:AnyRef](thenB:F[R]=>F[ST2]
    									   ,elseB:F[R]=>F[ST2])
    									   :F[R**T]=>F[ST2] =
      f => f.conditional(IFNONNULL,f.stack.rest,f.stack.top,thenB,elseB)
      
    def tailRecursive[ST<:List,ST2<:List]
      (func: (F[ST] => F[ST2]) => (F[ST]=>F[ST2]))
      	(fr:F[ST]):F[ST2] =
        fr.tailRecursive_int(func)(fr)
  }

  object Implicits{
    implicit def richFunc[ST1<:List,ST2<:List]
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
    def foldArray[R<:List,T,U<%Category1]
	                (func:Local[Int]=>F[R**U**T]=>F[R**U])
	                :F[R**Array[T]**U] => F[R**U] =
	    _ ~
	    swap() ~ 
        withLocal{ array =>
		  bipush(0) ~
		  withLocal{ index =>
		    tailRecursive[R**U,R**U]{self =>
		      _ ~
		      index.load ~
		      array.load ~
		      arraylength ~
		      isub ~
		      ifeq2(f=>f,
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
	    }
    def foldIterator[R<:List,T,U]
                    (func:Local[java.util.Iterator[T]]=>F[R**U**T]=>F[R**U])(implicit mf:scala.reflect.Manifest[T],cat1U:U=>Category1)
          :F[R**java.util.Iterator[T]**U] => F[R**U] =
            _ ~
              swap() ~
              withLocal( iterator =>
                  tailRecursive[R**U,R**U]( self =>
                    _ ~
	                  iterator.load ~
	                  invokemethod1(_.hasNext) ~
	                  ifne2(
	                    _ ~
	                      iterator.load ~
	                      invokemethod1(_.next) ~
	                      checkcast(mf.erasure.asInstanceOf[Class[T]]) ~
	                      func(iterator) ~
	                      self
	                    ,f=>f
	                  )
                   )
              )
  }

  trait Return[U<:AnyRef]{
    def jmp:F[Nil**U] => Nothing
  }

  trait ByteletCompiler{
	  def compileWithReturn[T<:AnyRef,U<:AnyRef](cl:Class[T])(
                       code: Return[U] =>
	                      	 F[Nil**T] => F[Nil**U]   
	  ): T => U

	  // default implementation: ignore return target
	  def compile[T<:AnyRef,U<:AnyRef](cl:Class[T])(
                       code: F[Nil**T] 
	                      => F[Nil**U]   
	  ): T => U = compileWithReturn(cl)((x:Return[U]) => code)
  }

  trait RichFunc[ST1<:List,ST2<:List] 
      extends (F[ST1] => F[ST2]){ first =>
    def ~[ST3<:List](second:F[ST2]=>F[ST3])
    	:RichFunc[ST1,ST3] = new RichFunc[ST1,ST3]{
      def apply(f:F[ST1]):F[ST3] = second(first(f))
    }
  }
}

abstract class AbstractFunction1[T,U] extends Function1[T,U]