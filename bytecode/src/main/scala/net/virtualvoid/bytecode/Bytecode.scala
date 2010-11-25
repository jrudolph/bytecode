package net.virtualvoid.bytecode

object Bytecode{
  import java.lang.{ String  => jString,
                     Boolean => jBoolean }

  
  case class JVMInt(v:Int){
    override def equals(o:Any) = v.equals(o)
  }
 
  /*
   * Define type and implicits for so-called Category 1 data types:
   * types which are of 32-bit size.
   * See §3.11.1 of the JVM specs
   * http://java.sun.com/docs/books/jvms/second_edition/html/Overview.doc.html#37906
   */
  trait Category1[-T]
  implicit val cat1Boolean  : Category1[Boolean] = null
  implicit val cat1Byte     : Category1[Byte] = null
  implicit val cat1Char     : Category1[Char] = null
  implicit val cat1Short    : Category1[Short] = null
  implicit val cat1Int      : Category1[Int] = null
  implicit val cat1Float    : Category1[Float] = null
  implicit def cat1AnyRef   : Category1[AnyRef] = null
  
  trait IsUnit[-T]
  trait NoUnit[-T]
  implicit val unitIsUnit  : IsUnit[Unit] = null
  implicit val anyrefNoUnit: NoUnit[AnyRef] = null
  implicit val boolNoUnit  : NoUnit[Boolean] = null
  implicit val byteNoUnit  : NoUnit[Byte] = null
  implicit val charNoUnit  : NoUnit[Char] = null
  implicit val shortNoUnit : NoUnit[Short] = null
  implicit val intNoUnit   : NoUnit[Int] = null
  implicit val floatNoUnit : NoUnit[Float] = null
  implicit val doubleNoUnit: NoUnit[Double] = null
  implicit val longNoUnit  : NoUnit[Long] = null
  
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
  
  implicit def normalCall1[R<:List, T, U: NoUnit](m:Method1[T,U]):F[R**T]=>F[R**U] = m.invoke()
  implicit def unitCall1[R<:List,T](m:Method1[T,Unit]):F[R**T]=>F[R] = m.invokeUnit()
  
  implicit def normalCall2[R <: List, T1, T2, U: NoUnit](m:Method2[T1,T2,U]):F[R**T1**T2]=>F[R**U] = m.invoke()
  implicit def unitCall2[R<:List,T1,T2](m:Method2[T1,T2,Unit]):F[R**T1**T2]=>F[R] = m.invokeUnit()
  
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
                                    
    def getstatic[R<:List,T](code:scala.reflect.Code[()=>T])
        :F[R] => F[R**T] =
        f => f.getstatic_int(code)
    def putstatic[R<:List,T](code:scala.reflect.Code[T=>Unit])
    	:F[R**T] => F[R] =
        f => f.putstatic_int(f.stack.rest,f.stack.top,code)
    
    def nop[R<:List]:F[R] => F[R] = f => f
    def pop[R <: List, T]: F[R**T] => F[R] = f => f.pop_int(f.stack.rest)
    def dup[R <: List, T]: F[R**T] => F[R**T**T] = 
      f => f.dup_int(f.stack.rest,f.stack.top)
    def dup_x1[R<:List,T2,T1]:F[R**T2**T1] => F[R**T1**T2**T1] = 
      f => f.dup_x1_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def swap[R <: List, T2: Category1, T1: Category1](): F[R**T2**T1] => F[R**T1**T2] = 
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

    def ifne[T<%JVMInt,R<:List](thenB:F[R] => Nothing):F[R**T] => F[R] =
      f => f.conditionalImperative(IFEQ,f.stack.rest,f.stack.top,thenB)
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
    def foldArray[R<:List,T,U]
      (array:Local[Array[T]])
      (func:Local[Int]=>F[R**U**T]=>F[R**U])
	  :F[R**U] => F[R**U] =
	    _ ~
		  bipush(0) ~
		  withLocal{ index =>
		    tailRecursive[R**U,R**U]{self =>
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
	    
    def foldIterator[R <: List, T <:AnyRef: Manifest, U: Category1]
                    (func: Local[java.util.Iterator[T]] => F[R**U**T] => F[R**U])
          : F[R**java.util.Iterator[T]**U] => F[R**U] = {
            val mf = implicitly[Manifest[T]]
            _ ~
              swap() ~
              withLocal( iterator =>
                  tailRecursive[R**U,R**U]( self =>
                    _ ~
	                  iterator.load ~
	                  method1((_:java.util.Iterator[T]).hasNext).invoke() ~
	                  ifne2(
	                    _ ~
	                      iterator.load ~
	                      method1((_:java.util.Iterator[T]).next).invoke() ~
	                      checkcast(mf.erasure.asInstanceOf[Class[T]]) ~
	                      func(iterator) ~
	                      self
	                    ,f=>f
	                  )
                   )
              )
          }
  }
  
  trait RichFunc[ST1<:List,ST2<:List] 
      extends (F[ST1] => F[ST2]){ first =>
    def ~[ST3<:List](second:F[ST2]=>F[ST3])
    	:RichFunc[ST1,ST3] = new RichFunc[ST1,ST3]{
      def apply(f:F[ST1]):F[ST3] = second(first(f))
    }
  }
}
