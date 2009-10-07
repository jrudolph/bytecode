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
  implicit val cat1Boolean  : Boolean   => Category1 = null
  implicit val cat1Byte     : Byte      => Category1 = null
  implicit val cat1Character: Character => Category1 = null
  implicit val cat1Short    : Short     => Category1 = null
  implicit val cat1Int      : Int       => Category1 = null
  implicit val cat1Float    : Float     => Category1 = null
  implicit val cat1AnyRef   : AnyRef    => Category1 = null
  
  trait IsUnit
  trait NoUnit
  implicit val unitIsUnit  : Unit      => IsUnit = null
  implicit val anyrefNoUnit: AnyRef    => NoUnit = null
  implicit val boolNoUnit  : Boolean   => NoUnit = null
  implicit val byteNoUnit  : Byte      => NoUnit = null
  implicit val charNoUnit  : Character => NoUnit = null
  implicit val shortNoUnit : Short     => NoUnit = null
  implicit val intNoUnit   : Int       => NoUnit = null
  implicit val floatNoUnit : Float     => NoUnit = null
  implicit val doubleNoUnit: Double    => NoUnit = null
  implicit val longNoUnit  : Long      => NoUnit = null
  
  trait Target[ST<:List]{
    def jmp:F[ST] => Nothing
  }
  
  trait F[+ST<:List]{
    def depth = -1
    def frame = this
    
    def stack:ST

    def bipush[ST2>:ST<:List](i1:Int):F[ST2**Int]
    def ldc[ST2>:ST<:List](str:jString):F[ST2**jString]

    def ~[X](f:F[ST]=>X):X = f(this)
    
    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int]
    def pop_int[R<:List](rest:R):F[R]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T]
    def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2]
    def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1]

    def invokemethod[R<:List,U](handle:MethodHandle):F[R**U]
                                   
    def getstatic_int[ST2>:ST<:List,T](code:scala.reflect.Code[()=>T]):F[ST2**T]
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

    def newInstance[T,ST2>:ST<:List](cl:Class[T]):F[ST2**T]
    
    def withLocal_int[T,ST<:List,ST2<:List](top:T,rest:ST,code:Local[T]=>F[ST]=>F[ST2]):F[ST2]
    
    def withTargetHere_int[X,ST2>:ST<:List](code:Target[ST2] => F[ST2] => X):X
    def conditionalImperative[R<:List,T,ST2<:List](cond:Int,rest:R,top:T
    											  ,thenB:F[R]=>Nothing):F[R]
  }
  
  trait Local[T]{
    def load[ST<:List]:F[ST] => F[ST**T]
    def store[ST<:List]:F[ST**T] => F[ST]
  }

  import _root_.java.lang.reflect.Method
  import _root_.scala.reflect.Manifest

  abstract class MethodHandle(val method:Method){
    def numParams:Int
    
    protected def normalCall[X<:List,R<:List,U]:F[X]=>F[R**U] = _.invokemethod(this)
    protected def unitCall[X<:List,Y<:List]:F[X]=>F[Y] = { f => 
      val nextF = f.invokemethod(this)
      nextF.pop_unit_int(nextF.stack.rest)
    }
  }
  trait Method1[-T,+U] extends MethodHandle {
    override val numParams = 1
    def invoke[R<:List,T1X<:T,UX>:U <% NoUnit]():F[R**T1X] => F[R**UX] = normalCall
    def invokeUnit[R<:List,T1X<:T,UX>:U <% IsUnit]():F[R**T1X] => F[R] = unitCall
  }
  trait Method2[-T1,-T2,+U] extends MethodHandle {
    override val numParams = 2
    def invoke[R<:List,T1X<:T1,T2X<:T2,UX>:U <% NoUnit]():F[R**T1X**T2X] => F[R**UX] = normalCall
    def invokeUnit[R<:List,T1X<:T1,T2X<:T2,UX>:U <% IsUnit]():F[R**T1X**T2X] => F[R] = unitCall
  }
  
  implicit def normalCall1[R<:List,T,U <% NoUnit](m:Method1[T,U]):F[R**T]=>F[R**U] = m.invoke()
  implicit def unitCall1[R<:List,T](m:Method1[T,Unit]):F[R**T]=>F[R] = m.invokeUnit()
  
  implicit def normalCall2[R<:List,T1,T2,U <% NoUnit](m:Method2[T1,T2,U]):F[R**T1**T2]=>F[R**U] = m.invoke()
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
    checkMethod(m,r.erasure,p1.erasure)(new MethodHandle(_) with Method1[T,U])
  def dynMethod[T,U](m:Method,p1:Class[T],r:Class[U]):Method1[T,U] =
    dynMethod[T,U](m)(Manifest.classType(p1),Manifest.classType(r))
  
  def method1[T,U](code:scala.reflect.Code[T=>U]):Method1[T,U] =
    new MethodHandle(CodeTools.methodFromTree(code.tree)) with Method1[T,U]
  
  def dynMethod[T1,T2,U](m:Method)(implicit p1:Manifest[T1],p2:Manifest[T1],r:Manifest[U]):Method2[T1,T2,U] =
    checkMethod(m,r.erasure,p1.erasure,p2.erasure)(new MethodHandle(_) with Method2[T1,T2,U])
  def dynMethod[T1,T2,U](m:Method,p1:Class[T1],p2:Class[T2],r:Class[U]):Method2[T1,T2,U] =
    dynMethod[T1,T2,U](m)(Manifest.classType(p1),Manifest.classType(p2),Manifest.classType(r))
  
  def method2[T1,T2,U](code:scala.reflect.Code[(T1,T2)=>U]):Method2[T1,T2,U] = 
    new MethodHandle(CodeTools.methodFromCode(code)) with Method2[T1,T2,U]
  
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

    def withTargetHere[ST<:List,X](func:Target[ST] => F[ST] => X):F[ST] => X = 
      f => f.withTargetHere_int(func)
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
	    
    def foldIterator[R<:List,T<:AnyRef,U]
                    (func:Local[java.util.Iterator[T]]=>F[R**U**T]=>F[R**U])(implicit mf:scala.reflect.Manifest[T],cat1U:U=>Category1)
          :F[R**java.util.Iterator[T]**U] => F[R**U] =
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

  trait Return[T]{
    def jmp:F[Nil**T] => Nothing
  }
  
  trait ByteletCompiler{
	  def compile[T1<:AnyRef,R<:AnyRef](cl:Class[T1])(
                       code: Local[T1] => F[Nil] => F[Nil**R]   
	  )(implicit mf:scala.reflect.Manifest[R]): T1 => R = 
		compile(cl,mf.erasure.asInstanceOf[Class[R]])(p1 => ret => f => f ~ code(p1) ~ ret.jmp)
	  def compile[T1<:AnyRef,R<:AnyRef]
	    (par1Cl:Class[T1],retCl:Class[R])
	    (code: Local[T1] => Return[R] => F[Nil] => Nothing): T1 => R	  
     def compile[T1<:AnyRef,T2<:AnyRef,R<:AnyRef](cl1:Class[T1],cl2:Class[T2],retCl:Class[R])(
	    code: (Local[T1],Local[T2]) => Return[R] => F[Nil] => Nothing
	  ): (T1,T2) => R
  }
  trait RichFunc[ST1<:List,ST2<:List] 
      extends (F[ST1] => F[ST2]){ first =>
    def ~[ST3<:List](second:F[ST2]=>F[ST3])
    	:RichFunc[ST1,ST3] = new RichFunc[ST1,ST3]{
      def apply(f:F[ST1]):F[ST3] = second(first(f))
    }
  }
}