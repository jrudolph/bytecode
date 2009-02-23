package net.virtualvoid.bytecode

object Bytecode{
  import java.lang.{String => jString,
                    Boolean => jBoolean
  }
  
  trait NatVisitor{
    type ResultType
    type Visit0 <: ResultType
    type VisitSucc[P<:Nat] <: ResultType
  }
  
  /* Peano-like natural numbers types */  
  trait Nat{
    type Accept[V<:NatVisitor] <: V#ResultType
  }
  final class _0 extends Nat{
    type Accept[V<:NatVisitor] = V#Visit0
  }  
  final class Succ[Pre<:Nat] extends Nat{
    type Accept[V<:NatVisitor] = V#VisitSucc[Pre]
  }
  
  type _1 = Succ[_0]
  type _2 = Succ[_1]
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  
  val _0 = new _0
  val _1 = new _1
  val _2 = new _2
  val _3 = new _3  

  trait List{
    type Rest <: List
    type Top
  }
  trait Nil extends List{
    type Rest = Nil
    type Top = Nothing
  }
  object N extends Nil
  
  case class Cons[R<:List,T](rest:R,top:T) extends List{
    type Rest = R
    type Top = T
    def l = rest
    /*def rest2:Rest = rest
    def top2:Top = top*/
  }
  // define an infix operator shortcut for the cons type
  type ** [x<:List,y] = Cons[x,y]

  // define the same for values
  trait Consable[T<:List]{
    def **[U](next:U): T**U
  }
  implicit def conser[T<:List](t:T) = new Consable[T]{
    def **[U](next:U): T**U = Cons(t,next)
  }

  trait Target[ST<:List,LT<:List]
  trait BackwardTarget[ST<:List,LT<:List] extends F[ST,LT] with Target[ST,LT] 
  trait ForwardTarget[ST<:List,LT<:List] extends Target[ST,LT]
  
  case class JVMInt(v:Int){
    override def equals(o:Any) = v.equals(o)
  }

  trait Zippable[ST<:List,L<:List,Cur,R<:List]{
    def depth:Int
    def frame:F[ST,_<:List]
  }
  
  case class Zipper[ST<:List,L<:List,Cur,R<:List](f:F[ST,_<:List],depth:Int) extends Zippable[ST,L,Cur,R]{
    def ~[X](f:Zipper[ST,L,Cur,R]=>X) = f(this)
    def frame = f
  }
  
  trait F[ST<:List,LT<:List]{
    def depth = -1
    def frame = this
    
    def stack:ST
    def locals:LT

    def bipush(i1:Int):F[ST**Int,LT]
    def ldc(str:jString):F[ST**jString,LT]
    def target:BackwardTarget[ST,LT]
    def jmp(t:Target[ST,LT]):Nothing
    
    // support for forward declaring targets
    def forwardTarget[ST<:List,LT<:List]:ForwardTarget[ST,LT]
    def targetHere(t:ForwardTarget[ST,LT]):F[ST,LT]

    def ~[X](f:F[ST,LT]=>X):X = f(this)
    
    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def pop_int[R<:List](rest:R):F[R,LT]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT]
    def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2,LT]
    def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1,LT]
    def method1_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def method1Dyn_int[R<:List,T,U](rest:R,top:T,method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT]
    def method2_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT]
    def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT]
    def ifne_int[R<:List](rest:R,top:JVMInt,inner:F[R,LT] => Nothing):F[R,LT]
    def ifeq2_int[R<:List,ST2<:List,LT2<:List](rest:R,top:JVMInt,then:F[R,LT]=>F[ST2,LT2],elseB:F[R,LT]=>F[ST2,LT2]):F[ST2,LT2]
    def aload_int[R<:List,T](rest:R,array:AnyRef/*Array[T]*/,i:Int):F[R**T,LT]
    def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT]
    def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT]
    
    def tailRecursive_int[ST2<:List,LT2<:List]
        (func: (F[ST,LT] => F[ST2,LT2]) => (F[ST,LT]=>F[ST2,LT2]))(fr:F[ST,LT]):F[ST2,LT2]
    
    def pop_unit_int[R<:List](rest:R):F[R,LT]

    def newInstance[T](cl:Class[T]):F[ST**T,LT]
    
    def loadI[T](i:Int):F[ST**T,LT]
    def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT]
  }
  
  case class CheckNTh[N<:Nat,L<:List,T]
  implicit def nth_0[R<:List,T,U<:T]:CheckNTh[_0,R**U,T] = null
  implicit def nthSucc[P<:Nat,R<:List,T,U](implicit next:CheckNTh[P,R,T]):CheckNTh[Succ[P],R**U,T] = null
  
  case class Depth[P<:Nat](depth:Int)
  implicit def depth_0:Depth[_0] = Depth[_0](0)
  implicit def depthSucc[P<:Nat](implicit next:Depth[P]):Depth[Succ[P]] = Depth[Succ[P]](next.depth + 1)
  
  /* it would be nice if we could abandon the () in declaration and application of 
   * load/store altogether but that doesn't seems to work since then
   * type and implicit infering won't work any more
   */
  trait LocalAccess[N<:Nat,T]{
    def load[ST<:List,LT<:List]()(implicit fn:CheckNTh[N,LT,T],depth:Depth[N]):F[ST,LT] => F[ST**T,LT]
    def store[ST<:List,LT<:List]()(implicit fn:Depth[N]):F[ST**T,LT] => F[ST,ReplaceNTh[N,LT,T]]
  }
     
  final class ReplaceNThVisitor[R<:List,T] extends NatVisitor{
    type ResultType = List
    type Visit0 = Cons[R#Rest,T]
    type VisitSucc[P<:Nat] = Cons[P#Accept[ReplaceNThVisitor[R#Rest,T]],R#Top]
  }
  type ReplaceNTh[N<:Nat,R<:List,T] = N#Accept[ReplaceNThVisitor[R,T]]
  
  object Instructions {
    def local[N<:Nat,T]:LocalAccess[N,T] = new LocalAccess[N,T]{
      def load[ST<:List,LT<:List]()(implicit check:CheckNTh[N,LT,T],depth:Depth[N]):F[ST,LT] => F[ST**T,LT] = 
        f => f.loadI(depth.depth)
      def store[ST<:List,LT<:List]()(implicit depth:Depth[N]):F[ST**T,LT] => F[ST,ReplaceNTh[N,LT,T]] = 
        f => f.storeI(f.stack.rest,f.stack.top,depth.depth)
    }
    
    def iop[R<:List,LT<:List](func:(F[R**Int**Int,LT],R,Int,Int)=>F[R**Int,LT]):
      F[R**Int**Int,LT] => F[R**Int,LT] = f => func(f,f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def iadd[R<:List,LT<:List] = 
      iop[R,LT](_.iadd_int(_,_,_))
    def imul[R<:List,LT<:List] = 
      iop[R,LT](_.imul_int(_,_,_))
    def isub[R<:List,LT<:List] = 
      iop[R,LT](_.isub_int(_,_,_))
    
    def invokemethod1[T,U,R<:List,LT<:List](code:scala.reflect.Code[T=>U]):F[R**T,LT] => F[R**U,LT] = 
      f => f.method1_int(f.stack.rest,f.stack.top,code)
    def invokemethod2[T1,T2,U,R<:List,LT<:List](code:scala.reflect.Code[(T1,T2)=>U]):
      F[R**T1**T2,LT] => F[R**U,LT] = f => f.method2_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top,code)
    def invokemethod1Dyn[T,U,R<:List,LT<:List](method:java.lang.reflect.Method,resT:Class[U]):
      F[R**T,LT] => F[R**U,LT] = f => f.method1Dyn_int(f.stack.rest,f.stack.top,method,resT)
    
    def pop_unit[R<:List,LT<:List]:F[R**Unit,LT] => F[R,LT] =
      f => f.pop_unit_int(f.stack.rest)
    
    def pop[R<:List,LT<:List,T]:F[R**T,LT]=>F[R,LT] = f=>f.pop_int(f.stack.rest)
    def dup[R<:List,LT<:List,T]:F[R**T,LT]=>F[R**T**T,LT] = f => f.dup_int(f.stack.rest,f.stack.top)
    def dup_x1[R<:List,LT<:List,T2,T1]:F[R**T2**T1,LT] => F[R**T1**T2**T1,LT] = f => f.dup_x1_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def swap[R<:List,LT<:List,T2,T1]:F[R**T2**T1,LT] => F[R**T1**T2,LT] = f => f.swap_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def checkcast[T,U,R<:List,LT<:List](cl:Class[U]):F[R**T,LT]=>F[R**U,LT] = f => f.checkcast_int(f.stack.rest,f.stack.top)(cl)
    
    def bipush[R<:List,LT<:List](i:Int):F[R,LT]=>F[R**Int,LT] = _.bipush(i)
    def ldc[R<:List,LT<:List](str:String):F[R,LT]=>F[R**String,LT] = _.ldc(str)
    
    def aload[R<:List,LT<:List,T]:F[R**Array[T]**Int,LT] => F[R**T,LT] = f=>f.aload_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def astore[R<:List,LT<:List,T]:F[R**Array[T]**Int**T,LT] => F[R,LT] = f=>f.astore_int(f.stack.rest.rest.rest,f.stack.rest.rest.top,f.stack.rest.top,f.stack.top)
    def arraylength[R<:List,LT<:List,T]:F[R**Array[T],LT] => F[R**Int,LT] = f=>f.arraylength_int(f.stack.rest,f.stack.top)

    implicit def int2JVMInt(i:Int) = JVMInt(i)
    implicit def bool2JVMInt(b:Boolean) = JVMInt(if (b) 1 else 0)
    
    def ifne[R<:List,LT<:List,T<%JVMInt](inner:F[R,LT]=>Nothing):F[R**T,LT] => F[R,LT] = f=>f.ifne_int(f.stack.rest,f.stack.top,inner)
    def target[ST<:List,LT<:List] = (f:F[ST,LT]) => f.target
    def targetHere[ST<:List,LT<:List](t:ForwardTarget[ST,LT]) = (f:F[ST,LT]) => f.targetHere(t)
    def jmp[ST<:List,LT<:List](t:Target[ST,LT]) = (f:F[ST,LT]) => f.jmp(t)
       
    def newInstance[ST<:List,LT<:List,T](cl:Class[T]) = (f:F[ST,LT]) => f.newInstance(cl)
    
    def after[ST<:List,LT<:List](f:F[_,_]=>F[ST,LT]):F[ST,LT]=>F[ST,LT] = f => f
    
    def ifeq2[R<:List,LT<:List,ST2<:List,LT2<:List,T<%JVMInt](then:F[R,LT]=>F[ST2,LT2],elseB:F[R,LT]=>F[ST2,LT2]):F[R**T,LT]=>F[ST2,LT2] = 
      f => f.ifeq2_int[R,ST2,LT2](f.stack.rest,f.stack.top,then,elseB)
    def ifne2[R<:List,LT<:List,ST2<:List,LT2<:List,T<%JVMInt](then:F[R,LT]=>F[ST2,LT2],elseB:F[R,LT]=>F[ST2,LT2]):F[R**T,LT]=>F[ST2,LT2] = 
      ifeq2(elseB,then)
    
    def tailRecursive[ST<:List,LT<:List,ST2<:List,LT2<:List]
      (func: (F[ST,LT] => F[ST2,LT2]) => (F[ST,LT]=>F[ST2,LT2]))(fr:F[ST,LT]):F[ST2,LT2] =
        fr.tailRecursive_int(func)(fr)
  }

  object Implicits{
    implicit def richFunc[ST1<:List,ST2<:List,LT1<:List,LT2<:List](func:F[ST1,LT1] => F[ST2,LT2]):RichFunc[ST1,LT1,ST2,LT2] = new RichFunc[ST1,LT1,ST2,LT2]{
      def apply(f:F[ST1,LT1]):F[ST2,LT2] = func(f)
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
      import Bytecode.Implicits._
	  def foldArray[R<:List,LT<:List,T,U,X](func:F[R**Int**U**T,LT**Array[T]]=>F[R**Int**U,LT**Array[T]]):F[R**Array[T]**U,LT**X] => F[R**U,LT**Array[T]] =
	    _ ~
	    swap ~ 
        local[_0,Array[T]].store() ~ 
	    bipush(0) ~
	    tailRecursive[R**U**Int,LT**Array[T],R**U,LT**Array[T]]{self =>
	      _ ~
	      dup ~
	      local[_0,Array[T]].load() ~
	      arraylength ~
	      isub ~
	      ifeq2(pop,
	            _ ~
	            dup_x1 ~
	            local[_0,Array[T]].load() ~
	            swap ~
	            aload ~
	            func ~
	            swap ~
	            bipush(1) ~ 
	            iadd ~
	            self
	      )
	    }
  }

  type S[s] = F[Nil**s,Nil]

  trait ByteletCompiler{
	  // compile a piece of code which
	  def compile[T<:AnyRef,U<:AnyRef](cl:Class[T])(
                       code: F[Nil**T,Nil] // gets a parameter of type T on the stack
	                      => F[Nil**U,_]   // and uses it and has then a value of type U on the stack
	  ): T => U
  }

  trait RichFunc[ST1<:List,LT1<:List,ST2<:List,LT2<:List] extends (F[ST1,LT1] => F[ST2,LT2]){ first =>
    def ~[ST3<:List,LT3<:List](second:F[ST2,LT2]=>F[ST3,LT3]):RichFunc[ST1,LT1,ST3,LT3] = new RichFunc[ST1,LT1,ST3,LT3]{
      def apply(f:F[ST1,LT1]):F[ST3,LT3] = second(first(f))
    }
  }
  
  def stack[X]:F[Nil**X,Nil]=>F[Nil**X,Nil] = null
  
  def test{
    import Instructions._
    implicit def richFunc[ST1<:List,ST2<:List,LT1<:List,LT2<:List](func:F[ST1,LT1] => F[ST2,LT2]):RichFunc[ST1,LT1,ST2,LT2] = null
    val compiler:ByteletCompiler = null
    
    val l:List = null
    //val u:Nothing = l
    def stack[X]:F[Nil**X,Nil]=>F[Nil**X,Nil] = null
    
    val x = stack[String] ~ invokemethod1(_.length) ~ dup ~ iadd ~ invokemethod1(Integer.valueOf(_))
    //val y = richFunc(method((_:String).length))
    
    
    //val f:F[Nil**String,Nil]=>F[Nil**Integer,Nil] = richFunc(method((_:String).length)) ~ dup ~ iadd ~ method(Integer.valueOf(_))
    def test[T](a:T,b:T):T = null.asInstanceOf[T]
    val x5:Number = test(3.2,5)
    
    def ifeq[R<:List,LT<:List,ST2<:List,LT2<:List](then:F[R,LT]=>F[ST2,LT2],elseB:F[R,LT]=>F[ST2,LT2]):F[R**Int,LT]=>F[ST2,LT2] = null
    
    val ifop:F[Nil**Int,Nil]=>F[Nil**String,Nil] = stack[Int] ~ ifeq(ldc("wurst"),ldc("gustav"));
    
    compiler.compile(classOf[String])(x)
    
    val func:Iterable[Integer] => Seq[String] =null
    val func2: Seq[Integer] => Iterable[String] = func
    val fr:F[Nil**String,Nil] = null
    val fr2:F[Nil**String,Nil] = fr
    
    def fun(i:Number):String = i.toString
    
    /*val fr3:F[Nil**Int,Nil] = null
    fr3 ~ method(fun(_))*/
    
//    val fr3:F[List**String,Nil] = fr
//    fr3 ~ method{(str:String) => str.length}
    
    val f:F[Nil,Nil**String**Int] = null
    f ~ local[_1,String].load()
    
    {
      // test replace type
      
      
      val x:ReplaceNTh[_0,Nil**String**Int**Float,Double] = null
      val x2:_0#Accept[ReplaceNThVisitor[Nil**String**Int**Float,Double]] = x
      val x3:ReplaceNThVisitor[Nil**String**Int**Float,Double]#Visit0 = x2
      val x4:(Nil**String**Int**Float)#Rest**Double = x3
      val x5:Nil**String**Int**Double = x
      
      val y:ReplaceNTh[_1,Cons[Cons[Cons[Nil,String],Int],Float],Double] = null
      val y2:_1#Accept[ReplaceNThVisitor[Nil**String**Int**Float,Double]] = y
      val y3:ReplaceNThVisitor[Nil**String**Int**Float,Double]#VisitSucc[_0] = y2
      val y4:Cons[_0#Accept[ReplaceNThVisitor[Nil**String**Int,Double]],Float] = y3
      val y5:Cons[ReplaceNThVisitor[Nil**String**Int,Double]#Visit0,Float] = y
      val y5a:ReplaceNThVisitor[Nil**String**Int,Double]#Visit0 = null
      val y6:Cons[Cons[(Nil**String**Int)#Rest,Double],Float] = y
      val y6a:Nil**String**Double= y5a
      val y7:Nil**String**Double**Float = y
      
      val z:ReplaceNTh[_2,Nil**String**Int**Float,Double] = null
      //val z2:Nil**Double**Int**Float = z
      
      ()
    }
    
  ()
  }  
}

abstract class AbstractFunction1[T,U] extends Function1[T,U]