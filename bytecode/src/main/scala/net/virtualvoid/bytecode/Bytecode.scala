package net.virtualvoid.bytecode

object Bytecode{
  import java.lang.{String => jString,
                    Boolean => jBoolean
  }

  trait List
  trait Nil extends List
  object N extends Nil
  
  case class Cons[+R<:List,+T](rest:R,top:T) extends List{
    def l = rest
  }
  trait Consable[T<:List]{
    def **[U](next:U): T**U
  }
  implicit def conser[T<:List](t:T) = new Consable[T]{
    def **[U](next:U): T**U = Cons(t,next)
  }

  // define an infix operator shortcut for the cons type
  type ** [x<:List,y] = Cons[x,y]

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
  
  trait F[+ST<:List,+LT<:List]{
    def depth = -1
    def frame = this
    
    def stack:ST
    def locals:LT

    def bipush[S>:ST](i1:Int):F[S**Int,LT]
    def ldc[S>:ST](str:jString):F[S**jString,LT]
    def target[S>:ST<:List,L>:LT<:List]:BackwardTarget[S,L] = null
    def jmp[S>:ST<:List,L>:LT<:List](t:Target[S,L]):Nothing = null.asInstanceOf[Nothing]
    
    // support for forward declaring targets
    def forwardTarget[ST<:List,LT<:List]:ForwardTarget[ST,LT] = null
    def targetHere[S>:ST<:List,L>:LT<:List](t:ForwardTarget[S,L]):F[S,L] = null

    def ~[X](f:F[ST,LT]=>X):X = f(this)
    
    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def pop_int[R<:List](rest:R):F[R,LT]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT]
    def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2,LT]
    def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1,LT]
    def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def method_int[R<:List,T,U](rest:R,top:T,method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT]
    def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT]
    def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT]
    def ifeq_int[R<:List](rest:R,top:JVMInt,inner:F[R,LT] => Nothing):F[R,LT]
    def ifeq2_int[R<:List,ST2<:List,LT2<:List](rest:R,top:JVMInt,then:F[R,LT]=>F[ST2,LT2],elseB:F[R,LT]=>F[ST2,LT2]):F[ST2,LT2]
    def aload_int[R<:List,T](rest:R,array:AnyRef/*Array[T]*/,i:Int):F[R**T,LT]
    def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT]
    def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT]
    
    def tailRecursive_int[ST2<:List,LT2<:List,S>:ST<:List,L>:LT<:List]
        (func: (F[S,L] => F[ST2,LT2]) => (F[S,L]=>F[ST2,LT2]))(fr:F[S,L]):F[ST2,LT2]
    
    def pop_unit_int[R<:List](rest:R):F[R,LT]

    def newInstance[T,S>:ST](cl:Class[T]):F[S**T,LT]
    
    def loadI[T,S>:ST](i:Int):F[S**T,LT]
    def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT]
  }
  object Operations{
    def iop[R<:List,LT<:List](func:(F[R**Int**Int,LT],R,Int,Int)=>F[R**Int,LT]):
      F[R**Int**Int,LT] => F[R**Int,LT] = f => func(f,f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def iadd[R<:List,LT<:List] = 
      iop[R,LT](_.iadd_int(_,_,_))
    def imul[R<:List,LT<:List] = 
      iop[R,LT](_.imul_int(_,_,_))
    def isub[R<:List,LT<:List] = 
      iop[R,LT](_.isub_int(_,_,_))
    
    def method[T,U,R<:List,LT<:List](code:scala.reflect.Code[T=>U]):F[R**T,LT] => F[R**U,LT] = 
      f => f.method_int(f.stack.rest,f.stack.top,code)
    def method2[T1,T2,U,R<:List,LT<:List](code:scala.reflect.Code[(T1,T2)=>U]):
      F[R**T1**T2,LT] => F[R**U,LT] = f => f.method_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top,code)
    def dynMethod[T,U,R<:List,LT<:List](method:java.lang.reflect.Method,resT:Class[U]):
      F[R**T,LT] => F[R**U,LT] = f => f.method_int(f.stack.rest,f.stack.top,method,resT)
    
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
    
    def ifeq[R<:List,LT<:List,T<%JVMInt](inner:F[R,LT]=>Nothing):F[R**T,LT] => F[R,LT] = f=>f.ifeq_int(f.stack.rest,f.stack.top,inner)
    def target[ST<:List,LT<:List] = (f:F[ST,LT]) => f.target
    def targetHere[ST<:List,LT<:List](t:ForwardTarget[ST,LT]) = (f:F[ST,LT]) => f.targetHere(t)
    def jmp[ST<:List,LT<:List](t:Target[ST,LT]) = (f:F[ST,LT]) => f.jmp(t)
       
    def newInstance[ST<:List,LT<:List,T](cl:Class[T]) = (f:F[ST,LT]) => f.newInstance(cl)
    
    def after[ST<:List,LT<:List](f:F[_,_]=>F[ST,LT]):F[ST,LT]=>F[ST,LT] = f => f
    
    def load[ST<:List,LT<:List,T,R](l:LT=>R**T):F[ST,LT]=>F[ST**T,LT] = f=>{
      var i = 0;
      val c:LT = new Cons(null.asInstanceOf[Cons[Nothing,Nothing]],null){
        override def l = {
          i+=1
          this.asInstanceOf[Cons[Nothing,Nothing]]
        }
      }.asInstanceOf[LT]
      l(c)
      f.loadI(i).asInstanceOf[F[ST**T,LT]]
    }
    
    def l0[R<:List,T]:R**T=>R**T = (f:R**T) => f
    def l1[R<:List,T2,T1]:R**T2**T1=>R**T2 = (f:R**T2**T1) => f.l
    def l2[R<:List,T3,T2,T1] = (f:R**T3**T2**T1) => f.l.l
    
    def ifeq2[R<:List,LT<:List,ST2<:List,LT2<:List,T<%JVMInt](then:F[R,LT]=>F[ST2,LT2],elseB:F[R,LT]=>F[ST2,LT2]):F[R**T,LT]=>F[ST2,LT2] = f=>f.ifeq2_int[R,ST2,LT2](f.stack.rest,f.stack.top,then,elseB)
    def tailRecursive[ST<:List,LT<:List,ST2<:List,LT2<:List]
      (func: (F[ST,LT] => F[ST2,LT2]) => (F[ST,LT]=>F[ST2,LT2]))(fr:F[ST,LT]):F[ST2,LT2] =
        fr.tailRecursive_int(func)(fr)
  }

  object Implicits{
    trait Zippable[ST<:List,L<:List,Cur,R<:List]{
      def l():Zipper[ST,L,Cur,R]
    }
    trait DeZippable[ST<:List,L<:List,Cur,R<:List]{
      def e():Zipper[ST,L,Cur,R]
    }
    trait EndZipped[ST<:List,LT<:List]{
      def e():F[ST,LT]
    }
    trait Loadable[ST<:List,L<:List,Cur,R<:List]{
      def load():Zipper[ST,L,Cur,R]
    }
    trait Storeable[ST<:List,L<:List,Cur,R<:List]{
      def store():Zipper[ST,L,Cur,R]
    }

    implicit def moreZipping[ST<:List,LR<:List,LT,Cur,R<:List](z:Zipper[ST,LR**LT,Cur,R]) = new Zippable[ST,LR,LT,R**Cur]{
      def l():Zipper[ST,LR,LT,R**Cur] = Zipper(z.f,z.depth + 1)
    }
    implicit def notEmptyZipper[ST<:List,L<:List,Cur,RR<:List,RT](z:Zipper[ST,L,Cur,RR**RT]) = new DeZippable[ST,L**Cur,RT,RR]{
      def e():Zipper[ST,L**Cur,RT,RR] = Zipper(z.f,z.depth - 1)
    }
    implicit def emptyZipper[ST<:List,L<:List,Cur](z:Zipper[ST,L,Cur,Nil]) = new EndZipped[ST,L**Cur]{
      def e():F[ST,L**Cur] = z.f.asInstanceOf[F[ST,L**Cur]]
    }
    implicit def zippable[ST<:List,R<:List,T](f:F[ST,R**T]) = new Zippable[ST,R,T,Nil]{
      def l():Zipper[ST,R,T,Nil] = Zipper(f,0)
    }
    implicit def zipLoad[ST<:List,L<:List,Cur,R<:List](z:Zipper[ST,L,Cur,R]) = new Loadable[ST**Cur,L,Cur,R]{
      def load():Zipper[ST**Cur,L,Cur,R] = Zipper(z.f.loadI(z.depth),z.depth)
    }
    implicit def zipStore[ST,SR<:List,L<:List,R<:List](z:Zipper[SR**ST,L,_,R]) = new Storeable[SR,L,ST,R]{
      def store():Zipper[SR,L,ST,R] = Zipper(z.f.storeI(z.f.stack.rest,z.f.stack.top,z.depth),z.depth)
    }
    implicit def genNewLocal[ST<:List](f:F[ST,Nil]) = new Zippable[ST,Nil,Nil,Nil]{
      def l():Zipper[ST,Nil,Nil,Nil] = Zipper(f,0)
    }
    implicit def genNewLocalInZipper[ST<:List,Cur,R<:List](z:Zipper[ST,Nil,Cur,R]) = new Zippable[ST,Nil,Nil,R**Cur]{
      def l():Zipper[ST,Nil,Nil,R**Cur] = Zipper(z.f,z.depth + 1)
    }
    
    implicit def richFunc[ST1<:List,ST2<:List,LT1<:List,LT2<:List](func:F[ST1,LT1] => F[ST2,LT2]):RichFunc[ST1,LT1,ST2,LT2] = new RichFunc[ST1,LT1,ST2,LT2]{
      def apply(f:F[ST1,LT1]):F[ST2,LT2] = func(f)
    }
  }
  object RichOperations{
    import Operations._
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
        (_.l.store.e) ~ 
	    bipush(0) ~
	    tailRecursive[R**U**Int,LT**Array[T],R**U,LT**Array[T]]{self =>
	      _ ~
	      dup ~
	      load(l0) ~
	      arraylength ~
	      isub ~
	      ifeq2(pop,
	            _ ~
	            dup_x1 ~
	            load(l0) ~
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
    import Operations._
    implicit def richFunc[ST1<:List,ST2<:List,LT1<:List,LT2<:List](func:F[ST1,LT1] => F[ST2,LT2]):RichFunc[ST1,LT1,ST2,LT2] = null
    val compiler:ByteletCompiler = null
    
    val l:List = null
    //val u:Nothing = l
    def stack[X]:F[Nil**X,Nil]=>F[Nil**X,Nil] = null
    
    val x = stack[String] ~ method(_.length) ~ dup ~ iadd ~ method(Integer.valueOf(_))
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
    val fr2:F[Nil**AnyRef,Nil] = fr
  ()
  }  
}

abstract class AbstractFunction1[T,U] extends Function1[T,U]
