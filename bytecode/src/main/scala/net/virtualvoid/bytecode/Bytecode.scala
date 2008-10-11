package net.virtualvoid.bytecode

object Bytecode{
  import java.lang.{String => jString,
                    Boolean => jBoolean
  }

  trait List
  trait Nil extends List
  object N extends Nil
  case class Cons[+R<:List,+T](rest:R,top:T) extends List

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

  trait F[ST<:List,LT<:List]{
    def stack:ST
    def locals:LT

    def bipush(i1:Int):F[ST**Int,LT]
    def ldc(str:jString):F[ST**jString,LT]
    def target:BackwardTarget[ST,LT]
    def jmp(t:Target[ST,LT]):Nothing
    
    // support for forward declaring targets
    def forwardTarget[ST<:List,LT<:List]:ForwardTarget[ST,LT]
    def targetHere(t:ForwardTarget[ST,LT]):F[ST,LT]

    def op[STR<:List,LTR<:List](f:F[ST,LT]=>F[STR,LTR]):F[STR,LTR] = f(this)

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
    def ifeq_int[R<:List](rest:R,top:Any,inner:F[R,LT] => Nothing):F[R,LT]
    def aload_int[R<:List,T](rest:R,array:AnyRef/*Array[T]*/,i:Int):F[R**T,LT]
    def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT]
    def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT]

    def newInstance[T](cl:Class[T]):F[ST**T,LT]
    
    def loadI[T](i:Int):F[ST**T,LT]
    def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT]
  }
  trait Int2Stack[ST<:List,LT<:List]{
    def i1:Int
    def i2:Int
    def rest:ST
    def frame:F[_,LT]
    def iadd():F[ST**Int,LT] = frame.iadd_int[ST](rest,i1,i2)
    def isub():F[ST**Int,LT] = frame.isub_int[ST](rest,i1,i2)
    def imul():F[ST**Int,LT] = frame.imul_int[ST](rest,i1,i2)
  }
  trait OneStack[R<:List,T,LT<:List]{
    def pop():F[R,LT]
    def dup():F[R**T**T,LT]
    def method[U](code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def dynMethod[U](method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT]
    def checkcast[U](cl:Class[U]):F[R**U,LT]
  }
  trait TwoStack[R<:List,T2,T1,LT<:List]{
    def method2WS[U](code:scala.reflect.Code[(T2,T1) => Unit]):F[R,LT] // TODO: unify with method2 if scala allows this
    def method2[U](code:scala.reflect.Code[(T2,T1) => U]):F[R**U,LT]
    def swap():F[R**T1**T2,LT]
    def dup_x1():F[R**T1**T2**T1,LT]
  }
  class BooleanStack[R<:List,LT<:List,X](f:F[R**X,LT]){
    def ifeq(inner:F[R,LT] => Nothing):F[R,LT] =
      f.ifeq_int(f.stack.rest,f.stack.top,inner)
  }
  case class Zipper[ST<:List,L<:List,Cur,R<:List](f:F[ST,_],depth:Int)
  object Implicits{
    implicit def int2Stack[R<:List,LT<:List](f:F[R**Int**Int,LT]):Int2Stack[R,LT] = new Int2Stack[R,LT]{
      val frame = f
      val stack = f.stack
      val rest = stack.rest.rest
      val i1 = stack.rest.top
      val i2 = stack.top
    }
    implicit def oneStack[R<:List,LT<:List,T](f:F[R**T,LT]):OneStack[R,T,LT] = new OneStack[R,T,LT]{
      def pop = f.pop_int(f.stack.rest)

      def dup = f.dup_int(f.stack.rest,f.stack.top)
      def method[U](code:scala.reflect.Code[T=>U]):F[R**U,LT] =
        f.method_int(f.stack.rest,f.stack.top,code)
      def dynMethod[U](method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT] =
        if (resCl.isAssignableFrom(method.getReturnType))
          f.method_int(f.stack.rest,f.stack.top,method,resCl)
        else
          throw new Error("incompatible method, the requested result type "+resCl.getName+" is not compatible with the method's return type " + method.getReturnType.getName)
      def checkcast[U](cl:Class[U]):F[R**U,LT] = f.checkcast_int(f.stack.rest,f.stack.top)(cl)
    }
    implicit def twoStack[R<:List,LT<:List,T1,T2](f:F[R**T2**T1,LT]):TwoStack[R,T2,T1,LT] = new TwoStack[R,T2,T1,LT]{
      def method2[U](code:scala.reflect.Code[(T2,T1) => U]):F[R**U,LT] =
        f.method_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top,code)
      def method2WS[U](code:scala.reflect.Code[(T2,T1) => Unit]):F[R,LT] =
        f.method_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top,code).asInstanceOf[F[R,LT]]
      def swap():F[R**T1**T2,LT] = f.swap_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
      def dup_x1():F[R**T1**T2**T1,LT] = f.dup_x1_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    }
    implicit def booleanStack[R<:List,LT<:List](f:F[R**Boolean,LT]):BooleanStack[R,LT,Boolean] = new BooleanStack(f)
    implicit def intBooleanStack[R<:List,LT<:List](f:F[R**Int,LT]):BooleanStack[R,LT,Int] = new BooleanStack(f)
    trait ArrayLoadStack[R<:List,T,LT<:List]{
      def aload():F[R**T,LT]
    }
    implicit def arrayLoadStack[R<:List,T,LT<:List](f:F[R**Array[T]**Int,LT]) = new ArrayLoadStack[R,T,LT](){
      def aload() = f.aload_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    }
    trait ArrayStoreStack[R<:List,LT<:List]{
      def astore():F[R,LT]
    }
    implicit def arrayStoreStack[R<:List,T,LT<:List](f:F[R**Array[T]**Int**T,LT]) = new ArrayStoreStack[R,LT]{
      def astore():F[R,LT] = f.astore_int(f.stack.rest.rest.rest,f.stack.rest.rest.top,f.stack.rest.top,f.stack.top)
    }
    trait ArrayLengthStack[R<:List,LT<:List]{
      def arraylength():F[R**Int,LT]
    }
    implicit def arrayLengthStack[R<:List,LT<:List,T](f:F[R**Array[T],LT]) = new ArrayLengthStack[R,LT]{
      def arraylength():F[R**Int,LT] = f.arraylength_int(f.stack.rest,f.stack.top)
    }

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
  }

  type S[s] = F[Nil**s,Nil]

  trait ByteletCompiler{
	  // compile a piece of code which
	  def compile[T<:AnyRef,U<:AnyRef](cl:Class[T])(
                       code: F[Nil**T,Nil] // gets a parameter of type T on the stack
	                      => F[Nil**U,_]   // and uses it and has then a value of type U on the stack
	  ): T => U
  }

  
}

abstract class AbstractFunction1[T,U] extends Function1[T,U]
