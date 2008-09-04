package net.virtualvoid.bytecode.v2

object Bytecode{
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

  trait F[ST<:List,LT<:List]{
    def stack:ST
    def locals:LT

    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def bipush(i1:Int):F[ST**Int,LT]
  }
  trait Int2Stack[ST<:List,LT<:List]{
    def i1:Int
    def i2:Int
    def rest:ST
    def frame:F[_,LT]
    def iadd():F[ST**Int,LT] = frame.iadd_int[ST](rest,i1,i2)
    def imult():F[ST**Int,LT] = null
  }
  trait OneStack[R<:List,T,LT<:List]{
    def pop():F[R,LT]
  }

  object Implicits{
    implicit def int2Stack[R<:List,LT<:List](f:F[R**Int**Int,LT]):Int2Stack[R,LT] = new Int2Stack[R,LT]{
      val frame = f
      val stack = f.stack
      val rest = stack.rest.rest
      val i1 = stack.rest.top
      val i2 = stack.top
    }
    implicit def oneStack[R<:List,LT<:List,T](f:F[R**T,LT]):OneStack[R,T,LT] = null
  }

  trait ByteletCompiler{
	  // compile a piece of code which
	  def compile[T,U](code: F[Nil**T,Nil] // gets a parameter of type T on the stack
	                      => F[Nil**U,_]   // and uses it and has then a value of type U on the stack
	  ): T => U
  }

  object InterpretingCompiler extends ByteletCompiler{
    case class IF[ST<:List,LT<:List](stack:ST,locals:LT) extends F[ST,LT]{
      def checked[T](x:Any) = x.asInstanceOf[T]

      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1+i2),locals)
      def bipush(i1:Int):F[ST**Int,LT] = IF(stack ** i1,locals)
    }

    def compile[T,U](code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U =
      t => code(IF(N**t,N)).stack.top
  }
}

object Test{
  def main(args:Array[String]):Unit = {
    import Bytecode._
    import Bytecode.Implicits._
    val succ: Int => Int = Bytecode.InterpretingCompiler.compile(
      (f:F[Nil**Int,Nil]) => f.bipush(1).iadd)
    System.out.println(succ(1))
    System.out.println(succ(2))
  }
}
