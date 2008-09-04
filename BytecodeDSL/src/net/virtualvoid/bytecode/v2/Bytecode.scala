package net.virtualvoid.bytecode.v2

object Bytecode{
  trait List
  trait Nil extends List
  object N extends Nil
  trait Cons[R<:List,T] extends List{
    def rest:R
    def top:T
  }

  // define an infix operator shortcut for the cons type
  type ** [x<:List,y] = Cons[x,y]

  trait F[ST<:List,LT<:List]{
    def stack:ST
    def locals:LT
  }

  trait Int2Stack[ST<:List,LT<:List]{
    def iadd():F[ST**Int,LT]
    def imult():F[ST**Int,LT]
  }
  trait OneStack[R<:List,T,LT<:List]{
    def pop():F[R,LT]
  }
  trait Ops[ST<:List,LT<:List]{
    def bipush(i:Int):F[ST**Int,LT]
  }

  object Implicits{
    implicit def ops[ST<:List,LT<:List](f:F[ST,LT]):Ops[ST,LT] = null
    implicit def int2Stack[R<:List,LT<:List](f:F[R**Int**Int,LT]):Int2Stack[R,LT] = null
    implicit def oneStack[R<:List,LT<:List,T](f:F[R**T,LT]):OneStack[R,T,LT] = null
  }

  val x : Nil**int**String**boolean = null

  trait ByteletCompiler{
	  // compile a piece of code which
	  def compile[T,U](code: F[Nil**T,Nil] // gets a parameter of type T on the stack
	                      => F[Nil**U,_]   // and uses it and has then a value of type U on the stack
	  ): T => U
  }

  object InterpretingCompiler extends ByteletCompiler{
    case class Stack[R<:List,T](rest:R,top:T) extends Cons[R,T]
    case class IF[ST<:List,LT<:List](stack:ST,locals:LT) extends F[ST,LT]

    def compile[T,U](code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U =
      t => code(IF(Stack(N,t),N)).stack.top
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
