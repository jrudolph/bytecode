package net.virtualvoid.bytecode

trait List
trait Nil extends List
object N extends Nil

case class Cons[+R<:List,+T](rest:R,top:T) extends List

// define the same for values
trait Consable[T<:List]{
  def **[U](next:U): T**U
}

trait ListShortcuts {
  // define an infix operator shortcut for the cons type
  type ** [x<:List,y] = Cons[x,y]
  
  implicit def conser[T<:List](t:T) = new Consable[T]{
    def **[U](next:U): T**U = Cons(t,next)
  }
}
