package net.virtualvoid.bytecode

// our heterogenous list type
trait List
// a list is either an empty list Nil...
trait Nil extends List
// or constructed from a rest-list with type R and a top element of type T
case class Cons[+R <: List, +T](rest: R,top: T) extends List

// define the same for values
object N extends Nil

trait ListShortcuts {
  // define an infix operator shortcut for the cons type
  type ** [x <: List, y] = Cons[x, y]

  trait Consable[T <: List] {
    def **[U](next: U): T**U
  }

  // make every type consable into a heterogenous list 
  implicit def conser[T <: List](t: T) = new Consable[T] {
    def **[U](next: U): T**U = Cons(t, next)
  }
}
