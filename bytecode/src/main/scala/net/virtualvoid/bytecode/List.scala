package net.virtualvoid.bytecode

// our heterogenous list type
trait List
// a list is either an empty list Nil...
trait Nil extends List
// or constructed from a rest-list with type R and a top element of type T
case class Cons[+R <: List, +T](rest: R,top: T) extends List

// define the same for values
object N extends Nil

/*
 * A Func is a polymorphic function with several parameters of types
 * as indicated with the parameter Args returning a value of type U.
 *
 * We define a shortcut `|=>` for Func in ListShortcuts.
 * 
 * Example:
 *   Nil**Int**Double |=> String
 *
 * would be the type of a function which pops an Int and a Double from the top
 * of a list and pushes a String result back.
 */
trait Func[Args <: List, U] {
  /**
   * This contains the implementation of the function.
   */
  def apply(args: Args): U
 
  /**
   * This contains the logic to make this function work generically on any
   * List which has the parameters on top.
   */
  def apply[R <: List, Res <: List](implicit sub: Sub[R, Args, Res]): R => Res**U =
    st => sub.res(st) match {
      case (b, r) => r ** apply(b)
    }
}

/**
 * Helper class to implement the substraction of two type lists.
 * The availability of an implicit value of type Sub[A, B, Res] means
 * that there is a valid subtraction A - B which yields Res.
 *
 * For example:
 *   Sub[Nil**String**Double, Nil**Double, Nil**String]
 *   is such a valid type which will be infered if it is requested as
 *   an implicit value. 
 */
trait Sub[A, B, Res] {
  /**
   * If Sub is a valid type wrt the generating implicits, this function
   * returns both the actual parameter values from the top of the list and
   * the rest list.
   */
  def res(a: A): (B, Res)
}
trait SubstractionImplicits {
  /**
   * The base case: if B is Nil, everything is subtracted and
   * we can return R.
   */
  implicit def sub0[R <: List]: Sub[R, Nil, R] = new Sub[R, Nil, R] {
    def res(a: R) = (N, a)
  }
  /**
   * The recursive case: It is a valid Sub-type if both
   * A and B have a compatible value T on top of the list and we have a valid
   * Sub for the rest of the lists. Res is the same as the inferred Res from the
   * implicit parameter.
   */
  implicit def subt[R1 <: List, T, R2 <: List, Res <: List](implicit s: Sub[R1, R2, Res]): Sub[R1**T, R2**T, Res] =
    new Sub[R1**T, R2**T, Res] {
      def res(a: R1**T) = s.res(a.rest) match {
        case (b, r) => (b**a.top, r)
      }
    }
}

trait ListShortcuts extends SubstractionImplicits {
  // define an infix operator shortcut for the cons type
  type ** [x <: List, y] = Cons[x, y]

  type |=>[R <: List, U] = Func[R, U]

  trait Consable[T <: List] {
    def **[U](next: U): T**U
  }

  // make every type consable into a heterogenous list 
  implicit def conser[T <: List](t: T) = new Consable[T] {
    def **[U](next: U): T**U = Cons(t, next)
  }
}
