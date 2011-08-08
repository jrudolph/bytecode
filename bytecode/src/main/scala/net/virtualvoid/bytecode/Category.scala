package net.virtualvoid.bytecode

/*
 * Define type and implicits for so-called Category 1 data types:
 * types which are of 32-bit size.
 * See §3.11.1 of the JVM specs
 * http://java.sun.com/docs/books/jvms/second_edition/html/Overview.doc.html#37906
 */
// FIXME: Produces a problem in the static tests with the interpreter
//@scala.annotation.implicitNotFound("${T} is no category 1 type.")
trait Category1

trait Category1TypeClassInstances {
  implicit val cat1Boolean  : Boolean => Category1 = null
  implicit val cat1Byte     : Byte => Category1 = null
  implicit val cat1Char     : Char => Category1 = null
  implicit val cat1Short    : Short => Category1 = null
  implicit val cat1Int      : Int => Category1 = null
  implicit val cat1Float    : Float => Category1 = null
  implicit def cat1AnyRef   : AnyRef => Category1 = null
}
