package net.virtualvoid.bytecode

/*
 * Define type and implicits for so-called Category 1 data types:
 * types which are of 32-bit size.
 * See §3.11.1 of the JVM specs
 * http://java.sun.com/docs/books/jvms/second_edition/html/Overview.doc.html#37906
 */
// FIXME: Produces a problem in the static tests with the interpreter
//@scala.annotation.implicitNotFound("${T} is no category 1 type.")
trait Category1[-T]

trait Category1TypeClassInstances {
  implicit val cat1Boolean  : Category1[Boolean] = null
  implicit val cat1Byte     : Category1[Byte] = null
  implicit val cat1Char     : Category1[Char] = null
  implicit val cat1Short    : Category1[Short] = null
  implicit val cat1Int      : Category1[Int] = null
  implicit val cat1Float    : Category1[Float] = null
  implicit def cat1AnyRef   : Category1[AnyRef] = null
}
