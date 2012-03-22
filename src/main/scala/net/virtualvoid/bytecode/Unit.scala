package net.virtualvoid.bytecode

trait IsUnit[-T]
trait NoUnit[-T]

trait UnitTypeClassInstances {
  implicit val unitIsUnit  : IsUnit[Unit] = null
  implicit val anyrefNoUnit: NoUnit[AnyRef] = null
  implicit val boolNoUnit  : NoUnit[Boolean] = null
  implicit val byteNoUnit  : NoUnit[Byte] = null
  implicit val charNoUnit  : NoUnit[Char] = null
  implicit val shortNoUnit : NoUnit[Short] = null
  implicit val intNoUnit   : NoUnit[Int] = null
  implicit val floatNoUnit : NoUnit[Float] = null
  implicit val doubleNoUnit: NoUnit[Double] = null
  implicit val longNoUnit  : NoUnit[Long] = null
}
