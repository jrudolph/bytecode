package net.virtualvoid.bytecode.v2

class Person{
      def name():java.lang.String = "Joe"
      def sb():java.lang.StringBuilder = new java.lang.StringBuilder
    }

object Test2 {
  import Bytecode._
  import Bytecode.Implicits._

  def main(args:Array[String]) {

    val f2:F[Nil**Int,Nil**java.lang.String**Int] = null
    f2
      .dup
      .l.l.store.e.e
      .l.l.store.e.e
      //.l.l.load.e.e
      //.pop
      //.method(_.length)
  }
}
