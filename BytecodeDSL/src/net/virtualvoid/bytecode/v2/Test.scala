package net.virtualvoid.bytecode.v2

class Person{
      def name():java.lang.String = "Joe"
      def sb():java.lang.StringBuilder = new java.lang.StringBuilder
      def accountNames():java.util.List[java.lang.String] = java.util.Arrays.asList("a","b")
}

object Test2 {
  import Bytecode._
  import Bytecode.Implicits._

  def main(args:Array[String]) {

    compile
  }

  def compile{
    /*
     * "Name: #name Accounts: #accountNames(,)*"
     * "Name: Joe Account: a,b"
     *
     */
    val f: Person=>String = ASMCompiler.compile(classOf[Person])(
        f => {
      val jmpTarget = f.dup
       .method(x => x.sb())
       .l.store.e  // store sb in l0
       .l.l.store.e.e // store person in l1
       .l.load.e
       .ldc("Name: ")
       .method2(_.append(_))
       .l.l.load.e.e
       .method(_.name)
       .method2(_.append(_))
       .ldc(" ")
       .method2(_.append(_))
       .l.l.load.e.e
       .method(_.accountNames)
       .method( names => names.iterator)
       .l.l.l.store.e.e.e
       .target
      jmpTarget
       .l.l.l.load.e.e.e
       .method(_.hasNext)
       .ifeq(
          f =>
        f.l.l.l.load.e.e.e
         .method(_.next)
         .checkcast(classOf[java.lang.String])
         .method2(_.append(_))
         .jmp(jmpTarget)
       )
       .method(_.toString)
    })
    System.out.println(f(new Person))
  }
  compile

}
