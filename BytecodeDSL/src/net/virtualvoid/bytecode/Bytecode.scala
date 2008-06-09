package net.virtualvoid.bytecode

trait Nil[ThisT<:Nil[ThisT]]{
  val self:ThisT = this.asInstanceOf[ThisT]
  
  def bipush(i:int):S[int,ThisT]
  def op[Res](f:ThisT => Res):Res
}

trait Empty extends Nil[Empty]{
}

trait S[Top,Rest] extends Nil[S[Top,Rest]]{
  def pop:Rest
  def dup:S[Top,S[Top,Rest]]
}

trait Ops{
  def add[Rest]:S[int,S[int,Rest]] => S[int,Rest]
  def swap[Rest,A,B]:S[A,S[B,Rest]] => S[B,S[A,Rest]]
  def func[Rest,A,B](method:scala.reflect.Code[A=>B]): S[A,Rest] => S[B,Rest]
  def call[Rest,A,B](method:scala.reflect.Code[A=>Unit]): S[A,S[B,Rest]] => S[B,Rest]
}

object Bytecode {
  def int2str(i:int):String = java.lang.Integer.toString(i)
  def println(i:int) = System.out.println(i)
  def main(args:Array[String]):Unit = {
    System.out println "Hallo Welt"
    val n:Empty = null
    val ops:Ops = null
    import ops._
    val s:S[int,S[int,Empty]] = 
      n.bipush(5).dup.op(add).dup
    s.dup.op(call(println(_)))
      .op(func(int2str(_)))
      .pop
      .bipush(12)
      .op(add)
      .bipush(5)
      .op(func(Integer.toString(_)))
      .op(call(System.out.println(_:String)))
      .dup
      .op(func(Integer.toString(_)))
      .op(swap)
      .dup
      .op(add)
    s
  }
}
