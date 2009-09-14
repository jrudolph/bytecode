package net.virtualvoid.collection

import _root_.net.virtualvoid.bytecode._
import Bytecode._

trait Seq[+T] {
  def map[U](f:T=>U):Seq[U]
  def filter(f:T=>Boolean):Seq[T]
  def foldLeft[U](start:U)(f:(U,T)=>U):U
}

trait CompilableFunc[T,U] {
  def compile[R <: List](frame:F[R**T]):F[R**U]
}
trait CompilableFunc2[T1,T2,U] {
  def compile[R <: List](frame:F[R**T1**T2]):F[R**U]
}


trait FastSeq[+T] extends Seq[T] {
  override def map[U](f:T=>U):FastSeq[U] = null
  override def filter(f:T=>Boolean):FastSeq[T] = null
  override def foldLeft[U](start:U)(f:(U,T)=>U):U
  
  //def compile[U,R<:List](func:CompilableFunc[(U,T),U])(f:F[R**U]):F[R**U]
}

class IntSeq(until:Int) extends FastSeq[Int]{
  override def foldLeft[U](start:U)(f:(U,Int)=>U):U = {
    def loop(cur:Int,curValue:U):U = 
      if (cur<until)
        loop(cur+1,f(curValue,cur))
      else
        curValue
    
    loop(0,start)
  }
  def compiledFoldLeft[U<:AnyRef](start:U)(f:CompilableFunc2[U,Int,U])(implicit mf:scala.reflect.Manifest[U]):U = {
    import Instructions._
    ASMCompiler.compile[U,U](mf.erasure.asInstanceOf[Class[U]])(
      _ ~ bipush(0)
        ~ withLocal( cur =>
            _ ~ 
              tailRecursive[Nil**U,Nil**U]( self =>
                _ ~
                  bipush(until) ~
                  cur.load ~
                  isub ~
                  ifeq2(
                    x=>x,
                    _ ~
                      cur.load ~
                      f.compile[Nil] ~
                      cur.load ~
                      bipush(1) ~
                      iadd ~
                      cur.store ~
                      self
                  )
              )
          )
    )(start)
  }
}

object BoxedIntAdd extends CompilableFunc2[Integer,Int,Integer]{
  import Instructions._
  def compile[R <: List](frame:F[R**Integer**Int]):F[R**Integer] = frame ~ swap() ~ invokemethod1(_.intValue) ~ iadd ~ invokemethod1(java.lang.Integer.valueOf(_))
}

object TestIntSeq {
  def main(args:Array[String]){
    val seq = new IntSeq(5)
    println(seq.compiledFoldLeft(Integer.valueOf(0))(BoxedIntAdd))
  }
}