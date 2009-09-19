package net.virtualvoid.collection

import _root_.net.virtualvoid.bytecode._
import Bytecode._

trait Seq[+T] { outer =>
  def map[U](outerF:T=>U):Seq[U] = new Seq[U] {
    def foldLeft[V](start:V)(f:(V,U)=>V):V = 
      outer.foldLeft(start)((state,next) => f(state,outerF(next)))
  }
  def filter(filterF:T=>Boolean):Seq[T] = new Seq[T] {
    def foldLeft[V](start:V)(f:(V,T)=>V):V = 
      outer.foldLeft(start)((state,next) =>
        if (filterF(next))
          f(state,next)
        else
          state // skip processing of filtered
  	  )
  }
  def foldLeft[U](start:U)(f:(U,T)=>U):U
}

trait CompilableFunc[T,U] {
  def compile[R <: List]:F[R**T]=>F[R**U]
}
trait CompilableFunc2[T1,T2,U] {
  def compile[R <: List]:F[R**T1**T2]=>F[R**U]
}


trait FastSeq[+T] extends Seq[T] {
  override def map[U](f:T=>U):FastSeq[U] = null
  override def filter(f:T=>Boolean):FastSeq[T] = null
  override def foldLeft[U](start:U)(f:(U,T)=>U):U
  
  //def compile[U,R<:List](func:CompilableFunc[(U,T),U])(f:F[R**U]):F[R**U]
}

class IntSeq(until:Int) {//extends FastSeq[Int]{
  outer =>
  def foldLeft[U](filterF:Int=>Boolean)(start:U)(f:(U,Int)=>U):U = {
    def loop(cur:Int,curValue:U):U = 
      if (cur<until)
        loop(cur+1,if (filterF(cur)) f(curValue,cur) else curValue)
      else
        curValue
    
    loop(0,start)
  }
  def filter(filterF:CompilableFunc[Int,Int]):IntSeq = new IntSeq(until) {
    import Instructions._
    override def compiledFoldLeft[U<:AnyRef](start:U)(f:CompilableFunc2[U,Int,U])(implicit mf:scala.reflect.Manifest[U]):U = {
      outer.compiledFoldLeft(start)(new CompilableFunc2[U,Int,U]{
        def compile[R <: List]:F[R**U**Int]=>F[R**U] =
          _ ~ 
            dup ~
            filterF.compile ~
            ifeq2(pop,f.compile)
      })(mf)
    }
  }
  def map[R<:List](mapF:CompilableFunc[Int,Int]):IntSeq = new IntSeq(until) {
    import Instructions._
    override def compiledFoldLeft[U<:AnyRef](start:U)(f:CompilableFunc2[U,Int,U])(implicit mf:scala.reflect.Manifest[U]):U =
      outer.compiledFoldLeft(start)(new CompilableFunc2[U,Int,U]{
        def compile[R <: List]:F[R**U**Int]=>F[R**U] =
	      _ ~ 
	      	mapF.compile ~
	      	f.compile
      })(mf)
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
                      f.compile ~
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

object Helper {
  def func1[T,U](f:F[Nil**T]=>F[Nil**U]):CompilableFunc[T,U] = new CompilableFunc[T,U]{
    def compile[R <: List]:F[R**T]=>F[R**U] = f.asInstanceOf[F[R**T]=>F[R**U]]
  }
  
  import Instructions._
  def NE(i:Int) = func1[Int,Int](_ ~ bipush(i) ~ isub)
  def AddConstant(i:Int) = func1[Int,Int](_ ~ bipush(i) ~ iadd)
}

object BoxedIntAdd extends CompilableFunc2[Integer,Int,Integer]{
  import Instructions._
  def compile[R <: List]:F[R**Integer**Int]=>F[R**Integer] = _ ~ swap() ~ invokemethod1(_.intValue) ~ iadd ~ invokemethod1(java.lang.Integer.valueOf(_))
}
 

object TestIntSeq {
  import Helper._
  
  def main(args:Array[String]){
    val seq = new IntSeq(5)
    println(seq.map(AddConstant(12)).filter(Helper.NE(2)).compiledFoldLeft(Integer.valueOf(0))(BoxedIntAdd))
    println(seq.filter(Helper.NE(2)).map(AddConstant(12)).compiledFoldLeft(Integer.valueOf(0))(BoxedIntAdd))
  }
}