package net.virtualvoid.bytecode

import _root_.org.specs._

object BytecodeCompilerSpecs extends Specification{
  def compiledTests(compiler:net.virtualvoid.bytecode.Bytecode.ByteletCompiler){
    import Bytecode._
    import Bytecode.Implicits._
    import Bytecode.Operations._
    
    "bipush(20)" in {
      compiler.compile(classOf[String])(_~pop~bipush(20)~method(Integer.valueOf(_)))
        .apply("Test") must be_==(20)}
    "method(_.length)" in {
      compiler.compile(classOf[String])(_~method(_.length)~method(Integer.valueOf(_)))
        .apply("Test") must be_==(4)}
    "locals + method2" in {
      compiler.compile(classOf[java.lang.String])(_~(_.l.store.e)~load(l0)~load(l0)~method2(_.concat(_)))
      .apply("Test") must be_==("TestTest")}
    "iadd with operations" in {
      compiler.compile(classOf[java.lang.Integer])(
        _ ~ method(_.intValue) ~ dup
        ~ iadd
        ~ method(Integer.valueOf(_))
      ).apply(12) must be_==(24)
    }
    "iadd" in {
      compiler.compile(classOf[java.lang.Integer])(_~method(_.intValue)~dup~iadd~bipush(3)~iadd~method(Integer.valueOf(_)))
      .apply(12) must be_==(27)}
    "store int in locals" in {
      compiler.compile(classOf[java.lang.Integer])(_~method(_.intValue)~dup~(_.l.store.e)~load(l0)~iadd~method(Integer.valueOf(_)))
      .apply(12) must be_==(24)}
    "store double in locals" in {
      compiler.compile(classOf[java.lang.Double])(_~method(_.doubleValue)~(_.l.store.e)~load(l0)~method(java.lang.Double.valueOf(_)))
      .apply(12.453) must be_==(12.453)}
    "store double after method2" in {
      compiler.compile(classOf[java.lang.Double])(_~method(_.doubleValue)~ldc("test")~dup~method2(_.concat(_))~pop~(_.l.store.e)~load(l0)~method(java.lang.Double.valueOf(_:Double)))
      .apply(12.453) must be_==(12.453)}
    "store something more than 1 level deep" in {
      compiler.compile(classOf[String])(_.l.l.store.e.e ~ load(l1))
      .apply("test") must be_==("test")
    }
    "load element with index 1 from a string array" in {
      compiler.compile(classOf[Array[String]])(_.bipush(1)~aload)
      .apply(array("That","is","a","Test")) must be_==("is")
    }
    "save string element to array and load it afterwards" in {
      compiler.compile(classOf[Array[String]])(_~dup~bipush(1)~ldc("test")~astore~bipush(1)~aload)
      .apply(array("That","is","a","Test")) must be_==("test")
    }
    "save int element to array and load it afterwards" in {
      compiler.compile(classOf[Array[Int]])(_~dup~bipush(1)~bipush(13)~astore~bipush(1)~aload~dup~iadd~method(Integer.valueOf(_)))
      .apply(array(1,2,3,4)) must be_==(26)
    }
    "get array length" in {
      compiler.compile(classOf[Array[String]])(_~arraylength~method(Integer.valueOf(_)))
      .apply(array("That","is","a","problem")) must be_==(4)
    }
    "isub" in {
      compiler.compile(classOf[java.lang.Integer])(_~method(_.intValue)~bipush(3)~isub~method(Integer.valueOf(_)))
      .apply(12) must be_==(9)
    }
    "dup_x1" in {
      compiler.compile(classOf[java.lang.Integer])(_~dup~method(_.toString)~swap~method(_.intValue)~dup_x1~swap~pop~iadd~method(Integer.valueOf(_)))
      .apply(12) must be_==(24)
    }
    "create new StringBuilder" in {
      compiler.compile(classOf[java.lang.String])(_~dup~newInstance(classOf[java.lang.StringBuilder])~swap~method2(_.append(_))~swap~method2(_.append(_))~method(_.toString))
      .apply("test") must be_==("testtest") 
    }
    "store string after void method" in {
      compiler.compile(classOf[java.lang.String])(_ ~ newInstance(classOf[java.text.SimpleDateFormat]) ~ ldc("yyyy") ~ method2(_.applyPattern(_)) ~ pop_unit ~ (_.l.store.e) ~ load(l0))
      .apply("test") must be_==("test")
    }
    "ifeq and jmp" in {
      if (compiler != Interpreter)
      compiler.compile(classOf[java.lang.Integer])(
        f => {
          val start = f ~
            method(_.intValue) ~
            (_.l.store.e) ~
            bipush(0) ~
            (_.l.l.store.e.e) ~
            target
          
          start ~
            load(l0) ~
            ifeq(f => 
              f ~ load(l0) ~
                dup ~
                bipush(1) ~
                isub ~
                (_.l.store.e) ~
                load(l1) ~
                iadd ~
                (_.l.l.store.e.e) ~
                jmp(start)
            ) ~
            load(l1) ~
            method(Integer.valueOf(_))
      }).apply(5) must be_==(15)
    }
  }
  def array(els:Int*):Array[Int] = Array(els:_*)
  def array(els:String*):Array[String] = Array(els:_*)
  
  "Compiler" should {
    "succeed in generic Tests" in compiledTests(net.virtualvoid.bytecode.ASMCompiler)
  }
  "Interpreter" should {
    "succeed in generic Tests" in compiledTests(net.virtualvoid.bytecode.Interpreter)
  }
  
  def println(i:Int) = System.out.println(i)
  
  {
    import Bytecode._
    import Operations._
    import Implicits._
    import RichOperations._
      
      def call[ST1<:List,ST2<:List,LT1<:List,LT2<:List](f: => (F[ST1,LT1]=>F[ST2,LT2])):F[ST1,LT1]=>F[ST2,LT2] =
        x => f(x)
      
      //def state[ST<:List,LT<:List](prefix:String):F[ST,LT]=>F[ST,LT] = {x => System.out.println(prefix + x.stack);x}
      
      def frame[ST<:List,LT<:List]:F[ST,LT] => F[ST,LT] = f=>f
      def simple[R<:List]:F[R,Nil]=>F[R,Nil] = f => f
      def id[ST<:List,LT<:List](f:F[ST,LT]):F[ST,LT]=>F[ST,LT] = x=>x
      def ider[ST<:List,LT<:List,ST2<:List,LT2<:List](fr:F[ST,LT],func:F[ST,LT]=>F[ST2,LT2]):F[ST2,LT2] = func(fr)
      
      lazy val f:F[Nil**Int**Int,Nil]=>F[Nil**Int,Nil] =
        _ ~
        //simple[Nil**Int**Int] ~
        //state("before check: ") ~
        dup ~
        ifeq2(
          pop,
          //simple[Nil**Int**Int]
          _ 
            ~ dup_x1 //x,sum,x
            ~ iadd ~ dup ~ method(println(_)) ~ pop_unit
            //~ state("after println")
            ~ swap
            ~ bipush(1) ~ isub ~ call(f)
        )
      

        //func(tailRecursive(func)_)(fr)
      
      def tailRecursive2[R<:List,X[_]<:List,Y[_]<:List,LT<:List](func: (F[X[R],LT] => F[Y[R],LT]) => (F[X[R],LT]=>F[Y[R],LT]))(fr:F[X[R],LT]): 
        F[Y[R],LT] = 
        func(tailRecursive2[R,X,Y,LT](func)_)(fr)
      
      val tr = tailRecursive[Nil**Int**Int,Nil,Nil**Int,Nil] {self =>
        simple[Nil**Int**Int] ~ pop ~ dup ~ self
      } _
          
      val func = ASMCompiler.compile(classOf[java.lang.Integer])(_ //simple[Nil**java.lang.Integer]
                                                                 ~ method(_.intValue) 
                                                                 ~ bipush(1) 
                                                                 ~ swap
                                                                 ~ tailRecursive[Nil**Int**Int,Nil,Nil**Int,Nil]{self =>
                                                                   simple[Nil**Int**Int] ~
                                                                     //state("before check: ") ~
                                                                     dup ~
                                                                     ifeq2(
                                                                       pop,
															          //simple[Nil**Int**Int]
															          fr =>
															            ider(fr,
															            id(fr)
															            ~ dup_x1 //x,sum,x
															            ~ iadd ~ dup ~ method(println(_)) ~ pop_unit
															            //~ state("after println")
															            ~ swap
															            ~ bipush(1) ~ isub ~ self)
															        )
                                                                 } _ 
                                                                   //~ f // 0 , x
                                                                 ~ method(Integer.valueOf(_)))
      System.out.println(func(5))
      
          

      
      import java.lang.{Iterable => jIterable}
      import java.util.{Iterator => jIterator}
      
      /**
       def foldIterator(func,it,start) =
         if (it.hasNext)
            foldIterator(func,func(it.next,start))
         else
       *    start
       */
      def foldIterable[R<:List,LT<:List,T,U,X](eleType:Class[T],func:F[R**U**T,LT**jIterator[T]]=>F[R**U,LT**jIterator[T]])
          :F[R**U**jIterable[T],LT**X] => F[R**U,LT**jIterator[T]] =
        _ ~
        method(_.iterator) ~
        (_.l.store.e) ~
        tailRecursive[R**U,LT**jIterator[T],R**U,LT**jIterator[T]]{ self =>
          _ ~
          load(l0) ~
          method(_.hasNext) ~
          ifeq2(
                f=>f,
                _ ~
                load(l0) ~
                method(_.next) ~
                checkcast(eleType) ~
                func ~
                self)         
        }
        
      
      val func2 = ASMCompiler.compile(classOf[Array[Int]])(
        _ ~
        bipush(0) ~
        dup ~
        (_.l.store.e) ~
        foldArray(iadd) ~
        method(Integer.valueOf(_))
      )
      
      val func3 = ASMCompiler.compile(classOf[java.util.List[java.lang.Integer]])(
        _ ~
        bipush(0) ~
        dup ~
        (_.l.store.e) ~
        swap ~
        foldIterable(classOf[java.lang.Integer],_ ~ method(_.intValue) ~ iadd) ~
        method(Integer.valueOf(_))
      )
      
      type SF[T<:List,U<:List,L<:List] = F[T,L] => F[U,L] 
      
      trait StackFunc1[T,U]{
        def f[R<:List,L<:List]:SF[R**T,R**U,L]
      }
      def sf[T,U](func:F[Nil**T,Nil] => F[Nil**U,Nil]):StackFunc1[T,U] =
        new StackFunc1[T,U]{
          def f[R<:List,L<:List]:SF[R**T,R**U,L] = func.asInstanceOf[SF[R**T,R**U,L]]
        }
      
      val toString = sf[java.lang.Integer,String](method(_.toString))
      
      val empty:F[Nil,Nil] = null
      
      val x = 
        sf[Int,String](_ ~
          method(java.lang.Integer.valueOf(_)) ~
            toString.f)
      
      val test:String = x       
      
      System.out.println(func2(Array(5,10,3,5,2)))
      System.out.println(func3(java.util.Arrays.asList(12,4,2,6,3,7,3)))
      
  }
}

/*
Description	Resource	Path	Location	Type
type mismatch;
 found   : (net.virtualvoid.bytecode.Bytecode.F[net.virtualvoid.bytecode.Bytecode.**[net.virtualvoid.bytecode.Bytecode.**[net.virtualvoid.bytecode.Bytecode.Nil,Int],java.lang.Iterable[Integer]],net.virtualvoid.bytecode.Bytecode.**[net.virtualvoid.bytecode.Bytecode.Nil,Int]]) => net.virtualvoid.bytecode.Bytecode.F[net.virtualvoid.bytecode.Bytecode.**[net.virtualvoid.bytecode.Bytecode.Nil,Int],net.virtualvoid.bytecode.Bytecode.**[net.virtualvoid.bytecode.Bytecode.Nil,java.util.Iterator[Integer]]]
 required: (net.virtualvoid.bytecode.Bytecode.F[net.virtualvoid.bytecode.Bytecode.**[net.virtualvoid.bytecode.Bytecode.**[net.virtualvoid.bytecode.Bytecode.Nil,java.lang.Iterable[Integer]],Int],net.virtualvoid.bytecode.Bytecode.**[net.virtualvoid.bytecode.Bytecode.Nil,Int]]) => ?	BytecodeCompilerSpecs.scala	bytecode/src/test/scala/net/virtualvoid/bytecode	Unknown	Scala Problem

*/

import org.specs.runner.JUnit4

class MyCompilerSpecTest extends JUnit4(BytecodeCompilerSpecs)