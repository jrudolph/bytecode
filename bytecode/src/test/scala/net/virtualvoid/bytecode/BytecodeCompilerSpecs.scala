package net.virtualvoid.bytecode

import _root_.org.specs._

object BytecodeCompilerSpecs extends Specification{
  def compiledTests(compiler:net.virtualvoid.bytecode.Bytecode.ByteletCompiler){
    import Bytecode._
    import Bytecode.Implicits._
    import Bytecode.Instructions._
    
    "bipush(20)" in {
      compiler.compile(classOf[String])(str => _~bipush(20)~invokemethod1(Integer.valueOf(_)))
        .apply("Test") must be_==(20)}
    "invokemethod1(_.length)" in {
      compiler.compile(classOf[String])(str => _~str.load~invokemethod1(_.length)~invokemethod1(Integer.valueOf(_)))
        .apply("Test") must be_==(4)}
    "locals + method2" in {
      compiler.compile(classOf[java.lang.String])(p => _ ~ p.load ~ withLocal{ str => _ ~ str.load ~ str.load ~ invokemethod2(_.concat(_))})
      .apply("Test") must be_==("TestTest")}
    "iadd with operations" in {
      compiler.compile(classOf[java.lang.Integer])( i =>
        _ ~ i.load ~ invokemethod1(_.intValue) ~ dup
        ~ iadd
        ~ invokemethod1(Integer.valueOf(_))
      ).apply(12) must be_==(24)
    }
    "iadd" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~invokemethod1(_.intValue)~dup~iadd~bipush(3)~iadd~invokemethod1(Integer.valueOf(_)))
      .apply(12) must be_==(27)}
    "store(_) int in locals" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~invokemethod1(_.intValue)~dup~withLocal{i=> _ ~ i.load}~iadd~invokemethod1(Integer.valueOf(_)))
      .apply(12) must be_==(24)}
    "store(_) double in locals" in {
      compiler.compile(classOf[java.lang.Double])(i => _~i.load~invokemethod1(_.doubleValue)~withLocal{d=>d.load}~invokemethod1(java.lang.Double.valueOf(_)))
      .apply(12.453) must be_==(12.453)}
    "store(_) double after method2" in {
      compiler.compile(classOf[java.lang.Double])(i => _~i.load~invokemethod1(_.doubleValue)~ldc("test")~dup~invokemethod2(_.concat(_))~pop~withLocal{d=>d.load}~invokemethod1(java.lang.Double.valueOf(_:Double)))
      .apply(12.453) must be_==(12.453)}
    "load element with index 1 from a string array" in {
      compiler.compile(classOf[Array[String]])(ar => _~ar.load~bipush(1)~aload)
      .apply(array("That","is","a","Test")) must be_==("is")
    }
    "save string element to array and load it afterwards" in {
      compiler.compile(classOf[Array[String]])(ar => _~ar.load~dup~bipush(1)~ldc("test")~astore~bipush(1)~aload)
      .apply(array("That","is","a","Test")) must be_==("test")
    }
    "save int element to array and load it afterwards" in {
      compiler.compile(classOf[Array[Int]])(ar=>_~ar.load~dup~bipush(1)~bipush(13)~astore~bipush(1)~aload~dup~iadd~invokemethod1(Integer.valueOf(_)))
      .apply(array(1,2,3,4)) must be_==(26)
    }
    "get array length" in {
      compiler.compile(classOf[Array[String]])(ar=>_~ar.load~arraylength~invokemethod1(Integer.valueOf(_)))
      .apply(array("That","is","a","problem")) must be_==(4)
    }
    "isub" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~invokemethod1(_.intValue)~bipush(3)~isub~invokemethod1(Integer.valueOf(_)))
      .apply(12) must be_==(9)
    }
    "dup_x1" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~dup~invokemethod1(_.toString)~swap()~invokemethod1(_.intValue)~dup_x1~swap()~pop~iadd~invokemethod1(Integer.valueOf(_)))
      .apply(12) must be_==(24)
    }
    "create new StringBuilder" in {
      compiler.compile(classOf[java.lang.String])(str => _~str.load~dup~newInstance(classOf[java.lang.StringBuilder])~swap()~invokemethod2(_.append(_))~swap()~invokemethod2(_.append(_))~invokemethod1(_.toString))
      .apply("test") must be_==("testtest") 
    }
    "store(_) string after void method" in {
      compiler.compile(classOf[java.lang.String])(str => _~str.load~newInstance(classOf[java.text.SimpleDateFormat]) ~ ldc("yyyy") ~ invokemethod2(_.applyPattern(_)) ~ pop_unit ~ withLocal{str=>str.load})
      .apply("test") must be_==("test")
    }
    "scala parameterless method call" in {
      compiler.compile(classOf[Option[java.lang.String]])(str =>
        _ ~
          str.load ~
          invokemethod1(_.isDefined) ~
          invokemethod1(java.lang.Boolean.valueOf(_))
      )
      .apply(Some("x")) must be_==(true)
    }
    "method call to superclass method" in {
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          invokemethod1(_.length) ~
          invokemethod1(Integer.valueOf(_))
      )
      .apply(new java.lang.StringBuilder) must be_==(0)
    }
    "method2 call to method which accepts superclass" in {
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          dup ~
          invokemethod2(_.append(_)) // accepts CharSequence
      )
      .apply(new java.lang.StringBuilder)
    }
    "getstatic StaticVariableContainer.x" in {
      StaticVariableContainer.x = 3263
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          pop ~
          getstatic(() => StaticVariableContainer.x) ~
          invokemethod1(java.lang.Integer.valueOf(_))
      )
      .apply(null) must be_==(3263)
    }
    "putstatic StaticVariableContainer.x" in {
      StaticVariableContainer.x = 15
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          bipush(38) ~
          putstatic(StaticVariableContainer.x = _) ~
          pop ~
          getstatic(() => StaticVariableContainer.x) ~
          invokemethod1(java.lang.Integer.valueOf(_))
      )
      .apply(null) must be_==(38)
    }
    /*"ifeq and jmp" in {
      if (compiler != Interpreter)
      compiler.compile(classOf[java.lang.Integer])(
        // sums all integers from 0 to i
        f => {
          val start = f ~
            invokemethod1(_.intValue) ~
            local[_0,Int].store() ~ //  store(_) current i in local 0
            bipush(0) ~
            local[_1,Int].store() ~ //  store(_) sum in local 1
            target
          
          start ~
            local[_0,Int].load() ~ // load i to check if it is already 0 
            ifne(f => 
              f ~ 
                local[_0,Int].load() ~
                dup ~
                bipush(1) ~
                isub ~
                local[_0,Int].store() ~
                local[_1,Int].load() ~
                iadd ~
                local[_1,Int].store() ~
                jmp(start)
            ) ~
            local[_1,Int].load() ~
            invokemethod1(Integer.valueOf(_))
      }).apply(5) must be_==(15)
    }*/
    "ifeq2" in {
      val f = compiler.compile(classOf[java.lang.Integer])( i => 
        _ ~ 
          i.load ~
          invokemethod1(_.intValue) ~
          bipush(5) ~
          isub ~
          ifeq2(
            _ ~
              ldc("equals 5")
            ,_ ~
              ldc("does not equal 5")
          )
      )
      f(10) must be_==("does not equal 5")
      f(5) must be_==("equals 5")
    }
    "ifne2" in {
      val f = compiler.compile(classOf[java.lang.Integer])( i =>
        _ ~ 
          i.load ~
          invokemethod1(_.intValue) ~
          bipush(5) ~
          isub ~
          ifne2(
            _ ~
              ldc("does not equal 5")
            ,_ ~
              ldc("equals 5")
          )
      )
      f(10) must be_==("does not equal 5")
      f(5) must be_==("equals 5")
    }
    "foldArray" in {
      val f = compiler.compile(classOf[Array[Int]])( array =>
        _ ~
          array.load ~
          bipush(0) ~
          RichOperations.foldArray(index => iadd) ~
          invokemethod1(Integer.valueOf(_))
      )
      f(Array(1,2,3,4)) must be_==(10)
      f(Array(5,17,12,3,28)) must be_==(65)
    }
    "foldIterator" in {
      val f = compiler.compile(classOf[java.util.Iterator[String]])( it =>
        _ ~
          it.load ~
          newInstance(classOf[java.lang.StringBuilder]) ~
          RichOperations.foldIterator(it => _ ~ invokemethod2(_.append(_))) ~
          invokemethod1(_.toString)
      )
      f(java.util.Arrays.asList("a","b","c").iterator) must be_==("abc")
    }
    "ifnull" in {
      val f = compiler.compile(classOf[AnyRef])( o =>
          _ ~ 
            o.load ~
            dup ~
            ifnull(
              _ ~ pop ~ ldc("isnull")
             ,_ ~ 
                invokemethod1(_.toString) ~
               	ldc(" isnotnull") ~
                invokemethod2(_.concat(_)))
      )
      f(null) must be_==("isnull")
      f("blub") must be_==("blub isnotnull")
    }
    "ifnonnull" in {
      val f = compiler.compile(classOf[AnyRef])( o =>
          _ ~ 
            o.load ~
            dup ~
            ifnonnull(
             _ ~ 
                invokemethod1(_.toString) ~
               	ldc(" isnotnull") ~
                invokemethod2(_.concat(_)),
             _ ~ pop ~ ldc("isnull"))
      )
      f(null) must be_==("isnull")
      f("blub") must be_==("blub isnotnull")
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
}

import org.specs.runner.JUnit4

class MyCompilerSpecTest extends JUnit4(BytecodeCompilerSpecs)