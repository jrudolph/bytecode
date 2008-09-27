package net.virtualvoid.bytecode

import org.specs._

object BytecodeCompilerSpecs extends Specification{
  def compiledTests(compiler:net.virtualvoid.bytecode.v2.Bytecode.ByteletCompiler){
    import net.virtualvoid.bytecode.v2.Bytecode._
    import net.virtualvoid.bytecode.v2.Bytecode.Implicits._
    
    "bipush(20)" in {
      compiler.compile(classOf[String])(_.pop.bipush(20).method(Integer.valueOf(_)))
        .apply("Test") must be_==(20)}
    "method(_.length)" in {
      compiler.compile(classOf[String])(_.method(_.length).method(Integer.valueOf(_)))
        .apply("Test") must be_==(4)}
    "locals + method2" in {
      compiler.compile(classOf[java.lang.String])(_.l.store.e.l.load.e.l.load.e.method2(_.concat(_)))
      .apply("Test") must be_==("TestTest")}
    "iadd" in {
      compiler.compile(classOf[java.lang.Integer])(_.method(_.intValue).dup.iadd.bipush(3).iadd.method(Integer.valueOf(_)))
      .apply(12) must be_==(27)}
    "store int in locals" in {
      compiler.compile(classOf[java.lang.Integer])(_.method(_.intValue).dup.l.store.e.l.load.e.iadd.method(Integer.valueOf(_)))
      .apply(12) must be_==(24)}
    "store double in locals" in {
      compiler.compile(classOf[java.lang.Double])(_.method(_.doubleValue).l.store.e.l.load.e.method(java.lang.Double.valueOf(_)))
      .apply(12.453) must be_==(12.453)}
    "store double after method2" in {
      compiler.compile(classOf[java.lang.Double])(_.method(_.doubleValue).ldc("test").dup.method2(_.concat(_)).pop.l.store.e.l.load.e.method(java.lang.Double.valueOf(_)))
      .apply(12.453) must be_==(12.453)}
    "store something more than 1 level deep" in {
      compiler.compile(classOf[String])(_.l.l.store.e.e.l.l.load.e.e)
      .apply("test") must be_==("test")
    }
    "load element with index 1 from a string array" in {
      compiler.compile(classOf[Array[String]])(_.bipush(1).aload)
      .apply(array("That","is","a","Test")) must be_==("is")
    }
    "save string element to array and load it afterwards" in {
      compiler.compile(classOf[Array[String]])(_.dup.bipush(1).ldc("test").astore.bipush(1).aload)
      .apply(array("That","is","a","Test")) must be_==("test")
    }
    "save int element to array and load it afterwards" in {
      compiler.compile(classOf[Array[Int]])(_.dup.bipush(1).bipush(13).astore.bipush(1).aload.dup.iadd.method(Integer.valueOf(_)))
      .apply(array(1,2,3,4)) must be_==(26)
    }
    "get array length" in {
      compiler.compile(classOf[Array[String]])(_.arraylength.method(Integer.valueOf(_)))
      .apply(array("That","is","a","problem")) must be_==(4)
    }
    "isub" in {
      compiler.compile(classOf[java.lang.Integer])(_.method(_.intValue).bipush(3).isub.method(Integer.valueOf(_)))
      .apply(12) must be_==(9)
    }
    "dup_x1" in {
      compiler.compile(classOf[java.lang.Integer])(_.dup.method(_.toString).swap.method(_.intValue).dup_x1.swap.pop.iadd.method(Integer.valueOf(_)))
      .apply(12) must be_==(24)
    }
    "create new StringBuilder" in {
      compiler.compile(classOf[java.lang.String])(_.dup.newInstance(classOf[java.lang.StringBuilder]).swap.method2(_.append(_)).swap.method2(_.append(_)).method(_.toString))
      .apply("test") must be_==("testtest") 
    }
  }
  def array(els:Int*):Array[Int] = Array(els:_*)
  def array(els:String*):Array[String] = Array(els:_*)
  
  "Compiler" should {
    "succeed in generic Tests" in compiledTests(net.virtualvoid.bytecode.v2.Bytecode.ASMCompiler)
  }
  "Interpreter" should {
    "succeed in generic Tests" in compiledTests(net.virtualvoid.bytecode.v2.Bytecode.Interpreter)
  }
}

import org.specs.runner.JUnit4

class MyCompilerSpecTest extends JUnit4(BytecodeCompilerSpecs)