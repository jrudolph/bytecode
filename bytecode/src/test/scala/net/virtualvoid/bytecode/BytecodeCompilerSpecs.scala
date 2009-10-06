package net.virtualvoid.bytecode

import _root_.org.specs._

object BytecodeCompilerSpecs extends Specification{
  def compiledTests(compiler:net.virtualvoid.bytecode.Bytecode.ByteletCompiler){
    import Bytecode._
    import Bytecode.Implicits._
    import Bytecode.Instructions._
    
    val boxInt:Method1[Int,java.lang.Integer] = method1(Integer.valueOf(_:Int))
    val unboxInt:Method1[java.lang.Integer,Int] = method1((_:java.lang.Integer).intValue)
    
    val boxDouble:Method1[Double,java.lang.Double] = method1(java.lang.Double.valueOf(_:Double))
    val unboxDouble:Method1[java.lang.Double,Double] = method1((_:java.lang.Double).doubleValue)
    
    val concat = method2((s1:String,s2:String)=> s1.concat(s2))
    
    val toString = method1((_:AnyRef).toString)
    val append = method2((_:java.lang.StringBuilder).append(_:CharSequence))
    
    "bipush(20)" in {
      compiler.compile(classOf[String])(str => _~bipush(20)~boxInt.invoke())
        .apply("Test") must be_==(20)}
    "invokemethod1(_.length)" in {
      compiler.compile(classOf[String])(str => _~str.load~method1((_:String).length).invoke()~boxInt.invoke())
        .apply("Test") must be_==(4)}
    "locals + method2" in {
      compiler.compile(classOf[java.lang.String])(p => _ ~ p.load ~ withLocal{ str => _ ~ str.load ~ str.load ~ concat.invoke()})
      .apply("Test") must be_==("TestTest")}
    "iadd with operations" in {
      compiler.compile(classOf[java.lang.Integer])( i =>
        _ ~ i.load ~ unboxInt.invoke() ~ dup
        ~ iadd
        ~ boxInt.invoke()
      ).apply(12) must be_==(24)
    }
    "iadd" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~unboxInt.invoke()~dup~iadd~bipush(3)~iadd~boxInt.invoke())
      .apply(12) must be_==(27)}
    "store(_) int in locals" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~unboxInt.invoke()~dup~withLocal{i=> _ ~ i.load}~iadd~boxInt.invoke())
      .apply(12) must be_==(24)}
    "store(_) double in locals" in {
      compiler.compile(classOf[java.lang.Double])(i => _~i.load~unboxDouble.invoke()~withLocal{d=>d.load}~boxDouble.invoke())
      .apply(12.453) must be_==(12.453)}
    "store(_) double after method2" in {
      compiler.compile(classOf[java.lang.Double])(i => _~i.load~unboxDouble.invoke()~ldc("test")~dup~concat.invoke()~pop~withLocal{d=>d.load}~boxDouble.invoke())
      .apply(12.453) must be_==(12.453)}
    "store Int after double" in {
      import java.lang.{Double => jDouble}
      compiler.compile(classOf[java.lang.Double])(dO => 
        _ ~
          dO.load ~
          unboxDouble.invoke() ~
          withLocal(d =>
            _ ~
              bipush(5) ~
              withLocal(i =>
                _ ~
                  d.load ~
                  boxDouble.invoke()))
      ).apply(.753) must be_==(.753)
    }
    "store Int after double, replace double by String, access int" in {
      compiler.compile(classOf[java.lang.Double])(dO =>
        _ ~
          dO.load ~
          unboxDouble.invoke() ~
          withLocal(d =>
            _ ~
              bipush(5) ~
              withLocal(i =>
                _ ~
                  d.load ~ 
                  pop ~ 
                  ldc("test") ~ 
                  withLocal(str => f => f) ~
                  i.load)) ~
          boxInt.invoke()
      ).apply(.753) must be_==(5)
    } 
    "load element with index 1 from a string array" in {
      compiler.compile(classOf[Array[String]])(ar => _~ar.load~bipush(1)~aload)
      .apply(array("That","is","a","Test")) must be_==("is")
    }
    "save string element to array and load it afterwards" in {
      compiler.compile(classOf[Array[String]])(ar => _~ar.load~dup~bipush(1)~ldc("test")~astore~bipush(1)~aload)
      .apply(array("That","is","a","Test")) must be_==("test")
    }
    "save int element to array and load it afterwards" in {
      compiler.compile(classOf[Array[Int]])(ar=>_~ar.load~dup~bipush(1)~bipush(13)~astore~bipush(1)~aload~dup~iadd~boxInt.invoke())
      .apply(array(1,2,3,4)) must be_==(26)
    }
    "get array length" in {
      compiler.compile(classOf[Array[String]])(ar=>_~ar.load~arraylength~boxInt.invoke())
      .apply(array("That","is","a","problem")) must be_==(4)
    }
    "isub" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~unboxInt.invoke()~bipush(3)~isub~boxInt.invoke())
      .apply(12) must be_==(9)
    }
    "dup_x1" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~dup~toString.invoke()~swap()~unboxInt.invoke()~dup_x1~swap()~pop~iadd~boxInt.invoke())
      .apply(12) must be_==(24)
    }
    "create new StringBuilder" in {
      compiler.compile(classOf[java.lang.String])(str => _~str.load~dup~newInstance(classOf[java.lang.StringBuilder])~swap()~append.invoke()~swap()~append.invoke()~toString.invoke())
      .apply("test") must be_==("testtest") 
    }
    "store(_) string after void method" in {
      compiler.compile(classOf[java.lang.String])(str => 
        _ ~ 
          str.load ~ 
          newInstance(classOf[java.text.SimpleDateFormat]) ~ 
          ldc("yyyy") ~ 
          method2((_:java.text.SimpleDateFormat).applyPattern(_:java.lang.String)).invokeUnit() ~
          withLocal{str=>str.load})
      .apply("test") must be_==("test")
    }
    "scala parameterless method call" in {
      compiler.compile(classOf[Option[java.lang.String]])(str =>
        _ ~
          str.load ~
          method1((_:Option[java.lang.String]).isDefined).invoke() ~
          method1(java.lang.Boolean.valueOf(_:Boolean)).invoke()
      )
      .apply(Some("x")) must be_==(true)
    }
    "method call to superclass method" in {
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          method1((_:java.lang.StringBuilder).length).invoke() ~
          boxInt.invoke()
      )
      .apply(new java.lang.StringBuilder) must be_==(0)
    }
    "method2 call to method which accepts superclass" in {
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          dup ~
          append.invoke() // accepts CharSequence
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
          boxInt.invoke()
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
          boxInt.invoke()
      )
      .apply(null) must be_==(38)
    }
    "ifne and jmp" in {
      //	if (compiler != Interpreter)
      compiler.compile(classOf[java.lang.Integer])(input =>
        // sums all integers from 0 to i
        _ ~ input.load ~
          	unboxInt.invoke() ~
          	withLocal(i => _ ~          	
                bipush(0) ~
                withLocal(sum => _ ~
                  withTargetHere(start => _ ~ 
                    i.load ~ 
                    ifne(
                      _ ~ 
                        i.load ~
                        dup ~
                        bipush(1) ~
                        isub ~
                        i.store ~
                        sum.load ~
                        iadd ~
                        sum.store ~
                        start.jmp
                    ) ~
                    sum.load ~
                    boxInt.invoke()
      )))).apply(5) must be_==(15)
    }
    "returning out of branch" in {
      val f:java.lang.Integer => String = 
      compiler.compile(classOf[java.lang.Integer],classOf[String])(input => ret =>
        _ ~
          input.load ~
          unboxInt.invoke() ~
          ifne(_ ~ ldc(">0") ~ ret.jmp) ~ ldc("==0") ~ ret.jmp
      )
      f(15) must be_==(">0")
      f(0) must be_==("==0")
    }
    "ifeq2" in {
      val f = compiler.compile(classOf[java.lang.Integer])( i => 
        _ ~ 
          i.load ~
          unboxInt.invoke() ~
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
          unboxInt.invoke() ~
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
          bipush(0) ~
          RichOperations.foldArray(array)(index => iadd) ~
          boxInt.invoke()
      )
      f(Array(1,2,3,4)) must be_==(10)
      f(Array(5,17,12,3,28)) must be_==(65)
    }
    "foldIterator" in {
      val f = compiler.compile(classOf[java.util.Iterator[String]])( it =>
        _ ~
          it.load ~
          newInstance(classOf[java.lang.StringBuilder]) ~
          RichOperations.foldIterator(it => _ ~ append.invoke()) ~
          toString.invoke()
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
                toString.invoke() ~
               	ldc(" isnotnull") ~
                concat.invoke())
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
                toString.invoke() ~
               	ldc(" isnotnull") ~
                concat.invoke(),
             _ ~ pop ~ ldc("isnull"))
      )
      f(null) must be_==("isnull")
      f("blub") must be_==("blub isnotnull")
    }
    "binary methods" in {
      compiler.compile(classOf[String],classOf[String],classOf[String])((str1,str2) => ret =>
        _ ~
          str1.load ~
          str2.load ~
          concat.invoke() ~
          ret.jmp
      )("String1","String2") must be_==("String1String2")
    }
    "dynamic unary method invocation" in {
      import java.lang.{Integer => jInt}
      val intValue = dynMethod[jInt,Int](classOf[jInt].getMethod("intValue"))
      compiler.compile(classOf[jInt])(i =>
        _ ~
          i.load ~
          intValue.invoke() ~
          method1(jInt.toString(_:Int)).invoke()
      ).apply(5) must be_==("5")
    }
    "dynamic binary method invocation" in {
      val intValue = dynMethod[String,String,String](classOf[String].getMethod("concat",classOf[String]))
      compiler.compile(classOf[String])(str =>
        _ ~
          str.load ~
          dup ~
          intValue.invoke()
      ).apply("Test") must be_==("TestTest")
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
  "Dynamic method type checking" should {
    import Bytecode.dynMethod
    "work with static methods" in {
      dynMethod[Int,java.lang.Integer](classOf[Integer].getMethod("valueOf",classOf[Int]))
    }
    "work with instance methods" in {
      dynMethod[java.lang.Integer,String](classOf[Integer].getMethod("toString"))
    }
    "comply with subtyping rules" in {
      dynMethod[java.lang.Integer,String](classOf[Number].getMethod("toString"))
      dynMethod[Number,String](classOf[java.lang.Integer].getMethod("toString")) must throwA[RuntimeException]
    }
    "throw if parameter count doesn't match" in {
      dynMethod[String,java.lang.Integer](classOf[String].getMethod("concat",classOf[String])) must throwA[RuntimeException]
      dynMethod[String,java.lang.Integer](classOf[Runtime].getMethod("getRuntime")) must throwA[RuntimeException]
    }
    "work for binary static methods" in {
      dynMethod[Int,Int,String](classOf[java.lang.Integer].getMethod("toString",classOf[Int],classOf[Int]))
    }
    "work for binary instance methods" in {
      dynMethod[String,String,String](classOf[String].getMethod("concat",classOf[String]))
    }
  }
}

import org.specs.runner.JUnit4

class MyCompilerSpecTest extends JUnit4(BytecodeCompilerSpecs)