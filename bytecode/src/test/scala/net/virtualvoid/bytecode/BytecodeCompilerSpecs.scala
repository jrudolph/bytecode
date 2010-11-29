package net.virtualvoid.bytecode

import org.specs._

object BytecodeCompilerSpecs extends Specification{
  def compiledTests(compiler: net.virtualvoid.bytecode.backend.ByteletCompiler) {
    import Bytecode._
    import Bytecode.Implicits._
    import Bytecode.Instructions._
    import Methods._
    
    val boxInt:Method1[Int,java.lang.Integer] = method1(Integer.valueOf(_:Int))
    val unboxInt:Method1[java.lang.Integer,Int] = method1((_:java.lang.Integer).intValue)
    
    val boxDouble:Method1[Double,java.lang.Double] = method1(java.lang.Double.valueOf(_:Double))
    val unboxDouble:Method1[java.lang.Double,Double] = method1((_:java.lang.Double).doubleValue)
    
    val concat = method2((s1:String,s2:String)=> s1.concat(s2))
    
    val toString = method1((_:AnyRef).toString)
    val append = method2((_:java.lang.StringBuilder).append(_:CharSequence))
    
    "bipush(20)" in {
      compiler.compile(classOf[String])(str => _~bipush(20)~boxInt)
        .apply("Test") must be_==(20)}
    "invokemethod1(_.length)" in {
      compiler.compile(classOf[String])(str => _~str.load~method1((_:String).length)~boxInt)
        .apply("Test") must be_==(4)}
    "locals + method2" in {
      compiler.compile(classOf[java.lang.String])(p => _ ~ p.load ~ withLocal{ str => _ ~ str.load ~ str.load ~ concat})
      .apply("Test") must be_==("TestTest")}
    "iadd with operations" in {
      compiler.compile(classOf[java.lang.Integer])( i =>
        _ ~ i.load ~ unboxInt ~ dup
        ~ iadd
        ~ boxInt
      ).apply(12) must be_==(24)
    }
    "iadd" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~unboxInt~dup~iadd~bipush(3)~iadd~boxInt)
      .apply(12) must be_==(27)}
    "store(_) int in locals" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~unboxInt~dup~withLocal{i=> _ ~ i.load}~iadd~boxInt)
      .apply(12) must be_==(24)}
    "store(_) double in locals" in {
      compiler.compile(classOf[java.lang.Double])(i => _~i.load~unboxDouble~withLocal{d=>d.load}~boxDouble)
      .apply(12.453) must be_==(12.453)}
    "store(_) double after method2" in {
      compiler.compile(classOf[java.lang.Double])(i => _~i.load~unboxDouble~ldc("test")~dup~concat~pop~withLocal{d=>d.load}~boxDouble)
      .apply(12.453) must be_==(12.453)}
    "store Int after double" in {
      import java.lang.{Double => jDouble}
      compiler.compile(classOf[java.lang.Double])(dO => 
        _ ~
          dO.load ~
          unboxDouble ~
          withLocal(d =>
            _ ~
              bipush(5) ~
              withLocal(i =>
                _ ~
                  d.load ~
                  boxDouble))
      ).apply(.753) must be_==(.753)
    }
    "store Int after double, replace double by String, access int" in {
      compiler.compile(classOf[java.lang.Double])(dO =>
        _ ~
          dO.load ~
          unboxDouble ~
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
          boxInt
      ).apply(.753) must be_==(5)
    } 
    "load element with index 1 from a string array" in {
      compiler.compile(classOf[Array[String]])(ar => _~ar.load~bipush(1)~aload)
      .apply(Array("That","is","a","Test")) must be_==("is")
    }
    "save string element to array and load it afterwards" in {
      compiler.compile(classOf[Array[String]])(ar => _~ar.load~dup~bipush(1)~ldc("test")~astore~bipush(1)~aload)
      .apply(Array("That","is","a","Test")) must be_==("test")
    }
    "save int element to array and load it afterwards" in {
      compiler.compile(classOf[Array[Int]])(ar=>_~ar.load~dup~bipush(1)~bipush(13)~astore~bipush(1)~aload~dup~iadd~boxInt)
      .apply(Array(1,2,3,4)) must be_==(26)
    }
    "get array length" in {
      compiler.compile(classOf[Array[String]])(ar=>_~ar.load~arraylength~boxInt)
      .apply(Array("That","is","a","problem")) must be_==(4)
    }
    "isub" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~unboxInt~bipush(3)~isub~boxInt)
      .apply(12) must be_==(9)
    }
    "dup_x1" in {
      compiler.compile(classOf[java.lang.Integer])(i => _~i.load~dup~toString~swap()~unboxInt~dup_x1~swap()~pop~iadd~boxInt)
      .apply(12) must be_==(24)
    }
    "create new StringBuilder" in {
      compiler.compile(classOf[java.lang.String])(str => _~str.load~dup~newInstance(classOf[java.lang.StringBuilder])~swap()~append~swap()~append~toString)
      .apply("test") must be_==("testtest") 
    }
    "create new StringBuilder with parameter" in {
      val ctor = ctor1(new java.lang.StringBuilder(_: String))

      compiler.compile(classOf[java.lang.String])(str => _~str.load~ctor()~toString)
      .apply("test") must be_==("test") 
    }
    "store(_) string after void method" in {
      compiler.compile(classOf[java.lang.String])(str => 
        _ ~ 
          str.load ~ 
          newInstance(classOf[java.text.SimpleDateFormat]) ~ 
          ldc("yyyy") ~ 
          method2((_:java.text.SimpleDateFormat).applyPattern(_:java.lang.String)) ~
          withLocal{str=>str.load})
      .apply("test") must be_==("test")
    }
    "scala parameterless method call" in {
      compiler.compile(classOf[Option[java.lang.String]])(str =>
        _ ~
          str.load ~
          method1((_:Option[java.lang.String]).isDefined) ~
          method1(java.lang.Boolean.valueOf(_:Boolean))
      )
      .apply(Some("x")) must be_==(true)
    }
    "method call to superclass method" in {
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          method1((_:java.lang.StringBuilder).length) ~
          boxInt
      )
      .apply(new java.lang.StringBuilder) must be_==(0)
    }
    "method2 call to method which accepts superclass" in {
      val sb = new java.lang.StringBuilder
      sb.append("test")
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          dup ~
          append // accepts CharSequence
      )
      .apply(sb).toString must be_==("testtest")
    }
    "getstatic StaticVariableContainer.x" in {
      StaticVariableContainer.x = 3263
      compiler.compile(classOf[java.lang.StringBuilder])( sb =>
        _ ~
          sb.load ~
          pop ~
          getstatic(() => StaticVariableContainer.x) ~
          boxInt
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
          boxInt
      )
      .apply(null) must be_==(38)
    }
    "returning out of branch" in {
      val f:java.lang.Integer => String = 
      compiler.compile(classOf[java.lang.Integer],classOf[String])(input => ret =>
        _ ~
          input.load ~
          unboxInt ~
          ifne(_ ~ ldc(">0") ~ ret.jmp) ~ ldc("==0") ~ ret.jmp
      )
      f(15) must be_==(">0")
      f(0) must be_==("==0")
    }
    "ifeq2" in {
      val f = compiler.compile(classOf[java.lang.Integer])( i => 
        _ ~ 
          i.load ~
          unboxInt ~
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
          unboxInt ~
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
          boxInt
      )
      f(Array(1,2,3,4)) must be_==(10)
      f(Array(5,17,12,3,28)) must be_==(65)
    }
    "foldIterator" in {
      val f = compiler.compile(classOf[java.util.Iterator[String]])( it =>
        _ ~
          it.load ~
          newInstance(classOf[java.lang.StringBuilder]) ~
          RichOperations.foldIterator(it => _ ~ append) ~
          toString
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
                toString ~
               	ldc(" isnotnull") ~
                concat)
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
                toString ~
               	ldc(" isnotnull") ~
                concat,
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
          concat ~
          ret.jmp
      )("String1","String2") must be_==("String1String2")
    }
    "dynamic unary method invocation" in {
      import java.lang.{Integer => jInt}
      val intValue = dynMethod[jInt,Int](classOf[jInt].getMethod("intValue"))
      compiler.compile(classOf[jInt])(i =>
        _ ~
          i.load ~
          intValue ~
          method1(jInt.toString(_:Int))
      ).apply(5) must be_==("5")
    }
    "dynamic binary method invocation" in {
      val intValue = dynMethod[String,String,String](classOf[String].getMethod("concat",classOf[String]))
      compiler.compile(classOf[String])(str =>
        _ ~
          str.load ~
          dup ~
          intValue
      ).apply("Test") must be_==("TestTest")
    } 
  }
  
  "Compiler" should {
    "succeed in generic Tests" in compiledTests(net.virtualvoid.bytecode.backend.ASM)
  }
  "Interpreter" should {
    "succeed in generic Tests" in compiledTests(net.virtualvoid.bytecode.backend.Interpreter)
  }
  "Dynamic method type checking" should {
    import Methods.dynMethod
    "work with static methods" in {
      dynMethod[Int,java.lang.Integer](classOf[Integer].getMethod("valueOf",classOf[Int])).method.getName must be_==("valueOf")
    }
    "work with instance methods" in {
      dynMethod[java.lang.Integer,String](classOf[Integer].getMethod("toString")).method.getName must be_==("toString")
    }
    "comply with subtyping rules" in {
      dynMethod[java.lang.Integer,String](classOf[Number].getMethod("toString")).method.getName must be_==("toString")
      dynMethod[Number,String](classOf[java.lang.Integer].getMethod("toString")) must throwA[RuntimeException]
    }
    "throw if parameter count doesn't match" in {
      dynMethod[String,java.lang.Integer](classOf[String].getMethod("concat",classOf[String])) must throwA[RuntimeException]
      dynMethod[String,java.lang.Integer](classOf[Runtime].getMethod("getRuntime")) must throwA[RuntimeException]
    }
    "work for binary static methods" in {
      dynMethod[Int,Int,String](classOf[java.lang.Integer].getMethod("toString",classOf[Int],classOf[Int])).method.getName must be_==("toString")
    }
    "work for binary instance methods" in {
      dynMethod[String,String,String](classOf[String].getMethod("concat",classOf[String])).method.getName must be_==("concat")
    }
  }
}
