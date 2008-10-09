package net.virtualvoid.bytecode

object Bytecode{
  import java.lang.{String => jString,
                    Boolean => jBoolean
  }

  trait List
  trait Nil extends List
  object N extends Nil
  
  case class Cons[+R<:List,+T](rest:R,top:T) extends List{
    def l = rest
  }
  trait Consable[T<:List]{
    def **[U](next:U): T**U
  }
  implicit def conser[T<:List](t:T) = new Consable[T]{
    def **[U](next:U): T**U = Cons(t,next)
  }

  // define an infix operator shortcut for the cons type
  type ** [x<:List,y] = Cons[x,y]

  trait Target[ST<:List,LT<:List] extends F[ST,LT]
  
  case class JVMInt(v:Int)

  trait Zippable[ST<:List,L<:List,Cur,R<:List]{
    def depth:Int
    def frame:F[ST,_]
  }
  
  case class Zipper[ST<:List,L<:List,Cur,R<:List](f:F[ST,_],depth:Int) extends Zippable[ST,L,Cur,R]{
    def ~[X](f:Zipper[ST,L,Cur,R]=>X) = f(this)
    def frame = f
  }
  
  trait F[ST<:List,LT<:List] extends Zippable[ST,LT,Nil,Nil]{
    def depth = -1
    def frame = this
    
    def stack:ST
    def locals:LT

    def bipush(i1:Int):F[ST**Int,LT]
    def ldc(str:jString):F[ST**jString,LT]
    def target:Target[ST,LT]
    def jmp(t:Target[ST,LT]):Nothing

    //def ~[STR<:List,LTR<:List](f:F[ST,LT]=>F[STR,LTR]):F[STR,LTR] = f(this)
    def ~[X](f:F[ST,LT]=>X):X = f(this)
    
    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def pop_int[R<:List](rest:R):F[R,LT]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT]
    def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2,LT]
    def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1,LT]
    def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def method_int[R<:List,T,U](rest:R,top:T,method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT]
    def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT]
    def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT]
    def ifeq_int[R<:List](rest:R,top:JVMInt,inner:F[R,LT] => Nothing):F[R,LT]
    def aload_int[R<:List,T](rest:R,array:AnyRef/*Array[T]*/,i:Int):F[R**T,LT]
    def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT]
    def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT]

    def newInstance[T](cl:Class[T]):F[ST**T,LT]
    
    def loadI[T](i:Int):F[ST**T,LT]
    def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT]
  }
  trait Int2Stack[ST<:List,LT<:List]{
    def i1:Int
    def i2:Int
    def rest:ST
    def frame:F[_,LT]
    def iadd():F[ST**Int,LT] = frame.iadd_int[ST](rest,i1,i2)
    def isub():F[ST**Int,LT] = frame.isub_int[ST](rest,i1,i2)
    def imul():F[ST**Int,LT] = frame.imul_int[ST](rest,i1,i2)
  }
  trait OneStack[R<:List,T,LT<:List]{
    def pop():F[R,LT]
    def dup():F[R**T**T,LT]
    def method[U](code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def dynMethod[U](method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT]
    def checkcast[U](cl:Class[U]):F[R**U,LT]
  }
  trait TwoStack[R<:List,T2,T1,LT<:List]{
    def method2[U](code:scala.reflect.Code[(T2,T1) => U]):F[R**U,LT]
    def swap():F[R**T1**T2,LT]
    def dup_x1():F[R**T1**T2**T1,LT]
  }
  /*class BooleanStack[R<:List,LT<:List,X](f:F[R**X,LT]){
    def ifeq(inner:F[R,LT] => Nothing):F[R,LT] =
      f.ifeq_int(f.stack.rest,f.stack.top,inner)
  }*/
  
  object Operations{
    def iop[R<:List,LT<:List](func:(F[R**Int**Int,LT],R,Int,Int)=>F[R**Int,LT]):
      F[R**Int**Int,LT] => F[R**Int,LT] = f => func(f,f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def iadd[R<:List,LT<:List] = 
      iop[R,LT](_.iadd_int(_,_,_))
    def imul[R<:List,LT<:List] = 
      iop[R,LT](_.imul_int(_,_,_))
    def isub[R<:List,LT<:List] = 
      iop[R,LT](_.isub_int(_,_,_))
    
    def method[T,U,R<:List,LT<:List](code:scala.reflect.Code[T=>U]):F[R**T,LT] => F[R**U,LT] = 
      f => f.method_int(f.stack.rest,f.stack.top,code)
    def method2[T1,T2,U,R<:List,LT<:List](code:scala.reflect.Code[(T1,T2)=>U]):
      F[R**T1**T2,LT] => F[R**U,LT] = f => f.method_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top,code)
    
    def pop[R<:List,LT<:List,T]:F[R**T,LT]=>F[R,LT] = f=>f.pop_int(f.stack.rest)
    def dup[R<:List,LT<:List,T]:F[R**T,LT]=>F[R**T**T,LT] = f => f.dup_int(f.stack.rest,f.stack.top)
    def dup_x1[R<:List,LT<:List,T2,T1]:F[R**T2**T1,LT] => F[R**T1**T2**T1,LT] = f => f.dup_x1_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def swap[R<:List,LT<:List,T2,T1]:F[R**T2**T1,LT] => F[R**T1**T2,LT] = f => f.swap_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    
    def checkcast[T,U,R<:List,LT<:List](cl:Class[U]):F[R**T,LT]=>F[R**U,LT] = f => f.checkcast_int(f.stack.rest,f.stack.top)(cl)
    
    def bipush[R<:List,LT<:List](i:Int):F[R,LT]=>F[R**Int,LT] = _.bipush(i)
    def ldc[R<:List,LT<:List](str:String):F[R,LT]=>F[R**String,LT] = _.ldc(str)
    
    def aload[R<:List,LT<:List,T]:F[R**Array[T]**Int,LT] => F[R**T,LT] = f=>f.aload_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    def astore[R<:List,LT<:List,T]:F[R**Array[T]**Int**T,LT] => F[R,LT] = f=>f.astore_int(f.stack.rest.rest.rest,f.stack.rest.rest.top,f.stack.rest.top,f.stack.top)
    def arraylength[R<:List,LT<:List,T]:F[R**Array[T],LT] => F[R**Int,LT] = f=>f.arraylength_int(f.stack.rest,f.stack.top)

    implicit def int2JVMInt(i:Int) = JVMInt(i)
    implicit def bool2JVMInt(b:Boolean) = JVMInt(if (b) 1 else 0)
    
    def ifeq[R<:List,LT<:List,T<%JVMInt](inner:F[R,LT]=>Nothing):F[R**T,LT] => F[R,LT] = f=>f.ifeq_int(f.stack.rest,f.stack.top,inner)
    def target[ST<:List,LT<:List] = (f:F[ST,LT]) => f.target
    def jmp[ST<:List,LT<:List](t:Target[ST,LT]) = (f:F[ST,LT]) => f.jmp(t)
    def newInstance[ST<:List,LT<:List,T](cl:Class[T]) = (f:F[ST,LT]) => f.newInstance(cl)
    
    def load[ST<:List,LT<:List,T,R](l:LT=>R**T):F[ST,LT]=>F[ST**T,LT] = f=>{
      var i = 0;
      val c:LT = new Cons(null.asInstanceOf[Cons[Nothing,Nothing]],null){
        override def l = {
          i+=1
          this.asInstanceOf[Cons[Nothing,Nothing]]
        }
      }.asInstanceOf[LT]
      l(c)
      f.loadI(i).asInstanceOf[F[ST**T,LT]]
    }
    def store[ST<:List,LT<:List,T,R,XT](l:(XT,T)=>LT):F[ST**T,XT] => F[ST,LT] = null
    
    def l0[R<:List,T]:R**T=>R**T = (f:R**T) => f
    def l1[R<:List,T2,T1]:R**T2**T1=>R**T2 = (f:R**T2**T1) => f.l
    def l2[R<:List,T3,T2,T1] = (f:R**T3**T2**T1) => f.l.l
    
    trait partial[F[_,_],x]{
      type f[y] = F[x,y]
    }
    
    trait partial2[F1[_,_],F2[_],y]{
      type f[x] = F1[F2[x],y]
    }
    
    trait Func[f[_]]{
      type applied[x] = f[x]
      
      def apply[X]:f[X]
      def next[X]:Func[partial2[Cons,f,X]#f]
    }
    
    def s0[R<:List,T]:(R**_,T)=>R**T = null
    def s1[R<:List,T1,T]: (R**_**T1,T)=>R**T**T1 = null
    def s2[R<:List,T1,T,T2]: (R**_**T2**T1,T)=>R**T**T2**T1 = null
    
    type wurst = Func[partial[Cons,Nil]#f]
    type wurst2 = wurst#applied[Int]
    val x:wurst2 = null
    val y:Nil**Int = x
    
    def t0[R<:List]:Func[partial[Cons,R]#f] = null
    def t1[R<:List,T] = t0[R].next[T]
    def t2[R<:List,T2,T1] = t0[R].next[T2].next[T1]
    
    //def store2[ST<:List,T,OLT<:List,LT<:List](func:(OLT,T)=>LT) : F[ST**T,OLT] => F[ST,LT] = null
    
    //def next[X[_],T](f:Func[X]):Func[partial2[Cons,X,T]#f] = null
    
    //val func = t1[Nil,Int]
    
    //val test:Nil**String**Int = func.apply[String]
    
    //def next[R<:List]:(R**_,T)=>R**T
    //implicit def convertL(f:X=>Y)
    
    val f:F[Nil**Int,Nil**String] = null
    val n:F[Nil,Nil**Int] = f ~ store(s0)
    
    val f2:F[Nil**Int,Nil**String**Double] = null
    val n2:F[Nil,Nil**Int**Double] = f2 ~ store(s1)
    
    //def store0 = f:F[ST**T,LT]
    
    /*val f:F[Nil,Nil**Int] = null
    f~load(l0)~dup~iadd
    
    val f2:F[Nil,Nil**String**Int] = null
    
    val x0:F[Nil**Int,Nil**String**Int] = f2~load(x=>x)
    val x:F[Nil**String,Nil**String**Int] = f2~load(l1)
    val f3:F[Nil,Nil**String**Int**Float] = null
    val y:F[Nil**String,Nil**String**Int**Float] = f3~load(l2)*/
/*               
java.lang.Error: 
Can't match this Function(List(LocalValue(NoSymbol,x$26,PrefixedType(SingleType(ThisType(Class(scala)),Field(scala.Predef,PrefixedType(ThisType(Class(scala)),Class(scala.Predef)))),
 TypeField(scala.Predef.String,PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String))))), 
 LocalValue(NoSymbol,x$27,PrefixedType(SingleType(ThisType(Class(scala)),Field(scala.Predef,PrefixedType(ThisType(Class(scala)),Class(scala.Predef)))),
 TypeField(scala.Predef.String,PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String))))))
 ,Apply(Select(Ident(LocalValue(NoSymbol,x$26,PrefixedType(SingleType(ThisType(Class(scala)),Field(scala.Predef,PrefixedType(ThisType(Class(scala)),Class(scala.Predef)))),TypeField(scala.Predef.String,PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String)))))),
 Method(java.lang.String.concat,MethodType(List(PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String))),PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String))))),List(Ident(LocalValue(NoSymbol,x$27,PrefixedType(SingleType(ThisType(Class(scala)),Field(scala.Predef,PrefixedType(ThisType(Class(scala)),Class(scala.Predef)))),TypeField(scala.Predef.String,PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String)))))))))
	at net.virtualvoid.bytecode.CodeTools$.methodFromCode(Tools.scala:27)
	at net.virtualvoid.bytecode.Interpreter$IF.method_int(Interpreter.scala:28)
	at net.virtualvoid.bytecode.Bytecode$Operations$$anonfun$method2$1.apply(Bytecode.scala:115)
	at net.virtualvoid.bytecode.Bytecode$Operations$$anonfun$method2$1.apply(Bytecode.scala:115)
	at net.virtualvoid.bytecode.Bytecode$F$class.$tilde(Bytecode.scala:52)
	at net.virtualvoid.bytecode.Interpreter$IF.$tilde(Interpreter.scala:8)
	at net.virtualvoid.bytecode.BytecodeCompilerSpecs$$anonfun$compiledTests$8$$anonfun$apply$32$$anonfun$apply$33.apply(BytecodeCompilerSpecs.scala:37)
	at net.virtualvoid.bytecode.BytecodeCompilerSpecs$$anonfun$compiledTests$8$$anonfun$apply$32$$anonfun$apply$33.apply(BytecodeCompilerSpecs.scala:37)
	at net.virtualvoid.bytecode.Interpreter$$anonfun$compile$1.apply(Interpreter.scala:61)
	at net.virtualvoid.bytecode.BytecodeCompilerSpecs$$anonfun$compiledTests$8$$anonfun$apply$32.apply(BytecodeCompilerSpecs.scala:38)
	at net.virtualvoid.bytecode.BytecodeCompilerSpecs$$anonfun$compiledTests$8$$anonfun$apply$32.apply(BytecodeCompilerSpecs.scala:38)
	at org.specs.matcher.AnyMatchers$$anon$3.apply(AnyMatchers.scala:48)
	at org.specs.specification.Assertable$class.executeMatch$1(Assert.scala:40)
	at org.specs.specification.Assertable$class.applyMatcher(Assert.scala:47)
	at org.specs.specification.Assert.applyMatcher(Assert.scala:68)
	at org.specs.specification.Assert.must(Assert.scala:72)
	at net.virtualvoid.bytecode.BytecodeCompilerSpecs$$anonfun$compiledTests$8.apply(BytecodeCompilerSpecs.scala:38)
	at net.virtualvoid.bytecode.BytecodeCompilerSpecs$$anonfun$compiledTests$8.apply(BytecodeCompilerSpecs.scala:38)
	at org.specs.specification.ExampleLifeCycle$class.executeTest(ExampleLifeCycle.scala:20)
	at org.specs.Specification.executeTest(Specification.scala:25)
	at org.specs.specification.Sut.executeTest(Sut.scala:99)
	at org.specs.specification.Example$$anonfun$2.apply(Example.scala:87)
	at org.specs.specification.Example$$anonfun$2.apply(Example.scala:74)
	at org.specs.specification.Example$$anonfun$in$1.apply(Example.scala:103)
	at org.specs.specification.Example.execute(Example.scala:116)
	at org.specs.specification.Example.subExamples(Example.scala:61)
	at org.specs.runner.ExamplesTestSuite$$anonfun$initialize$2.apply(JUnit.scala:114)
	at org.specs.runner.ExamplesTestSuite$$anonfun$initialize$2.apply(JUnit.scala:113)
	at scala.Iterator$class.foreach(Iterator.scala:410)
	at scala.collection.mutable.SingleLinkedList$$anon$1.foreach(SingleLinkedList.scala:50)
	at scala.Iterable$class.foreach(Iterable.scala:256)
	at scala.collection.mutable.Queue.foreach(Queue.scala:24)
	at org.specs.runner.ExamplesTestSuite.initialize(JUnit.scala:113)
	at org.specs.runner.JUnitSuite$class.init(JUnit.scala:30)
	at org.specs.runner.ExamplesTestSuite.init(JUnit.scala:106)
	at org.specs.runner.JUnitSuite$class.getName(JUnit.scala:39)
	at org.specs.runner.ExamplesTestSuite.getName(JUnit.scala:106)
	at org.specs.runner.TestDescription$class.asDescription(JUnitSuiteRunner.scala:79)
	at org.specs.runner.JUnitSuiteRunner.asDescription(JUnitSuiteRunner.scala:13)
	at org.specs.runner.TestDescription$class.makeDescription(JUnitSuiteRunner.scala:87)
	at org.specs.runner.JUnitSuiteRunner.makeDescription(JUnitSuiteRunner.scala:13)
	at org.specs.runner.TestDescription$$anonfun$makeDescription$1.apply(JUnitSuiteRunner.scala:89)
	at org.specs.runner.TestDescription$$anonfun$makeDescription$1.apply(JUnitSuiteRunner.scala:88)
	at scala.List.foreach(List.scala:834)
	at org.specs.runner.TestDescription$class.makeDescription(JUnitSuiteRunner.scala:88)
	at org.specs.runner.JUnitSuiteRunner.makeDescription(JUnitSuiteRunner.scala:13)
	at org.specs.runner.TestDescription$$anonfun$makeDescription$1.apply(JUnitSuiteRunner.scala:89)
	at org.specs.runner.TestDescription$$anonfun$makeDescription$1.apply(JUnitSuiteRunner.scala:88)
	at scala.List.foreach(List.scala:834)
	at org.specs.runner.TestDescription$class.makeDescription(JUnitSuiteRunner.scala:88)
	at org.specs.runner.JUnitSuiteRunner.makeDescription(JUnitSuiteRunner.scala:13)
	at org.specs.runner.JUnitSuiteRunner.getDescription(JUnitSuiteRunner.scala:37)
	at org.junit.runner.Runner.testCount(Runner.java:38)
	at org.eclipse.jdt.internal.junit4.runner.JUnit4TestClassReference.countTestCases(JUnit4TestClassReference.java:29)
	at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.countTests(RemoteTestRunner.java:480)
	at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.runTests(RemoteTestRunner.java:448)
	at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.runTests(RemoteTestRunner.java:673)
	at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.run(RemoteTestRunner.java:386)
	at org.eclipse.jdt.internal.junit.runner.RemoteTestRunner.main(RemoteTestRunner.java:196)

               
               */
  }

  object Implicits{
    trait Zippable[ST<:List,L<:List,Cur,R<:List]{
      def l():Zipper[ST,L,Cur,R]
    }
    trait DeZippable[ST<:List,L<:List,Cur,R<:List]{
      def e():Zipper[ST,L,Cur,R]
    }
    trait EndZipped[ST<:List,LT<:List]{
      def e():F[ST,LT]
    }
    trait Loadable[ST<:List,L<:List,Cur,R<:List]{
      def load():Zipper[ST,L,Cur,R]
    }
    trait Storeable[ST<:List,L<:List,Cur,R<:List]{
      def store():Zipper[ST,L,Cur,R]
    }

    implicit def moreZipping[ST<:List,LR<:List,LT,Cur,R<:List](z:Zipper[ST,LR**LT,Cur,R]) = new Zippable[ST,LR,LT,R**Cur]{
      def l():Zipper[ST,LR,LT,R**Cur] = Zipper(z.f,z.depth + 1)
    }
    implicit def notEmptyZipper[ST<:List,L<:List,Cur,RR<:List,RT](z:Zipper[ST,L,Cur,RR**RT]) = new DeZippable[ST,L**Cur,RT,RR]{
      def e():Zipper[ST,L**Cur,RT,RR] = Zipper(z.f,z.depth - 1)
    }
    implicit def emptyZipper[ST<:List,L<:List,Cur](z:Zipper[ST,L,Cur,Nil]) = new EndZipped[ST,L**Cur]{
      def e():F[ST,L**Cur] = z.f.asInstanceOf[F[ST,L**Cur]]
    }
    implicit def zippable[ST<:List,R<:List,T](f:F[ST,R**T]) = new Zippable[ST,R,T,Nil]{
      def l():Zipper[ST,R,T,Nil] = Zipper(f,0)
    }
    implicit def zipLoad[ST<:List,L<:List,Cur,R<:List](z:Zipper[ST,L,Cur,R]) = new Loadable[ST**Cur,L,Cur,R]{
      def load():Zipper[ST**Cur,L,Cur,R] = Zipper(z.f.loadI(z.depth),z.depth)
    }
    implicit def zipStore[ST,SR<:List,L<:List,R<:List](z:Zipper[SR**ST,L,_,R]) = new Storeable[SR,L,ST,R]{
      def store():Zipper[SR,L,ST,R] = Zipper(z.f.storeI(z.f.stack.rest,z.f.stack.top,z.depth),z.depth)
    }
    implicit def genNewLocal[ST<:List](f:F[ST,Nil]) = new Zippable[ST,Nil,Nil,Nil]{
      def l():Zipper[ST,Nil,Nil,Nil] = Zipper(f,0)
    }
    implicit def genNewLocalInZipper[ST<:List,Cur,R<:List](z:Zipper[ST,Nil,Cur,R]) = new Zippable[ST,Nil,Nil,R**Cur]{
      def l():Zipper[ST,Nil,Nil,R**Cur] = Zipper(z.f,z.depth + 1)
    }
  }

  type S[s] = F[Nil**s,Nil]

  trait ByteletCompiler{
	  // compile a piece of code which
	  def compile[T<:AnyRef,U<:AnyRef](cl:Class[T])(
                       code: F[Nil**T,Nil] // gets a parameter of type T on the stack
	                      => F[Nil**U,_]   // and uses it and has then a value of type U on the stack
	  ): T => U
  }

  
}

abstract class AbstractFunction1[T,U] extends Function1[T,U]
