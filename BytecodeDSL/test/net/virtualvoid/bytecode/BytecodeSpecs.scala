package net.virtualvoid.bytecode

import org.specs._

import scala.tools.nsc.{Interpreter,Settings}

object BytecodeSpecs extends Specification {
  import scala.tools.nsc.reporters._

  val mySettings = new Settings
  object interpreter extends Interpreter(mySettings){
    var writer = new java.io.StringWriter
    var pWriter = newWriter
    def newWriter = new java.io.PrintWriter(writer)
    
    def lastError = writer.toString
    
    object myReporter extends ConsoleReporter(mySettings,null,null){
      override def printMessage(msg: String) { pWriter.print(msg + "\n"); pWriter.flush() }
      override def reset{
        writer = new java.io.StringWriter
        pWriter = newWriter
        super.reset
      }
    }
    override def newCompiler(se:Settings,reporter:Reporter) = {
      super.newCompiler(se,myReporter)
    }
  }
  
  interpreter.interpret("import net.virtualvoid.bytecode.v2.Bytecode._")
  interpreter.interpret("import net.virtualvoid.bytecode.v2.Bytecode.Implicits._")
  
  import org.specs.matcher.Matcher
  def compilePrefixed(prefix:String,suffix:String) = new Matcher[String]{
    def apply(str: =>String) = 
      {
        interpreter.myReporter.reset
        interpreter.compileString(
          """object Test {
import net.virtualvoid.bytecode.v2.Bytecode._
import net.virtualvoid.bytecode.v2.Bytecode.Implicits._
"""+prefix+str+suffix+"}")
        (!interpreter.myReporter.hasErrors,"compiled","did not compile with error: "+interpreter.lastError)
      }      
  }
  def compile = compilePrefixed("","")
  def compileWithStack(stack:String) = compilePrefixed("(null:F["+stack+",Nil]).","")
  
  case class Frame(stack:String,locals:String){
    def state:String = stack + "," + locals
  }
  case class Stack(s:String) extends Frame(s,"Nil")
  case class Locals(l:String) extends Frame("Nil",l)
  
  def haveOp(op:String) = new Matcher[Frame]{
    val inner = compilePrefixed("(null:F[","])."+op)
    def apply(f: =>Frame) = inner(f.state) 
  }  

  import org.specs.specification.Example
  def suffix(suffix:String)(e: =>Example) = {currentSut.verb += suffix;e}
  val apply = suffix(" apply")(_)
  val notApply = suffix(" not apply")(_)
  
  "implicits" should apply {
    "dup on Int Stack" in {Stack("Nil**Int") must haveOp("dup")}
    
    "iadd on Int**Int" in {Stack("Nil**Int**Int") must haveOp("iadd")}
    "iadd on _**Int**Int" in {Stack("(_<:List)**Int**Int") must haveOp("iadd")}
   
    "l.load.e.dup.iadd with Int local" in {Locals("Nil**Int") must haveOp("l.load.e.dup.iadd")}
    "l.store.e on no locals (should generate one local)" in {Frame("Nil**String","Nil") must haveOp("l.store.e.l.load.e.method(_.length)")}
  }
  
  "implicits" should notApply {
    "dup on empty Stack" in {Stack("Nil") mustNot haveOp("dup")}
    "pop on empty Stack" in {Stack("Nil") mustNot haveOp("pop")}
    
    "iadd on String**Int" in {Stack("Nil**String**Int") mustNot haveOp("iadd")}
  }
  
  import net.virtualvoid.bytecode.v2.Bytecode._
  def calcValue[U<:AnyRef](tuple:(String,U)) = new Matcher[S[String]=>F[Nil**U,_]]{
    import net.virtualvoid.bytecode.v2.Bytecode.Implicits._
    def apply(f: => S[String]=>F[Nil**U,_]) = {
      val compiled = ASMCompiler.compile(classOf[String])(f)
      val res = compiled(tuple._1)
      (res == tuple._2,"executed correctly","wrong result "+res+" instead of " +tuple._2)
    }
  }  
  
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
      .apply(12.453) must be_==(12.453)
    }
  }
  
  "Compiler" should {
    "succeed in generic Tests" in compiledTests(net.virtualvoid.bytecode.v2.Bytecode.ASMCompiler)
  }
  "Interpreter" should {
    "succeed in generic Tests" in compiledTests(net.virtualvoid.bytecode.v2.Bytecode.Interpreter)
  }
}

import org.specs.runner.JUnit4

class MySpecTest extends JUnit4(BytecodeSpecs)