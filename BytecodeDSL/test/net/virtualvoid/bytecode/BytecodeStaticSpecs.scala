package net.virtualvoid.bytecode

import org.specs._

import scala.tools.nsc.{Interpreter,Settings}

object BytecodeStaticSpecs extends Specification {
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
    
    "aload with String[]" in {Stack("Nil**Array[String]**Int") must haveOp("aload")}
    "aload with int[]" in {Stack("Nil**Array[Int]**Int") must haveOp("aload")}
    "astore with String[]" in {Stack("Nil**Array[String]**Int**String") must haveOp("astore")}
    "astore with int[]" in {Stack("Nil**Array[Int]**Int**Int") must haveOp("astore")}
    "arraylength with String[]" in {Stack("Nil**Array[String]") must haveOp("arraylength")}
    "dup_x1" in {Stack("Nil**String**Int") must haveOp("dup_x1")}
    
    "ifeq with int stack" in {Stack("Nil**Int") must haveOp("ifeq(null)")}
  }
  
  "implicits" should notApply {
    "dup on empty Stack" in {Stack("Nil") mustNot haveOp("dup")}
    "pop on empty Stack" in {Stack("Nil") mustNot haveOp("pop")}
    "dup_x1 on one Stack" in {Stack("Nil**String") mustNot haveOp("dup_x1")}
    
    "iadd on String**Int" in {Stack("Nil**String**Int") mustNot haveOp("iadd")}
  }
}

import org.specs.runner.JUnit4

class MyStaticSpecTest extends JUnit4(BytecodeStaticSpecs)