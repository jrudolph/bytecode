package net.virtualvoid.bytecode

import _root_.org.specs._

import _root_.scala.tools.nsc.Settings

object BytecodeStaticSpecs extends Specification {
  import _root_.scala.tools.nsc.reporters._

  val outerClassLoader = getClass.getClassLoader.asInstanceOf[java.net.URLClassLoader]
  val mySettings = new Settings
  
  def asFile(url:java.net.URL):java.io.File =
      new java.io.File(url.toURI)

  def printClassLoaders(cl: ClassLoader) {
    if (cl != null)
      { 
	val urls = if (cl.isInstanceOf[java.net.URLClassLoader]) cl.asInstanceOf[java.net.URLClassLoader].getURLs.mkString(", ") else ""
        println(cl.toString+": "+urls); 
        
        printClassLoaders(cl.getParent);}
  }
  def inflateClassPath(jarfile: java.io.File): Seq[java.io.File] =
    jarfile +: new java.util.jar.JarFile(jarfile).getManifest.getMainAttributes.getValue("Class-Path").split("\\s+").map(new java.io.File(_))

  object interpreter extends _root_.scala.tools.nsc.Interpreter(mySettings){
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
      se.classpath.value = outerClassLoader.getURLs flatMap(url => inflateClassPath(asFile(url))) map (_.getAbsolutePath) mkString(java.io.File.pathSeparator)
      printClassLoaders(outerClassLoader)
      println(se.classpath.value)
      super.newCompiler(se,myReporter)
    }
  }
  
  import org.specs.matcher.Matcher
  def compilePrefixed(prefix:String,suffix:String) = new Matcher[String]{
    def apply(str: =>String) = 
      {
        try {
         interpreter.myReporter.reset
        } catch {case t: Throwable => t.printStackTrace; throw t}
        interpreter.compileString(
          """object Test {
import _root_.net.virtualvoid.bytecode.Bytecode
import Bytecode._
import Bytecode.Instructions._
import Bytecode.Implicits._
"""+prefix+str+suffix+"}")
        (!interpreter.myReporter.hasErrors,"compiled","did not compile with error: "+interpreter.lastError)
      }      
  }
  def compile = compilePrefixed("","")
  def compileWithStack(stack:String) = compilePrefixed("(null:F["+stack+",Nil]).","")
  
  case class Stack(types:String)
  
  def haveOp(op:String) = new Matcher[Stack]{
    val inner = compilePrefixed("(null:F[","]) ~ "+op)
    def apply(f: =>Stack) = inner(f.types) 
  }  

  /* FIX: Does not work anymore in specs 1.4.x and didn't find out how
   * to fix yet
  import org.specs.specification.Example
  def suffix(suffix:String)(e: =>Example) = {currentSut.verb += suffix;e}
  val apply = suffix(" apply")(_)
  val notApply = suffix(" not apply")(_)
   */
  
  "implicits" should {
    "dup on Int Stack" in {Stack("Nil**Int") must haveOp("dup")}
    
    "iadd on Int**Int" in {Stack("Nil**Int**Int") must haveOp("iadd")}
    //"iadd on _**Int**Int" in {Stack("(_<:List)**Int**Int") must haveOp("iadd")}
   
    //"l.load.e.dup.iadd with Int local" in {Locals("Nil**Int") must haveOp("local[_0,Int].load()~dup~iadd")}
    //"l.l.load.e.e.dup.iadd with Int local on place 2" in {Locals("Nil**Int**String") must haveOp("local[_1,Int].load()~dup~iadd")}
    //"l.store.e on no locals (should generate one local)" in {Frame("Nil**String") must haveOp("local[_0,String].store()~local[_0,String].load()~invokemethod1(_.length)")}
    
    "aload with String[]" in {Stack("Nil**Array[String]**Int") must haveOp("aload")}
    "aload with int[]" in {Stack("Nil**Array[Int]**Int") must haveOp("aload")}
    "astore with String[]" in {Stack("Nil**Array[String]**Int**String") must haveOp("astore")}
    "astore with int[]" in {Stack("Nil**Array[Int]**Int**Int") must haveOp("astore")}
    "arraylength with String[]" in {Stack("Nil**Array[String]") must haveOp("arraylength")}
    "dup_x1" in {Stack("Nil**String**Int") must haveOp("dup_x1")}
    
    "getstatic" in {Stack("Nil") must haveOp("getstatic(() => net.virtualvoid.bytecode.StaticVariableContainer.x)")} 
    "putstatic" in {Stack("Nil**Int") must haveOp("putstatic(net.virtualvoid.bytecode.StaticVariableContainer.x = _)")}
    
    //"ifeq with int stack" in {Stack("Nil**Int") must haveOp("ifeq(null)")}
    
    "swap on String**Int" in {Stack("Nil**String**Int") must haveOp("swap()")}
    "swap on Byte**Character" in {Stack("Nil**Byte**Character") must haveOp("swap()")}
    "swap on Boolean**Character" in {Stack("Nil**Boolean**Character") must haveOp("swap()")}
    "swap on Int**Short" in {Stack("Nil**Int**Short") must haveOp("swap()")}
    "swap on Float**Object" in {Stack("Nil**Float**Object") must haveOp("swap()")}

    "pop on String" in {Stack("Nil**String") must haveOp("pop")}
    "dup on String" in {Stack("Nil**String") must haveOp("dup")}
  }
  
  "implicits" should {
    "dup on empty Stack" in {Stack("Nil") mustNot haveOp("dup")}
    "pop on empty Stack" in {Stack("Nil") mustNot haveOp("pop")}
    "dup_x1 on one Stack" in {Stack("Nil**String") mustNot haveOp("dup_x1")}
    
    "iadd on String**Int" in {Stack("Nil**String**Int") mustNot haveOp("iadd")}
    
    "swap on String**Double" in {Stack("Nil**String**Double") mustNot haveOp("swap()")}
    "swap on Long**String" in {Stack("Nil**Long**String") mustNot haveOp("swap()")}
    "swap on Long**Double" in {Stack("Nil**Long**Double") mustNot haveOp("swap()")}
    "dup on String**Double" in {Stack("Nil**String**Double") mustNot haveOp("dup")}
    "pop on String**Double" in {Stack("Nil**String**Double") mustNot haveOp("pop")}
    
    "putstatic must respect element on the stack vs variable type" in {Stack("Nil**String") mustNot haveOp("putstatic(net.virtualvoid.bytecode.StaticVariableContainer.x = _)")}
  }
}

//import org.specs.runner.JUnit4

//class MyStaticSpecTest extends JUnit4(BytecodeStaticSpecs)
