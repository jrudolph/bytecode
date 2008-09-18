package net.virtualvoid.string

import java.lang.{StringBuilder,String=>jString}

trait StringBuildable{
  def sb():java.lang.StringBuilder
}

object Compiler{
  import net.virtualvoid.bytecode.v2.Bytecode
  import Bytecode._
  import Bytecode.Implicits._

  val parser = new StrParser{}
  import parser.lexical._

  def elementType(it:java.lang.reflect.Type):Class[_ <: AnyRef] = {
    TypeHelper.genericInstanceType(it,classOf[java.lang.Iterable[_]],Array()) match{
      case Some(cl:java.lang.Class[AnyRef]) => cl
      case _ => throw new java.lang.Error("Can't get element type of "+it)
    }
  }

  def compileTok[R<:List,LR<:List,T<:java.lang.Object](tok:StrToken,cl:Class[T])(f:F[R**StringBuilder,LR**T]):F[R**StringBuilder,LR**T]
    = tok match {
      case Literal(str) => f.ldc(str).method2(_.append(_))
      case p@ParentExp(inner,parent) =>{
        val m = p.method(cl)
        f.l.load.e
         .swap
         .l.load.e
         .dynMethod(m,classOf[AnyRef])
         .l.store.e
         .op(compileTok(inner,m.getReturnType.asInstanceOf[Class[AnyRef]]))
         .swap
         .l.store.e
      }
      case Exp("this") =>{
        f.l.load.e
         .checkcast(classOf[java.lang.Object]) // TODO: don't know why we need this, examine it
         .method((_:java.lang.Object).toString)
         .method2((sb:StringBuilder,s:jString)=>sb.append(s))}
      case e:Exp => {
        f.l.load.e
         .dynMethod(e.method(cl),classOf[AnyRef])
         .method(_.toString)
         .method2(_.append(_))
      }
      case SpliceExp(exp,sep,inner) => {
        val m = exp.method(cl)
        val eleType:Class[AnyRef] = elementType(m.getGenericReturnType).asInstanceOf[Class[AnyRef]]

        if (classOf[java.lang.Iterable[_]].isAssignableFrom(m.getReturnType)){
          val jmpTarget =
            f.l.load.e
             .swap // save one instance of T for later
             .l.load.e
             .dynMethod(exp.method(cl),classOf[java.lang.Iterable[AnyRef]])
             .method(_.iterator)
             .l.store.e
             .target
          jmpTarget
             .l.load.e //sb,it
             .method(_.hasNext)
             .ifeq(f=>
               f.l.load.e
                .swap
                .l.load.e
                .method(_.next)
                .checkcast(eleType)
                .l.store.e
                .op(compileToks(inner,eleType))
                .swap
                .dup
                .l.store.e
                .method(_.hasNext)
                .ifeq(f =>
                   f.ldc(sep:jString)
                    .method2(_.append(_))
                    .jmp(jmpTarget)) //todo: introduce ifeq(thenCode,elseTarget)
                .jmp(jmpTarget))
             .swap
             .l.store.e
        }
        else
          throw new java.lang.Error("can only iterate over iterables right now")
      }
    }
  def compileToks[R<:List,LR<:List,T<:java.lang.Object](tok:Seq[StrToken],cl:Class[T])(f:F[R**StringBuilder,LR**T]) = {
    var mf:F[R**StringBuilder,LR**T] = f
    for (t<-tok)
      mf = compileTok(t,cl)(f)
    mf
  }
  def compile[T<:AnyRef](format:String,cl:Class[T]):T=>jString = {
    val toks = parser.parse(format)
    ASMCompiler.compile(cl)(
     f =>
       f.dup.l.store.e
         .checkcast(classOf[StringBuildable])
         .method(_.sb)
         .op(compileToks(toks,cl))
         .method(_.toString)
     )
  }
  case class Bank(n:String){
    def name():jString = n
  }
  case class Account(n:String,b:Bank) {
    def number():jString = n
    def bank() = b
  }
  class Person extends StringBuildable{
      def name():java.lang.String = "Joe"
      def sb():java.lang.StringBuilder = new java.lang.StringBuilder
      def accountNames():java.util.List[java.lang.String] = java.util.Arrays.asList("a","b")
      val sparkasse = Bank("Sparkasse")
      def accounts():java.lang.Iterable[Account] = java.util.Arrays.asList(Account("78910",sparkasse),Account("12345",Bank("Volksbank")))
  }
  def main(args:Array[String]){
    def output(format:String) = System.out.println(compile(format,classOf[Person])(new Person))
    output("Name: #name Accounts: ")
    output("Name: #name Accounts: #accountNames{,}*")
    output("Name: #name Accounts: #accounts[#number]{, }*")
    output("Name: #name Accounts: #accounts[#number(#bank.name)]{, }*")
  }
}