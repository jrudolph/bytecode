package net.virtualvoid.bytecode.v2

object Bytecode{
  trait List
  trait Nil extends List
  object N extends Nil
  case class Cons[+R<:List,+T](rest:R,top:T) extends List

  trait Consable[T<:List]{
    def **[U](next:U): T**U
  }
  implicit def conser[T<:List](t:T) = new Consable[T]{
    def **[U](next:U): T**U = Cons(t,next)
  }

  // define an infix operator shortcut for the cons type
  type ** [x<:List,y] = Cons[x,y]

  trait F[ST<:List,LT<:List]{
    def stack:ST
    def locals:LT

    def bipush(i1:Int):F[ST**Int,LT]

    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def pop_int[R<:List](rest:R):F[R,LT]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT]
    def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT]
  }
  trait Int2Stack[ST<:List,LT<:List]{
    def i1:Int
    def i2:Int
    def rest:ST
    def frame:F[_,LT]
    def iadd():F[ST**Int,LT] = frame.iadd_int[ST](rest,i1,i2)
    def imul():F[ST**Int,LT] = frame.imul_int[ST](rest,i1,i2)
  }
  trait OneStack[R<:List,T,LT<:List]{
    def pop():F[R,LT]
    def dup():F[R**T**T,LT]
    def method[U](code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def checkcast[U](cl:Class[U]):F[R**U,LT]
  }
  trait LocalZipper[ST<:List,L,Cur,R[_]<:List]{
    def load():F[ST**Cur,R[L**Cur]]
  }
  trait Zippable[ST<:List,L<:List,Cur,R[_]<:List]{
    def l():LocalZipper[ST,L,Cur,R]
  }
  trait LocalStoreZipper[T,Rest<:List,L,R[_<:List]]{
    //def load():F[Rest**T**Cur,R[L**Cur]]
    //def store():F[Rest,R[L**T]]
    //def l():LocalZipper
  }


  object Implicits{
    implicit def int2Stack[R<:List,LT<:List](f:F[R**Int**Int,LT]):Int2Stack[R,LT] = new Int2Stack[R,LT]{
      val frame = f
      val stack = f.stack
      val rest = stack.rest.rest
      val i1 = stack.rest.top
      val i2 = stack.top
    }
    implicit def oneStack[R<:List,LT<:List,T](f:F[R**T,LT]):OneStack[R,T,LT] = new OneStack[R,T,LT]{
      def pop = f.pop_int(f.stack.rest)

      def dup = f.dup_int(f.stack.rest,f.stack.top)
      def method[U](code:scala.reflect.Code[T=>U]):F[R**U,LT] =
        f.method_int(f.stack.rest,f.stack.top,code)
      def checkcast[U](cl:Class[U]):F[R**U,LT] = f.checkcast_int(f.stack.rest,f.stack.top)(cl)
    }
    /*
     * Severity and Description	Path	Resource	Location	Creation Time	Id
the kinds of the type arguments
(ST,R,T,net.virtualvoid.bytecode.v2.Bytecode.Implicits.Id)
do not conform to the expected kinds of the type parameters
(type ST,type L,type Cur,type R) in trait Zippable.
net.virtualvoid.bytecode.v2.Bytecode.Implicits.Id's type parameters do not match type R's
expected parameters:
type s's bounds >: Nothing <: net.virtualvoid.bytecode.v2.Bytecode.List
are stricter than type _'s declared bounds >: Nothing <: Any	BytecodeDSL/src/net/virtualvoid/bytecode/v2	Bytecode.scala	Unknown	1220615849755	40778

*/
    //type Id[s] = s
    trait Id[X] extends List
    trait Ap[X,Y]
    type Test[x] = Cons[_,x]
    implicit def toZipper[ST<:List,R<:List,T](f:F[ST,R**T]):Zippable[ST,R,T,Id] = null
    //implicit def localZipper[ST,Rest,L,Cur,R[_]<:List](x:LocalZipper[ST,Rest**L,Cur,R]):Zippable[ST,Rest,L,Test[R[Cur]]] = null
  }

  type S[s] = F[Nil**s,Nil]

  trait ByteletCompiler{
	  // compile a piece of code which
	  def compile[T<:AnyRef,U<:AnyRef](cl:Class[T],
                       code: F[Nil**T,Nil] // gets a parameter of type T on the stack
	                      => F[Nil**U,_]   // and uses it and has then a value of type U on the stack
	  ): T => U
  }

  object Interpreter extends ByteletCompiler{
    case class IF[ST<:List,LT<:List](stack:ST,locals:LT) extends F[ST,LT]{

      def bipush(i1:Int):F[ST**Int,LT] = IF(stack ** i1,locals)

      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1+i2),locals)
      def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1*i2),locals)
      def pop_int[R<:List](rest:R):F[R,LT] = IF(rest,locals)
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT] = IF(rest**top**top,locals)
      def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT] = null
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT] = IF(rest**top.asInstanceOf[U],locals)
    }

    def compile[T,U](cl:Class[T],code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U =
      t => code(IF(N**t,N)).stack.top
  }
  object ASMCompiler extends ByteletCompiler{
    import org.objectweb.asm._
    import org.objectweb.asm.Opcodes._

    class ASMFrame[ST<:List,LT<:List](mv:MethodVisitor) extends F[ST,LT]{
      def self[T]:T = this.asInstanceOf[T]

      val loopingList = new Cons(null.asInstanceOf[List],null){
        override val rest = this
        override val top = null
      }

      def locals:LT=loopingList.asInstanceOf[LT]
      def stack:ST=loopingList.asInstanceOf[ST]

      def bipush(i1:Int):F[ST**Int,LT] = {
        mv.visitIntInsn(BIPUSH, i1)
        self
      }
      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = {
        mv.visitInsn(IADD)
        self
      }
      def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = {
        mv.visitInsn(IMUL)
        self
      }
      def pop_int[R<:List](rest:R):F[R,LT] = {
        mv.visitInsn(POP)
        self
      }
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT] = {
        mv.visitInsn(DUP)
        self
      }
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT] = {
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));
        self
      }
      def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT] = {
        import scala.reflect._
        def getClass(name:String):java.lang.Class[_] = name match{
          case "scala.Int" => Integer.TYPE
          case _ => java.lang.Class.forName(name)
        }
        code.tree match {
        // match simple function applications like i=>i.intValue or (_:java.lang.Integer).intValue
        case Function(List(x@LocalValue(_,_,PrefixedType(_,Class(clazz)))),Apply(Select(Ident(x1),Method(method,_)),List())) if x==x1 => {
          System.out.println("Classname: "+clazz)
          System.out.println("Methodname: "+method)
          val cl = java.lang.Class.forName(clazz).asInstanceOf[scala.Predef.Class[T]]
          val methodName = method.substring(clazz.length+1)
          val m = cl.getMethod(methodName)
          mv.visitMethodInsn(INVOKEVIRTUAL,Type.getInternalName(cl),methodName,Type.getMethodDescriptor(m))
        }
        case Function(List(x),Apply(Select(_,Method(method,MethodType(List(PrefixedType(_,Class(argClazz))),PrefixedType(_,Class(clazz))))),List(Ident(x1)))) if x==x1 => {
          System.out.println("Classname: "+clazz)
          val cl = java.lang.Class.forName(clazz)
          val methodName = method.substring(clazz.length+1)
          val argCl = getClass(argClazz)
          System.out.println("static Methodname: "+methodName)
          val m = cl.getMethod(methodName,argCl)
          mv.visitMethodInsn(INVOKESTATIC, Type.getInternalName(cl), methodName, Type.getMethodDescriptor(m));
        }
        case _ => throw new Error("Can't match this "+code.tree)
        }
        self
      }
    }
    def classFromBytes(className:String,bytes:Array[Byte]):Class[_] = {
      new java.lang.ClassLoader{
        override def findClass(name:String):java.lang.Class[_] = {
          val fos = new java.io.FileOutputStream(name+".class")
          fos.write(bytes)
          fos.close
          defineClass(className,bytes,0,bytes.length);
        }
      }.loadClass(className)
    }
    var i = 0
    def compile[T<:AnyRef,U<:AnyRef](cl:Class[T],code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U = {
      i+=1
      val className = "Compiled" + i

      val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
      cw.visit(V1_5,ACC_PUBLIC + ACC_SUPER,className,null,"net/virtualvoid/bytecode/v2/AbstractFunction1", null)

      { // constructor
        val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "net/virtualvoid/bytecode/v2/AbstractFunction1", "<init>", "()V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
      }

      { // apply
        val mv = cw.visitMethod(ACC_PUBLIC, "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", null, null);
        mv.visitCode()
        // put the parameter on the stack
        mv.visitVarInsn(ALOAD, 1);
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));

        code(new ASMFrame[Nil**T,Nil](mv))

        mv.visitInsn(ARETURN);
        mv.visitMaxs(1, 2)
        mv.visitEnd
      }
      cw.visitEnd
      classFromBytes(className,cw.toByteArray).newInstance.asInstanceOf[T=>U]
    }
  }
}

abstract class AbstractFunction1[T,U] extends Function1[T,U]

object Test{
  import Bytecode._
  import Bytecode.Implicits._

  def localsTest = {
    val f:F[Nil,Nil**Int**Float**String] = null
    //f.l.l.load
    null
  }
  def main(args:Array[String]):Unit = {

    import java.lang.{Integer=>jInt}
    val bcs =
      (f:S[jInt]) =>
      f.method(_.intValue)
       .bipush(1)
       .iadd
       .bipush(3)
       .dup
       .pop
       .dup
       .iadd
       .iadd
       .method(jInt.valueOf(_))

    //val isucc: jInt => jInt = Bytecode.Interpreter.compile(bcs)
    val csucc: jInt => jInt = Bytecode.ASMCompiler.compile(classOf[jInt],bcs)
    val isucc = csucc

    def testRun(i:Int) {
      System.out.println(String.format("Test for %d interpreted %d compiled %d same %s"
                           ,int2Integer(i.intValue),int2Integer(isucc(i).intValue),int2Integer(csucc(i).intValue),(isucc(i)==csucc(i)).toString))
    }

    testRun(1)
    testRun(2)

    val f = ASMCompiler.compile(classOf[java.lang.String],
      (f:S[java.lang.String]) => f.method(_.length).bipush(3).imul.method(Integer.valueOf(_)))
    System.out.println(f("123"))
    System.out.println(f("123456"))
  }
}