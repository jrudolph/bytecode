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
    def pop_int[R<:List](rest:R):F[R,LT]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT]
  }
  trait Int2Stack[ST<:List,LT<:List]{
    def i1:Int
    def i2:Int
    def rest:ST
    def frame:F[_,LT]
    def iadd():F[ST**Int,LT] = frame.iadd_int[ST](rest,i1,i2)
    def imult():F[ST**Int,LT] = null
  }
  trait OneStack[R<:List,T,LT<:List]{
    def pop():F[R,LT]
    def dup():F[R**T**T,LT]
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
    }
  }

  trait ByteletCompiler{
	  // compile a piece of code which
	  def compile[T,U](code: F[Nil**T,Nil] // gets a parameter of type T on the stack
	                      => F[Nil**U,_]   // and uses it and has then a value of type U on the stack
	  ): T => U
  }

  object Interpreter extends ByteletCompiler{
    case class IF[ST<:List,LT<:List](stack:ST,locals:LT) extends F[ST,LT]{

      def bipush(i1:Int):F[ST**Int,LT] = IF(stack ** i1,locals)

      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1+i2),locals)
      def pop_int[R<:List](rest:R):F[R,LT] = IF(rest,locals)
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT] = IF(rest**top**top,locals)
    }

    def compile[T,U](code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U =
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
      def pop_int[R<:List](rest:R):F[R,LT] = {
        mv.visitInsn(POP)
        self
      }
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT] = {
        mv.visitInsn(DUP)
        self
      }
    }
    def classFromBytes(className:String,bytes:Array[Byte]):Class[_] = {
      new java.lang.ClassLoader{
        override def findClass(name:String):java.lang.Class[_] =
          defineClass(className,bytes,0,bytes.length);
      }.loadClass(className)
    }
    var i = 0
    def compile[T,U](code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U = {
      val className = "Compiled"+i

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
        // this is a hack only valid for integers as input values
        mv.visitVarInsn(ALOAD, 1);
        mv.visitTypeInsn(CHECKCAST, "java/lang/Integer");
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Integer", "intValue", "()I");

        code(new ASMFrame[Nil**T,Nil](mv))

        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;");
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
  def main(args:Array[String]):Unit = {
    import Bytecode._
    import Bytecode.Implicits._
    val bcs =
      (f:F[Nil**Int,Nil]) => f.bipush(1).iadd.bipush(3).dup.pop.dup.iadd.iadd
    val isucc: Int => Int = Bytecode.Interpreter.compile(bcs)
    val csucc: Int => Int = Bytecode.ASMCompiler.compile(bcs)

    def testRun(i:Int) {
      System.out.println(String.format("Test for %d interpreted %d compiled %d same %s"
                           ,int2Integer(i),int2Integer(isucc(i)),int2Integer(csucc(i)),(isucc(i)==csucc(i)).toString))
    }

    testRun(1)
    testRun(2)
  }

}
