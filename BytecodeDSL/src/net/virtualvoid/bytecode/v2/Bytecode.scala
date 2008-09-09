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
    def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT]
    def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT]

    def loadI[T](i:Int):F[ST**T,LT]
    def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT]
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
  trait TwoStack[R<:List,T2,T1,LT<:List]{
    def method2[U](code:scala.reflect.Code[(T2,T1) => U]):F[R**U,LT]
  }
  case class Zipper[ST<:List,L<:List,Cur,R<:List](f:F[ST,_],depth:Int)
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
    implicit def twoStack[R<:List,LT<:List,T1,T2](f:F[R**T2**T1,LT]):TwoStack[R,T2,T1,LT] = new TwoStack[R,T2,T1,LT]{
      def method2[U](code:scala.reflect.Code[(T2,T1) => U]):F[R**U,LT] =
        f.method_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top,code)
    }

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
      def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT] = null
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT] = IF(rest**top.asInstanceOf[U],locals)

      def get[T](i:Int,l:List):T = l match{
        case N => throw new Error("not possible")
        case Cons(r,t:T) => if (i == 0) t else get(i-1,r)
      }
      def store[T](i:Int,l:List,t:T):List = l match {
        case N => if (i == 0) Cons(N,t) else Cons(store(i-1,N,t),N)
        case Cons(r,old:T) => if (i == 0) Cons(r,t) else Cons(store(i-1,r,t),old)
      }

      def loadI[T](i:Int):F[ST**T,LT] = IF(stack**get(i,locals),locals)
      def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT] =
        IF(rest,store(i,locals,top).asInstanceOf[NewLT])
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
      def loadI[T](i:Int):F[ST**T,LT] = {
        mv.visitVarInsn(ALOAD, i); // TODO: load the correct type
        self
      }
      def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT] = {
        mv.visitVarInsn(ASTORE, i); // TODO: store the correct type
        self
      }
      def getClass(name:String):java.lang.Class[_] = name match{
        case "scala.Int" => Integer.TYPE
        case _ => java.lang.Class.forName(name)
      }
      def getInvokeMethod(cl:Class[_]) = if (cl.isInterface) INVOKEINTERFACE else INVOKEVIRTUAL

      /*
       Function(
       * List(LocalValue(NoSymbol,x$2,PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String))), LocalValue(NoSymbol,x$3,PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String))))
       * ,Apply(Select(Ident(LocalValue(NoSymbol,x$2,PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String)))),Method(java.lang.String.concat,MethodType(List(PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String))),PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String)))))
       * ,List(Ident(LocalValue(NoSymbol,x$3,PrefixedType(ThisType(Class(java.lang)),Class(java.lang.String)))))))
       */
      def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT] = {
        import scala.reflect._
        code.tree match {
          case Function(List(p1,p2@LocalValue(_,_,PrefixedType(_,Class(paramClass)))),Apply(Select(Ident(th),Method(method,_)),List(Ident(x)))) if th == p1 && x == p2 =>{
            val i = method.lastIndexOf(".")
            val clName = method.substring(0,i)
            val methodName = method.substring(i+1)
            System.out.println(clName);
            System.out.println(methodName);
            val cl = java.lang.Class.forName(clName)
            val cl2 = java.lang.Class.forName(paramClass)
            val m = cl.getMethod(methodName,cl2)
            mv.visitMethodInsn(getInvokeMethod(cl),Type.getInternalName(cl),methodName,Type.getMethodDescriptor(m))
          }
          case _ => throw new Error("Can't match this "+code.tree)
        }
        self
      }

      def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT] = {
        import scala.reflect._
        code.tree match {
        // that's very bad duplication from the next pattern: this matches same as next but for an applied type
        case Function(List(x@LocalValue(_,_,AppliedType(PrefixedType(_,Class(clazz)),_))),Apply(Select(Ident(x1),Method(method,_)),List())) if x==x1 => {
          System.out.println("Classname: "+clazz)
          System.out.println("Methodname: "+method)
          val cl = java.lang.Class.forName(clazz).asInstanceOf[scala.Predef.Class[T]]
          val methodName = method.substring(clazz.length+1)
          val m = cl.getMethod(methodName)
          mv.visitMethodInsn(getInvokeMethod(cl),Type.getInternalName(cl),methodName,Type.getMethodDescriptor(m))
        }
        // match simple function applications like i=>i.intValue or (_:java.lang.Integer).intValue
        case Function(List(x@LocalValue(_,_,PrefixedType(_,Class(clazz)))),Apply(Select(Ident(x1),Method(method,_)),List())) if x==x1 => {
          System.out.println("Classname: "+clazz)
          System.out.println("Methodname: "+method)
          val cl = java.lang.Class.forName(clazz).asInstanceOf[scala.Predef.Class[T]]
          val methodName = method.substring(clazz.length+1)
          val m = cl.getMethod(methodName)
          mv.visitMethodInsn(getInvokeMethod(cl),Type.getInternalName(cl),methodName,Type.getMethodDescriptor(m))
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
  def main(args:Array[String]):Unit = {
    import Bytecode._
    import Bytecode.Implicits._
    import java.lang.{Integer=>jInt}

    val func = Interpreter.compile(classOf[AnyRef],(f2:S[AnyRef]) => f2.pop.bipush(12).dup.iadd.l.store.e.l.load.e.dup.imul)
    System.out.println(func(null))

    val func2 = ASMCompiler.compile(classOf[java.lang.String],
                                   (f:S[java.lang.String]) =>
      f.dup
       .l.l.store.e.e
       .method(_.toUpperCase)
       .dup.method2(_.concat(_))
       )

    System.out.println(func2("wurst"));

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

    def compile{
    /*
     * "Name #name"
     *
     */
      val f: Person=>String = ASMCompiler.compile(classOf[Person],
        (f:S[Person]) =>
      f.dup
       .method(x => x.sb())
       .l.store.e
       .l.l.store.e.e
       .l.load.e
       .l.l.load.e.e
       .method(_.name)
       .method2(_.append(_))
       .method(_.toString)
    )
    System.out.println(f(new Person))

    }
    compile
    }
}
