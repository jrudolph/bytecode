package net.virtualvoid.bytecode

import Bytecode._
import java.lang.{String=>jString}

object ASMCompiler extends ByteletCompiler{
    import org.objectweb.asm._
    import org.objectweb.asm.Opcodes._

    case class ClassStack(mrest:ClassStack,mtop:Class[_]) extends Cons[ClassStack,Class[_]](mrest,mtop){
      def **(cl:Class[_]) = ClassStack(this,cl)
      
      def get(i:Int):Class[_] = if (i>0) rest.get(i-1) else top
      def set(i:Int,cl:Class[_]):ClassStack = if (i>0) ClassStack(rest.set(i-1,cl),top) else ClassStack(rest,cl)
    }
    class UnsetClassStack(lrest:ClassStack) extends ClassStack(lrest,null){
      override def get(i:Int):Class[_] = if (i==0) throw new Error("tried to get local which never was saved") else super.get(i)
      override def set(i:Int,cl:Class[_]):ClassStack = if (i>0) new UnsetClassStack(set(i-1,cl)) else ClassStack(this,cl)  
    }
    case object EmptyClassStack extends UnsetClassStack(null)
    
    class ASMFrame[ST<:List,LT<:List](mv:MethodVisitor,stackClass:ClassStack,localsClass:ClassStack) extends F[ST,LT]{
      def self[T]:T = this.asInstanceOf[T]

      val loopingList = new Cons(null.asInstanceOf[List],null){
        override val rest = this
        override val top = null
      }
      
      def stack = loopingList.asInstanceOf[ST]
      def locals = loopingList.asInstanceOf[LT]
      
      def newStacked[T](cl:Class[T]) = new ASMFrame[ST**T,LT](mv,stackClass**cl,localsClass)
      
      def bipush(i1:Int):F[ST**Int,LT] = {
        mv.visitIntInsn(BIPUSH, i1)
        newStacked(classOf[Int])
      }
      def ldc(str:jString):F[ST**jString,LT] = {
        mv.visitLdcInsn(str)
        newStacked(classOf[jString])
      }
      case class ASMTarget[ST<:List,LT<:List](mv:MethodVisitor,stackClass:ClassStack,localsClass:ClassStack,label:Label)
          extends ASMFrame[ST,LT](mv,stackClass,localsClass) with Target[ST,LT]
      def target:Target[ST,LT] = {
        val label = new Label
        mv.visitLabel(label)
        ASMTarget(mv,stackClass,localsClass,label)
      }

      def jmp(t:Target[ST,LT]):Nothing = {
        mv.visitJumpInsn(GOTO,t.asInstanceOf[ASMTarget[ST,LT]].label)
        null.asInstanceOf[Nothing]
      }

      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = {
        mv.visitInsn(IADD)
        new ASMFrame[R**Int,LT](mv,stackClass.rest,localsClass)
      }
      def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = {
        mv.visitInsn(ISUB)
        new ASMFrame[R**Int,LT](mv,stackClass.rest,localsClass)
      }
      def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = {
        mv.visitInsn(IMUL)
        new ASMFrame[R**Int,LT](mv,stackClass.rest,localsClass)
      }
      def pop_int[R<:List](rest:R):F[R,LT] = {
        mv.visitInsn(POP)
        new ASMFrame[R,LT](mv,stackClass.rest,localsClass)
      }
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT] = {
        mv.visitInsn(DUP)
        new ASMFrame[R**T**T,LT](mv,stackClass**stackClass.top,localsClass)
      }
      def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2,LT] = {
        mv.visitInsn(SWAP)
        new ASMFrame[R**T1**T2,LT](mv,stackClass.rest.rest**stackClass.top**stackClass.rest.top,localsClass)
      }
      def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1,LT] = {
        mv.visitInsn(DUP_X1)
        new ASMFrame[R**T1**T2**T1,LT](mv,stackClass.rest.rest**stackClass.top**stackClass.rest.top**stackClass.top,localsClass)
      }
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT] = {
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));
        new ASMFrame[R**U,LT](mv,stackClass.rest**cl,localsClass)
      }
      def ifeq_int[R<:List](rest:R,top:JVMInt,inner:F[R,LT] => Nothing):F[R,LT] = {
        val l = new Label
        mv.visitJumpInsn(IFEQ,l)

        inner(self)

        mv.visitLabel(l)
        new ASMFrame[R,LT](mv,stackClass.rest,localsClass)
      }
      import CodeTools._
      def aload_int[R<:List,T](rest:R,array:AnyRef,i:Int):F[R**T,LT] = {
        val elType = stackClass.rest.top.getComponentType
        
        mv.visitInsn(opcode(elType,IALOAD))
        
        new ASMFrame[R**T,LT](mv,stackClass.rest.rest ** elType,localsClass)
      }
      def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT] = {
        val elType = stackClass.rest.rest.top.getComponentType
        
        mv.visitInsn(opcode(elType,IASTORE))
        
        new ASMFrame[R,LT](mv,stackClass.rest.rest.rest,localsClass)
      }
      def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT] = {
        mv.visitInsn(ARRAYLENGTH)
        
        new ASMFrame[R**Int,LT](mv,stackClass.rest ** classOf[Int],localsClass)
      }      
      def opcode(cl:Class[_],opcode:Int) = 
        Type.getType(cleanClass(cl.getName)).getOpcode(opcode)

      def loadI[T](i:Int):F[ST**T,LT] = {
        val toLoad = localsClass.get(i)
        mv.visitVarInsn(opcode(toLoad,ILOAD), i);
        new ASMFrame[ST**T,LT](mv,stackClass**toLoad,localsClass)
      }
      def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT] = {
        mv.visitVarInsn(opcode(stackClass.top,ISTORE), i);
        new ASMFrame[R,NewLT](mv,stackClass.rest,localsClass.set(i,stackClass.top))
      }
      
      def newInstance[T](cl:Class[T]):F[ST**T,LT] = {
        val cons = cl.getConstructor()
        mv.visitTypeInsn(NEW,Type.getInternalName(cl))
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKESPECIAL,Type.getInternalName(cl),"<init>",Type.getConstructorDescriptor(cons))
        new ASMFrame[ST**T,LT](mv,stackClass**cl,localsClass)
      }
      
      def getInvokeMethod(cl:Class[_]) = if (cl.isInterface) INVOKEINTERFACE else INVOKEVIRTUAL
      def getInvokeMethod2(m:java.lang.reflect.Method) = 
        if ((m.getModifiers & java.lang.reflect.Modifier.STATIC) > 0)
          INVOKESTATIC
        else
          getInvokeMethod(m.getDeclaringClass)

      def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT] = 
        invokeMethod2(methodFromCode(code))
      def method_int[R<:List,T,U](rest:R,top:T,method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT] = {
        invokeMethod(method)
      }

      def invokeMethodX[R<:List,U](rest:ClassStack,m:java.lang.reflect.Method) = {
        val cl = m.getDeclaringClass
        mv.visitMethodInsn(getInvokeMethod2(m),Type.getInternalName(cl),m.getName,Type.getMethodDescriptor(m))
        new ASMFrame[R**U,LT](mv,rest ** m.getReturnType,localsClass)
      }
      def invokeMethod[R<:List,U](m:java.lang.reflect.Method) = invokeMethodX[R,U](stackClass.rest,m)
      def invokeMethod2[R<:List,U](m:java.lang.reflect.Method) = invokeMethodX[R,U](stackClass.rest.rest,m)
      
      def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT] = 
        invokeMethod(methodFromTree(code.tree))
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
    def compile[T<:AnyRef,U<:AnyRef](cl:Class[T])(code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U = {
      i+=1
      val className = "Compiled" + i

      val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
      cw.visit(V1_5,ACC_PUBLIC + ACC_SUPER,className,null,"net/virtualvoid/bytecode/AbstractFunction1", null)

      { // constructor
        val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL, "net/virtualvoid/bytecode/AbstractFunction1", "<init>", "()V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
      }

      { // apply
        val mv = cw.visitMethod(ACC_PUBLIC, "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", null, null);
        mv.visitCode()
        // put the parameter on the stackClass
        mv.visitVarInsn(ALOAD, 1);
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));

        code(new ASMFrame[Nil**T,Nil](mv,EmptyClassStack ** cl,EmptyClassStack))

        mv.visitInsn(ARETURN);
        mv.visitMaxs(1, 2)
        mv.visitEnd
      }
      cw.visitEnd
      classFromBytes(className,cw.toByteArray).newInstance.asInstanceOf[T=>U]
    }
  }