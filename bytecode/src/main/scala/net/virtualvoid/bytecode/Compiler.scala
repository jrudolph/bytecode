package net.virtualvoid.bytecode

import Bytecode._
import java.lang.{String=>jString}

object ASMCompiler extends ByteletCompiler{
    import _root_.org.objectweb.asm._
    import Opcodes._

    case class ClassStack(mrest:ClassStack,mtop:Class[_]) 
    	extends Cons[ClassStack,Class[_]](mrest,mtop){
      def **(cl:Class[_]) = ClassStack(this,cl)
      
      def get(i:Int):Class[_] = if (i>0) rest.get(i-1) else top
      def set(i:Int,cl:Class[_]):ClassStack = 
        if (i>0) ClassStack(rest.set(i-1,cl),top) else ClassStack(rest,cl)
      def unset(i:Int):ClassStack = 
        if (i>0) ClassStack(rest.unset(i-1),top) else UnsetClassStack(rest)
      
      def firstUnset:Int =
        rest.firstUnset + 1
    }
    case class UnsetClassStack(lrest:ClassStack) extends ClassStack(lrest,null){
      override def get(i:Int):Class[_] = 
        if (i==0) 
          throw new Error("tried to get local which never was saved") 
        else super.get(i)
      override def set(i:Int,cl:Class[_]):ClassStack = 
        if (i>0) UnsetClassStack(set(i-1,cl)) else ClassStack(this,cl)
      
      override def firstUnset:Int = 0
    }
    case object EmptyClassStack extends UnsetClassStack(null)
    
    object JmpException extends RuntimeException 
    
    class ASMFrame[+ST<:List](val mv:MethodVisitor
                                      ,val stackClass:ClassStack
                                      ,val localsClass:ClassStack) 
                                      	extends F[ST]{
      def self[T]:T = this.asInstanceOf[T]

      val loopingList = new Cons(null.asInstanceOf[List],null){
        override val rest = this
        override val top = null
      }
      
      def stack = loopingList.asInstanceOf[ST]
      
      def withStack[ST2<:List](classes:ClassStack) =
        new ASMFrame[ST2](mv,classes,localsClass)
      
      def newStacked[T,ST2>:ST](cl:Class[T]) = 
        withStack(stackClass**cl)
      
      def bipush[ST2>:ST](i1:Int):F[ST2**Int] = {
        mv.visitIntInsn(BIPUSH, i1)
        newStacked(classOf[Int])
      }
      def ldc[ST2>:ST](str:jString):F[ST2**jString] = {
        mv.visitLdcInsn(str)
        newStacked(classOf[jString])
      }
      
      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int] = {
        mv.visitInsn(IADD)
        withStack(stackClass.rest)
      }
      def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int] = {
        mv.visitInsn(ISUB)
        withStack(stackClass.rest)
      }
      def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int] = {
        mv.visitInsn(IMUL)
        withStack(stackClass.rest)
      }
      def pop_int[R<:List](rest:R):F[R] = {
        mv.visitInsn(POP)
        withStack(stackClass.rest)
      }
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T] = {
        mv.visitInsn(DUP)
        withStack(stackClass**stackClass.top)
      }
      def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2] = {
        mv.visitInsn(SWAP)
        withStack(stackClass.rest.rest**stackClass.top**stackClass.rest.top)
      }
      def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1] = {
        mv.visitInsn(DUP_X1)
        withStack(
          stackClass.rest.rest**stackClass.top**
            stackClass.rest.top**stackClass.top)
      }
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U] = {
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));
        withStack(stackClass.rest**cl)
      }
      def ifne_int[R<:List](rest:R,top:JVMInt,inner:F[R] => Nothing):F[R] = {
        val l = new Label
        mv.visitJumpInsn(IFEQ,l)
        
        try{
          inner(self)
        }
        catch{
          case JmpException =>
        }

        mv.visitLabel(l)
        withStack(stackClass.rest)
      }
      import CodeTools._
      def aload_int[R<:List,T](rest:R,array:AnyRef,i:Int):F[R**T] = {
        val elType = stackClass.rest.top.getComponentType
        
        mv.visitInsn(opcode(elType,IALOAD))
        
        withStack(stackClass.rest.rest ** elType)
      }
      def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R] = {
        val elType = stackClass.rest.rest.top.getComponentType
        
        mv.visitInsn(opcode(elType,IASTORE))
        
        withStack(stackClass.rest.rest.rest)
      }
      def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int] = {
        mv.visitInsn(ARRAYLENGTH)
        
        withStack(stackClass.rest ** classOf[Int])
      }      
      def opcode(cl:Class[_],opcode:Int) = 
        Type.getType(cleanClass(cl.getName)).getOpcode(opcode)
      
      def newInstance[T,ST2>:ST](cl:Class[T]):F[ST2**T] = {
        val cons = cl.getConstructor()
        mv.visitTypeInsn(NEW,Type.getInternalName(cl))
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKESPECIAL,Type.getInternalName(cl),"<init>"
                           ,Type.getConstructorDescriptor(cons))
        withStack(stackClass**cl)
      }
      
      def getInvokeInsn(m:java.lang.reflect.Method) = 
        if ((m.getModifiers & java.lang.reflect.Modifier.STATIC) > 0)
          INVOKESTATIC
        else if (m.getDeclaringClass.isInterface)
          INVOKEINTERFACE
        else
          INVOKEVIRTUAL

      def method2_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1
                                       ,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U] = 
        invokeMethod2(methodFromCode(code))
      def method1Dyn_int[R<:List,T,U](rest:R
                                      ,top:T
                                      ,method:java.lang.reflect.Method
                                      ,resCl:Class[U]):F[R**U] = 
        invokeMethod(method)

      def invokeMethodX[R<:List,U](rest:ClassStack,m:java.lang.reflect.Method) = {
        val cl = m.getDeclaringClass
        mv.visitMethodInsn(getInvokeInsn(m),Type.getInternalName(cl)
                           ,m.getName
                           ,Type.getMethodDescriptor(m))
        withStack(rest ** m.getReturnType)
      }
      def invokeMethod[R<:List,U](m:java.lang.reflect.Method) = 
        invokeMethodX[R,U](stackClass.rest,m)
      def invokeMethod2[R<:List,U](m:java.lang.reflect.Method) = 
        invokeMethodX[R,U](stackClass.rest.rest,m)
      
      def method1_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U] = 
        invokeMethod(methodFromTree(code.tree))
      
      def getstatic_int[ST2>:ST,T](code:scala.reflect.Code[()=>T]):F[ST2**T] = {
        val field = fieldFromTree(code.tree)
        mv.visitFieldInsn(GETSTATIC
                          ,Type.getInternalName(field.getDeclaringClass)
                          ,field.getName
                          ,Type.getDescriptor(field.getType))
        newStacked(field.getType)
      }
      def putstatic_int[R<:List,T](rest:R,top:T,code:scala.reflect.Code[T=>Unit]):F[R] = {
        val field = fieldFromTree(code.tree)
        mv.visitFieldInsn(PUTSTATIC
                          ,Type.getInternalName(field.getDeclaringClass)
                          ,field.getName
                          ,Type.getDescriptor(field.getType))
        withStack(stackClass.rest)
      }
      
      def pop_unit_int[R<:List](rest:R):F[R] = 
        withStack(stackClass.rest)
      
      def conditional[R<:List,T,ST2<:List](cond:Int,rest:R,top:T
    	              					  ,thenB:F[R]=>F[ST2]
    									  ,elseB:F[R]=>F[ST2]):F[ST2] = {
        /*
         * ifeq thenLabel
         *   elseB
         *   jmp endLabel
         * thenLabel: 
         *   elseB
         * endLabel:
         */        
        
        val thenLabel = new Label
        val endLabel = new Label
        
        val frameAfterCheck = new ASMFrame[R](mv,stackClass.rest,localsClass)
        
        mv.visitJumpInsn(cond,thenLabel)
        
        val afterElseFrame = elseB(frameAfterCheck)
        
        mv.visitJumpInsn(GOTO,endLabel)
        
        mv.visitLabel(thenLabel)
        
        val afterThenFrame = thenB(frameAfterCheck)
        
        mv.visitLabel(endLabel)
        
        if (afterElseFrame.isInstanceOf[InvalidFrame])
          if (afterThenFrame.isInstanceOf[InvalidFrame])
            throw new java.lang.Error("One execution path of ifeq2 must"+
                                        " have inferable types as output")
          else
            afterThenFrame
        else            
          afterElseFrame
      }
      class InvalidFrame extends ASMFrame[Nothing](null,null,null){
        override val toString = "invalid frame" 
      }
      def invalidFrame[ST<:List]:F[ST] = 
        (new InvalidFrame).asInstanceOf[F[ST]]
      def tailRecursive_int[ST1>:ST<:List,ST2<:List]
        (func: (F[ST1] => F[ST2]) => (F[ST1]=>F[ST2]))
        (fr:F[ST1]):F[ST2] = {
          val start = new Label
          mv.visitLabel(start)
          func {f => 
            mv.visitJumpInsn(GOTO,start)
            invalidFrame
          }(this)
      }
        
      def withLocal_int[T,ST<:List,ST2<:List](top:T
                                             ,rest:ST
                                             ,code:Local[T]=>F[ST]=>F[ST2]):F[ST2] = {
        val localIndex = localsClass.firstUnset
        var localClazz:Class[_] = stackClass.top
        
        mv.visitVarInsn(opcode(localClazz,ISTORE),localIndex)
        
        val afterBlock = code(new Local[T]{
          def load[ST<:List]:F[ST] => F[ST**T] = f => {
            val asmF:ASMFrame[ST] = f.asInstanceOf[ASMFrame[ST]]
            asmF.mv.visitVarInsn(opcode(localClazz,ILOAD),localIndex)
            asmF.withStack(asmF.stackClass ** localClazz)
          }
          def store[ST<:List]:F[ST**T] => F[ST] = f => {
            val asmF:ASMFrame[ST] = f.asInstanceOf[ASMFrame[ST]]
            asmF.mv.visitVarInsn(opcode(localClazz,ISTORE),localIndex)
            asmF.withStack(asmF.stackClass.rest)
          }
        })(new ASMFrame[ST](mv,stackClass.rest,localsClass.set(localIndex,localClazz))).asInstanceOf[ASMFrame[ST2]]
        
        new ASMFrame[ST2](mv,afterBlock.stackClass,localsClass.unset(localIndex))
      }
    }
    def classFromBytes(className:String,bytes:Array[Byte]):Class[_] = {
      new java.lang.ClassLoader(getClass.getClassLoader){
        lazy val thisClass = {
          val fos = new java.io.FileOutputStream(className+".class")
          fos.write(bytes)
          fos.close
          defineClass(className,bytes,0,bytes.length)
        }
        override def findClass(name:String):java.lang.Class[_] = {
          if (name == className)
            thisClass
          else
            getParent.loadClass(name)
        }
      }.loadClass(className)
    }
    var i = 0
    def compile[T<:AnyRef,U<:AnyRef](cl:Class[T])
    (code: F[Nil**T]=>F[Nil**U]): T => U = {
      i+=1
      val className = "Compiled" + i

      val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
      cw.visit(V1_5,ACC_PUBLIC + ACC_SUPER,className,null
               ,"net/virtualvoid/bytecode/AbstractFunction1", null)

      { // constructor
        val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL
                           , "net/virtualvoid/bytecode/AbstractFunction1"
                           , "<init>"
                           , "()V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
      }

      { // apply
        val mv = cw.visitMethod(ACC_PUBLIC
                                , "apply"
                                , "(Ljava/lang/Object;)Ljava/lang/Object;"
                                , null
                                , null);
        mv.visitCode()
        // put the parameter on the stackClass
        mv.visitVarInsn(ALOAD, 1);
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));

        code(new ASMFrame[Nil**T](mv,EmptyClassStack ** cl,EmptyClassStack))

        mv.visitInsn(ARETURN);
        mv.visitMaxs(1, 2)
        mv.visitEnd
      }
      cw.visitEnd
      classFromBytes(className,cw.toByteArray).newInstance.asInstanceOf[T=>U]
    }
  }