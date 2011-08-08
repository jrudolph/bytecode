package net.virtualvoid.bytecode
package backend

import Bytecode._
import java.lang.{String=>jString}

object ASM extends ByteletCompiler{
    import _root_.org.objectweb.asm._
    import Opcodes._

    trait ClassStack {
      def **(cl:Class[_]) = ConsClassStack(this,cl)
      def rest:ClassStack
      def top:Class[_]
      def popN(n:Int):ClassStack = if (n > 0) rest.popN(n-1) else this
    }
    case class ConsClassStack(mrest:ClassStack,mtop:Class[_]) 
    	extends ClassStack {
      def top:Class[_] = mtop
      def rest:ClassStack = mrest
    }
    case object EmptyClassStack extends ClassStack{
      def top:Class[_] = throw new Error("Tried to get top element of EmptyStack")
      def rest:ClassStack = throw new Error("Tried to pop class from empty class stack, this is a bug.")
    }
    
    object JmpException extends RuntimeException 
    
    class ASMFrame[+ST<:Stack](val mv:MethodVisitor
                                      ,val stackClass:ClassStack
                                      ,val nextFreeLocal:Int) 
                                      	extends F[ST]{
      def self[T]:T = this.asInstanceOf[T]

      val loopingList = new Cons(null.asInstanceOf[Stack],null){
        override val rest = this
        override val top = null
      }
      
      def stack = loopingList.asInstanceOf[ST]
      
      def withStack[ST2<:Stack](classes:ClassStack) =
        new ASMFrame[ST2](mv,classes,nextFreeLocal)
      
      def newStacked[T,ST2>:ST<:Stack](cl:Class[T]) =
        withStack(stackClass**cl)
      
      def bipush[ST2>:ST<:Stack](i1:Int):F[ST2**Int] = {
        mv.visitIntInsn(BIPUSH, i1)
        newStacked(classOf[Int])
      }
      def ldc[ST2>:ST<:Stack](str:jString):F[ST2**jString] = {
        mv.visitLdcInsn(str)
        newStacked(classOf[jString])
      }
      
      def iadd_int[R<:Stack](rest:R,i1:Int,i2:Int):F[R**Int] = {
        mv.visitInsn(IADD)
        withStack(stackClass.rest)
      }
      def isub_int[R<:Stack](rest:R,i1:Int,i2:Int):F[R**Int] = {
        mv.visitInsn(ISUB)
        withStack(stackClass.rest)
      }
      def imul_int[R<:Stack](rest:R,i1:Int,i2:Int):F[R**Int] = {
        mv.visitInsn(IMUL)
        withStack(stackClass.rest)
      }
      def withCategory(insns:(Int,Int)):Int = size(stackClass.top) match {
        case 1 => insns._1
        case 2 => insns._2
      }
      val pop = (POP,POP2)
      val dup = (DUP,DUP2)
      val dup_x1 = (DUP_X1,DUP2_X1)
      def pop_int[R<:Stack](rest:R):F[R] = {
        mv.visitInsn(withCategory(pop))
        withStack(stackClass.rest)
      }
      def dup_int[R<:Stack,T](rest:R,top:T):F[R**T**T] = {
        mv.visitInsn(withCategory(dup))
        withStack(stackClass**stackClass.top)
      }
      def swap_int[R<:Stack,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2] = {
        mv.visitInsn(SWAP)
        withStack(stackClass.rest.rest**stackClass.top**stackClass.rest.top)
      }
      def dup_x1_int[R<:Stack,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1] = {
        mv.visitInsn(withCategory(dup_x1))
        withStack(
          stackClass.rest.rest**stackClass.top**
            stackClass.rest.top**stackClass.top)
      }
      def checkcast_int[R<:Stack,T,U](rest:R,top:T)(cl:Class[U]):F[R**U] = {
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));
        withStack(stackClass.rest**cl)
      }
      def ifne_int[R<:Stack](rest:R,top:JVMInt,inner:F[R] => Nothing):F[R] = {
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
      def aload_int[R<:Stack,T](rest:R,array:AnyRef,i:Int):F[R**T] = {
        val elType = stackClass.rest.top.getComponentType
        
        mv.visitInsn(opcode(elType,IALOAD))
        
        withStack(stackClass.rest.rest ** elType)
      }
      def astore_int[R<:Stack,T](rest:R,array:AnyRef,index:Int,t:T):F[R] = {
        val elType = stackClass.rest.rest.top.getComponentType
        
        mv.visitInsn(opcode(elType,IASTORE))
        
        withStack(stackClass.rest.rest.rest)
      }
      def arraylength_int[R<:Stack](rest:R,array:AnyRef):F[R**Int] = {
        mv.visitInsn(ARRAYLENGTH)
        
        withStack(stackClass.rest ** classOf[Int])
      }      
      
      
      def newInstance[T,ST2>:ST<:Stack](cl:Class[T]):F[ST2**T] = {
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

      def invokemethod[R<:Stack,U](handle:MethodHandle)
                                   :F[R**U] = {
        val m = handle.method                                     
        val cl = m.getDeclaringClass
        mv.visitMethodInsn(getInvokeInsn(m)
                          ,Type.getInternalName(cl)
                          ,m.getName
                          ,Type.getMethodDescriptor(m))
        withStack(stackClass.popN(handle.numParams) ** m.getReturnType)
      }
      def invokeconstructor[R<:Stack,U](cons: Constructor)
                                   :F[R**U] = {
        val cl = cons.constructor.getDeclaringClass
        mv.visitMethodInsn(INVOKESPECIAL
                          ,Type.getInternalName(cl)
                          ,"<init>"
                          ,Type.getConstructorDescriptor(cons.constructor))
        withStack(stackClass.popN(cons.numParams) ** cl)
      }
      def new_int[ST2 >: ST <: Stack, U](cl: Class[U]): F[ST2**Uninitialized[U]] = {
        mv.visitTypeInsn(NEW,Type.getInternalName(cl))
        withStack(stackClass ** cl)
      }
                                   
      def getstatic_int[ST2>:ST<:Stack,T](code:scala.reflect.Code[()=>T]):F[ST2**T] = {
        val field = fieldFromTree(code.tree)
        mv.visitFieldInsn(GETSTATIC
                          ,Type.getInternalName(field.getDeclaringClass)
                          ,field.getName
                          ,Type.getDescriptor(field.getType))
        newStacked(field.getType)
      }
      def putstatic_int[R<:Stack,T](rest:R,top:T,code:scala.reflect.Code[T=>Unit]):F[R] = {
        val field = fieldFromTree(code.tree)
        mv.visitFieldInsn(PUTSTATIC
                          ,Type.getInternalName(field.getDeclaringClass)
                          ,field.getName
                          ,Type.getDescriptor(field.getType))
        withStack(stackClass.rest)
      }
      
      def pop_unit_int[R<:Stack](rest:R):F[R] =
        withStack(stackClass.rest)
      
      def conditional[R<:Stack,T,ST2<:Stack](cond:Int,rest:R,top:T
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
        
        val frameAfterCheck = new ASMFrame[R](mv,stackClass.rest,nextFreeLocal)
        
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
      class InvalidFrame extends ASMFrame[Nothing](null,null,0){
        override val toString = "invalid frame" 
      }
      def invalidFrame[ST<:Stack]:F[ST] =
        (new InvalidFrame).asInstanceOf[F[ST]]
      def tailRecursive_int[ST1>:ST<:Stack,ST2<:Stack]
        (func: (F[ST1] => F[ST2]) => (F[ST1]=>F[ST2]))
        (fr:F[ST1]):F[ST2] = {
          val start = new Label
          mv.visitLabel(start)
          func {f => 
            mv.visitJumpInsn(GOTO,start)
            invalidFrame
          }(this)
      }
      def size(cl:Class[_]) = {
        val Double = classOf[Double]
        val Long = classOf[Long]
        cl match {
          case Double => 2
          case Long => 2
          case _ => 1
        }
      }
      def withLocal_int[T,ST<:Stack,ST2<:Stack](top:T
                                             ,rest:ST
                                             ,code:Local[T]=>F[ST]=>F[ST2]):F[ST2] = {
        val localIndex = nextFreeLocal
        var localClazz:Class[_] = stackClass.top
        
        mv.visitVarInsn(opcode(localClazz,ISTORE),localIndex)
        
        val afterBlock = code(local(localIndex,localClazz))(new ASMFrame[ST](mv,stackClass.rest,nextFreeLocal + size(stackClass.top))).asInstanceOf[ASMFrame[ST2]]
        
        new ASMFrame[ST2](mv,afterBlock.stackClass,nextFreeLocal)
      }
      def withTargetHere_int[X,ST2>:ST<:Stack](code:Target[ST2] => F[ST2] => X):X = {
        val label = new Label
        mv.visitLabel(label)
        code(new Target[ST2]{
          def jmp:F[ST2] => Nothing = {_ => mv.visitJumpInsn(GOTO,label); throw JmpException}                                               
        })(this)
      }
      def conditionalImperative[R<:Stack,T,ST2<:Stack](cond:Int,rest:R,top:T
    											  ,thenB:F[R]=>Nothing):F[R] = {
        val afterCondition:F[R] = withStack(stackClass.rest)
        
        val after = new Label
    	mv.visitJumpInsn(cond,after)
    	try{
    	  thenB(afterCondition)
    	  throw new RuntimeException("Control-flow exception expected")
    	}catch{
    	  case JmpException => // do nothing
    	}
    	mv.visitLabel(after)
    	afterCondition
      }
      def lookupSwitch[R <: Stack, ST2 <: Stack](cond: Int, rest: R)(candidates: Int*)(mapping: F[R] => PartialFunction[Option[Int], F[ST2]]): F[ST2] = {
        // TODO: emit warnings/errors if mapping doesn't include all the candidates
        val map = mapping(withStack(stackClass.rest))

        // the label to jump to after executing the branch, where the control
        // flow is joined again
        val afterLabel = new Label

        def emitBranch(cond: Option[Int], branchLabel: Label): F[ST2] = {
          mv.visitLabel(branchLabel)
          val res = map(cond)
          mv.visitJumpInsn(GOTO, afterLabel)
          res
        }

        val candsAndLabels = candidates.map(i => (Some(i), new Label))
        val dfltLabel = new Label

        mv.visitLookupSwitchInsn(dfltLabel, candidates.toArray, candsAndLabels.map(_._2).toArray)
        candsAndLabels.view.unzip.zipped foreach emitBranch
        val res = emitBranch(None, dfltLabel)
        mv.visitLabel(afterLabel)
        res
      }
    }

    def opcode(cl:Class[_],opcode:Int) = 
        Type.getType(CodeTools.cleanClass(cl.getName)).getOpcode(opcode)
    def local[T](index:Int,clazz:Class[_]):Local[T] = 
        new Local[T]{
          def load[ST <: Stack,T2>:T]: F[ST] => F[ST**T2] = f => {
            val asmF:ASMFrame[ST] = f.asInstanceOf[ASMFrame[ST]]
            asmF.mv.visitVarInsn(opcode(clazz,ILOAD),index)
            asmF.withStack(asmF.stackClass ** clazz)
          }
          def store[ST<:Stack]:F[ST**T] => F[ST] = f => {
            val asmF:ASMFrame[ST] = f.asInstanceOf[ASMFrame[ST]]
            asmF.mv.visitVarInsn(opcode(clazz,ISTORE),index)
            asmF.withStack(asmF.stackClass.rest)
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
    def ret[U](mv:MethodVisitor):Return[U] = new Return[U]{def jmp:F[Nil**U] => Nothing = {f => mv.visitInsn(ARETURN);throw JmpException}}
    
    var i = 0
    def compile[T<:AnyRef,U<:AnyRef](cl:Class[T],retCl:Class[U])(
                       code: Local[T] => Return[U] => F[Nil] => Nothing   
	  ): T => U = {
      classStub[T=>U](cl){ mv =>
        code(local(1,cl))(ret(mv))(new ASMFrame[Nil](mv,EmptyClassStack,2))
      }
    }
    def compile[T1<:AnyRef,T2<:AnyRef,U<:AnyRef](cl1:Class[T1],cl2:Class[T2],retCl:Class[U])(
	    code: (Local[T1],Local[T2]) => Return[U] => F[Nil] => Nothing
	  ): (T1,T2) => U = {
      classStub[(T1,T2)=>U](cl1,cl2){ mv =>
        code(local(1,cl1),local(2,cl2))(ret(mv))(new ASMFrame[Nil](mv,EmptyClassStack,3))
      }
    }
    def classStub[T](params:Class[_]*)(body: MethodVisitor => Unit) :T = {
      val numParams = params.length
      val superClass = "scala/runtime/AbstractFunction"+numParams
      val signature = "("+"Ljava/lang/Object;"*numParams+")Ljava/lang/Object;"
      
      i+=1
      val className = "Compiled" + i

      val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
      cw.visit(V1_5,ACC_PUBLIC + ACC_SUPER,className,null
               ,superClass, null)

      { // constructor
        val mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
        mv.visitCode();
        mv.visitVarInsn(ALOAD, 0);
        mv.visitMethodInsn(INVOKESPECIAL
                           , superClass
                           , "<init>"
                           , "()V");
        mv.visitInsn(RETURN);
        mv.visitMaxs(1, 1);
        mv.visitEnd();
      }

      { // apply
        val mv = cw.visitMethod(ACC_PUBLIC
                                , "apply"
                                , signature
                                , null
                                , null);
        mv.visitCode()

        for (i <- 1 to numParams){
	        mv.visitVarInsn(ALOAD, i);
	        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(params(i-1)));
	        mv.visitVarInsn(ASTORE,i);
        }

        try{
        	body(mv)
        	throw new RuntimeException("Return statement is missing")
        }catch{
          case JmpException => // we expect to end by catching an exception
        }

        //mv.visitInsn(ARETURN);
        mv.visitMaxs(1, 2)
        mv.visitEnd
      }
      cw.visitEnd
      classFromBytes(className,cw.toByteArray).newInstance.asInstanceOf[T]
    }
  }

abstract class AbstractFunction1[T,U] extends Function1[T,U]
abstract class AbstractFunction2[T1,T2,U] extends Function2[T1,T2,U]
