package net.virtualvoid.bytecode

import Bytecode._
import java.lang.{String=>jString}

object ASMCompiler extends ByteletCompiler{
    import _root_.org.objectweb.asm._
    import Opcodes._

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
    
    object JmpException extends RuntimeException
    
    final case class CompilationData(className:String,cw:ClassWriter,constructor:MethodVisitor){
      var lazyValNum = 0
      def nextLazyVal = {
        lazyValNum += 1
        "lazyVal" + lazyValNum
      }
    }
    
    class ASMFrame[ST<:List,LT<:List](mv:MethodVisitor,stackClass:ClassStack,localsClass:ClassStack,data:CompilationData) extends F[ST,LT]{
      def self[T]:T = this.asInstanceOf[T]

      val loopingList = new Cons(null.asInstanceOf[List],null){
        override val rest = this
        override val top = null
      }
      
      def stack = loopingList.asInstanceOf[ST]
      def locals = loopingList.asInstanceOf[LT]
      
      def newFrame[S<:List,L<:List](stackClasses:ClassStack,localsClasses:ClassStack) = 
        new ASMFrame[S,L](mv,stackClasses,localsClasses,data)
      
      def newStack[NST<:List](stackClasses:ClassStack):F[NST,LT] = 
        newFrame(stackClasses,localsClass)
      
      def newOnStack[T](cl:Class[T]):F[ST**T,LT] = 
        newStack(stackClass**cl)
      
      def bipush(i1:Int):F[ST**Int,LT] = {
        mv.visitIntInsn(BIPUSH, i1)
        newOnStack(classOf[Int])
      }
      def ldc(str:jString):F[ST**jString,LT] = {
        mv.visitLdcInsn(str)
        newOnStack(classOf[jString])
      }
      
      trait ASMTarget{
        def label:Label
      }
      
      case class ASMBackwardTarget[ST<:List,LT<:List](mv:MethodVisitor,stackClass:ClassStack,localsClass:ClassStack,label:Label,data:CompilationData)
          extends ASMFrame[ST,LT](mv,stackClass,localsClass,data) with BackwardTarget[ST,LT] with ASMTarget
      def target:BackwardTarget[ST,LT] = {
        val label = new Label
        mv.visitLabel(label)
        ASMBackwardTarget(mv,stackClass,localsClass,label,data)
      }
      
      case class ASMForwardTarget[ST<:List,LT<:List](label:Label) extends ForwardTarget[ST,LT] with ASMTarget
      def forwardTarget[ST<:List,LT<:List] = {
        val label = new Label
        ASMForwardTarget(label)
      }
      def targetHere(t:ForwardTarget[ST,LT]):F[ST,LT] = { // TODO: make sure a label isn't visited twice
        mv.visitLabel(t.asInstanceOf[ASMForwardTarget[ST,LT]].label)
        this
      }

      def jmp(t:Target[ST,LT]):Nothing = {
        mv.visitJumpInsn(GOTO,t.asInstanceOf[ASMTarget].label)
        throw JmpException
      }
      
      def integerBinOp[R<:List](ins:Int):F[R**Int,LT] = {
        mv.visitInsn(ins)
        newStack(stackClass.rest)
      }

      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] =
        integerBinOp(IADD)
      
      def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] =
        integerBinOp(ISUB)
      
      def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] =
        integerBinOp(IMUL)
        
      def pop_int[R<:List](rest:R):F[R,LT] = {
        mv.visitInsn(POP)
        newStack(stackClass.rest)
      }
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT] = {
        mv.visitInsn(DUP)
        newStack(stackClass**stackClass.top)
      }
      def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2,LT] = {
        mv.visitInsn(SWAP)
        newStack(stackClass.rest.rest**stackClass.top**stackClass.rest.top)
      }
      def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1,LT] = {
        mv.visitInsn(DUP_X1)
        newStack(stackClass.rest.rest**stackClass.top**stackClass.rest.top**stackClass.top)
      }
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT] = {
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));
        newStack(stackClass.rest**cl)
      }
      def ifeq_int[R<:List](rest:R,top:JVMInt,inner:F[R,LT] => Nothing):F[R,LT] = {
        val l = new Label
        mv.visitJumpInsn(IFEQ,l)
        
        try{
          inner(self)
        }
        catch{
          case JmpException =>
        }

        mv.visitLabel(l)
        newStack(stackClass.rest)
      }
      import CodeTools._
      def aload_int[R<:List,T](rest:R,array:AnyRef,i:Int):F[R**T,LT] = {
        val elType = stackClass.rest.top.getComponentType
        
        mv.visitInsn(opcode(elType,IALOAD))
        
        newStack(stackClass.rest.rest ** elType)
      }
      def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT] = {
        val elType = stackClass.rest.rest.top.getComponentType
        
        mv.visitInsn(opcode(elType,IASTORE))
        
        newStack(stackClass.rest.rest.rest)
      }
      def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT] = {
        mv.visitInsn(ARRAYLENGTH)
        
        newStack(stackClass.rest ** classOf[Int])
      }      
      def opcode(cl:Class[_],opcode:Int) = 
        Type.getType(cleanClass(cl.getName)).getOpcode(opcode)

      def loadI[T](i:Int):F[ST**T,LT] = {
        val toLoad = localsClass.get(i)
        mv.visitVarInsn(opcode(toLoad,ILOAD), i);
        newStack(stackClass**toLoad)
      }
      def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT] = {
        mv.visitVarInsn(opcode(stackClass.top,ISTORE), i);
        newFrame(stackClass.rest,localsClass.set(i,stackClass.top))
      }
      
      def newInstance[T](cl:Class[T]):F[ST**T,LT] = {
        val cons = cl.getConstructor()
        mv.visitTypeInsn(NEW,Type.getInternalName(cl))
        mv.visitInsn(DUP)
        mv.visitMethodInsn(INVOKESPECIAL,Type.getInternalName(cl),"<init>",Type.getConstructorDescriptor(cons))
        newStack(stackClass**cl)
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

      def invokeMethodX[R<:List,U](rest:ClassStack,m:java.lang.reflect.Method):F[R**U,LT] = {
        val cl = m.getDeclaringClass
        mv.visitMethodInsn(getInvokeMethod2(m),Type.getInternalName(cl),m.getName,Type.getMethodDescriptor(m))
        newStack(rest ** m.getReturnType)
      }
      def invokeMethod[R<:List,U](m:java.lang.reflect.Method) = invokeMethodX[R,U](stackClass.rest,m)
      def invokeMethod2[R<:List,U](m:java.lang.reflect.Method) = invokeMethodX[R,U](stackClass.rest.rest,m)
      
      def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT] = 
        invokeMethod(methodFromTree(code.tree))
      
      def pop_unit_int[R<:List](rest:R):F[R,LT] = newStack(stackClass.rest)
      
      def ifeq2_int[R<:List,ST2<:List,LT2<:List](rest:R,top:JVMInt,then:F[R,LT]=>F[ST2,LT2],elseB:F[R,LT]=>F[ST2,LT2]):F[ST2,LT2] = {
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
        
        val frameAfterCheck = newStack[R](stackClass.rest)
        
        mv.visitJumpInsn(IFEQ,thenLabel)
        
        val afterElseFrame = elseB(frameAfterCheck)
        
        mv.visitJumpInsn(GOTO,endLabel)
        
        mv.visitLabel(thenLabel)
        
        val afterThenFrame = then(frameAfterCheck)
        
        mv.visitLabel(endLabel)
        
        if (afterElseFrame.isInstanceOf[InvalidFrame])
          if (afterThenFrame.isInstanceOf[InvalidFrame])
            throw new java.lang.Error("One execution path of ifeq2 must have inferable types as output")
          else
            afterThenFrame
        else            
          afterElseFrame
      }
      class InvalidFrame extends ASMFrame[Nothing,Nothing](null,null,null,null){
        override val toString = "invalid frame" 
      }
      def invalidFrame[ST<:List,LT<:List]:F[ST,LT] = (new InvalidFrame).asInstanceOf[F[ST,LT]]
      def tailRecursive_int[ST2<:List,LT2<:List]
        (func: (F[ST,LT] => F[ST2,LT2]) => (F[ST,LT]=>F[ST2,LT2]))(fr:F[ST,LT]):F[ST2,LT2] = {
          val start = new Label
          mv.visitLabel(start)
          func {f => 
            mv.visitJumpInsn(GOTO,start)
            invalidFrame
          }(this)
      }
      def lazyVal_int[T<:AnyRef](tpe:Class[T],init:F[Nil,Nil] => F[Nil**T,Nil]):F[ST**T,LT] = {
        /* 1. getField
         * 2. null     => init, storeField, return
         * 3. not-null => return
         */
        
        //create new private field and initialization
        val name = data.nextLazyVal
        
        def fieldInsn(mv:MethodVisitor,insn:Int)(func: MethodVisitor=>Unit){
          mv.visitVarInsn(ALOAD,0)
          func(mv)
          mv.visitFieldInsn(insn,data.className,name,Type.getDescriptor(tpe))
        }
        
        data.cw.visitField(ACC_PRIVATE,name,Type.getDescriptor(tpe),null,null).visitEnd
        fieldInsn(data.constructor,PUTFIELD)(_.visitInsn(ACONST_NULL))


        val afterInit = new Label
        
        fieldInsn(mv,GETFIELD){mv=>}
        mv.visitInsn(DUP)
        mv.visitJumpInsn(IFNONNULL,afterInit)
        
        // initialize field
        mv.visitInsn(POP) // throw away dupped null
        init(newFrame(EmptyClassStack,EmptyClassStack))
        mv.visitInsn(DUP)
        fieldInsn(mv,PUTFIELD){_.visitInsn(SWAP)}
        
        mv.visitLabel(afterInit)
        
        newOnStack[T](tpe)
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
    def compile[T<:AnyRef,U<:AnyRef](cl:Class[T])(code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U = {
      i+=1
      val className = "Compiled" + i

      val cw = new ClassWriter(ClassWriter.COMPUTE_MAXS)
      cw.visit(V1_5,ACC_PUBLIC + ACC_SUPER,className,null,"net/virtualvoid/bytecode/AbstractFunction1", null)
      
      //create constructor and let it open for further initialization
      val cons_mv = cw.visitMethod(ACC_PUBLIC, "<init>", "()V", null, null);
      cons_mv.visitCode();
      cons_mv.visitVarInsn(ALOAD, 0);
      cons_mv.visitMethodInsn(INVOKESPECIAL, "net/virtualvoid/bytecode/AbstractFunction1", "<init>", "()V");

      { // apply
        val mv = cw.visitMethod(ACC_PUBLIC, "apply", "(Ljava/lang/Object;)Ljava/lang/Object;", null, null);
        mv.visitCode()
        // put the parameter on the stackClass
        mv.visitVarInsn(ALOAD, 1);
        mv.visitTypeInsn(CHECKCAST, Type.getInternalName(cl));

        code(new ASMFrame[Nil**T,Nil](mv,EmptyClassStack ** cl,EmptyClassStack,CompilationData(className,cw,cons_mv)))

        mv.visitInsn(ARETURN);
        mv.visitMaxs(1, 2)
        mv.visitEnd
      }
      
      // close constructor
      cons_mv.visitInsn(RETURN);
      cons_mv.visitMaxs(1, 1);      
      cons_mv.visitEnd();
      
      cw.visitEnd
      classFromBytes(className,cw.toByteArray).newInstance.asInstanceOf[T=>U]
    }
  }