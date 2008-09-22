package net.virtualvoid.bytecode.v2

object CodeTools{
  import scala.reflect._
  import java.lang.reflect.{Method=>jMethod}
  
  def cleanClass(name:String):java.lang.Class[_] = name match{
    case "int" => Integer.TYPE
    case "scala.Int" => Integer.TYPE
    case "boolean" => java.lang.Boolean.TYPE
    case "scala.Boolean" => java.lang.Boolean.TYPE
    case "double" => java.lang.Double.TYPE
    case "scala.Double" => java.lang.Double.TYPE
    case _ => java.lang.Class.forName(name)
  }
  
  def methodFromCode[T1,T2,U](code:Code[(T1,T2)=>U]) = code.tree match{
    case Function(List(p1,p2@LocalValue(_,_,PrefixedType(_,Class(paramClass)))),Apply(Select(Ident(th),Method(method,_)),List(Ident(x)))) if th == p1 && x == p2 =>{
      val i = method.lastIndexOf(".")
      val clName = method.substring(0,i)
      val methodName = method.substring(i+1)
      val cl = java.lang.Class.forName(clName)
      val cl2 = java.lang.Class.forName(paramClass)
      val m = cl.getMethod(methodName,cl2)
      m
    }	
    case _ => throw new Error("Can't match this "+code.tree)
  }
  def methodFromTree(tree:Tree):jMethod = tree match{
    case Function(List(x@LocalValue(_,_,PrefixedType(_,TypeField(_,PrefixedType(_,Class(clazz)))))),Apply(Select(Ident(x1),Method(method,_)),List())) if x==x1 => {
      val cl = java.lang.Class.forName(clazz)
      val methodName = method.substring(clazz.length+1)
      val m = cl.getMethod(methodName)
      m
    }
    // that's very bad duplication from the next pattern: this matches same as next but for an applied type
    case Function(List(x@LocalValue(_,_,AppliedType(PrefixedType(_,Class(clazz)),_))),Apply(Select(Ident(x1),Method(method,_)),List())) if x==x1 => {
      val cl = java.lang.Class.forName(clazz)
      val methodName = method.substring(clazz.length+1)
      val m = cl.getMethod(methodName)
      m
    }
    // match simple function applications like i=>i.intValue or (_:java.lang.Integer).intValue
    case Function(List(x@LocalValue(_,_,PrefixedType(_,Class(clazz)))),Apply(Select(Ident(x1),Method(method,_)),List())) if x==x1 => {
      val cl = java.lang.Class.forName(clazz)
      val methodName = method.substring(clazz.length+1)
      val m = cl.getMethod(methodName)
      m
    }
    case Function(List(x),Apply(Select(_,Method(method,MethodType(List(PrefixedType(_,Class(argClazz))),PrefixedType(_,Class(clazz))))),List(Ident(x1)))) if x==x1 => {
      val cl = java.lang.Class.forName(clazz)
      val methodName = method.substring(clazz.length+1)
      val argCl = cleanClass(argClazz)
      val m = cl.getMethod(methodName,argCl)
      m
    }
    case _ => throw new Error("Can't match this "+tree)
  }
  
  def box(a:Any):AnyRef = a match{
    case i:Int => Integer.valueOf(i)
    case d:Double => java.lang.Double.valueOf(d)
    case f:Float => java.lang.Float.valueOf(f)
    case b:Boolean => java.lang.Boolean.valueOf(b)
    case o:AnyRef => o
  }
  def invokeMethod(method:jMethod,args:Any*) = {
    if ((method.getModifiers & java.lang.reflect.Modifier.STATIC) != 0)
      method.invoke(null,args.map(box).toArray:_*)
    else
      method.invoke(args(0),args.drop(1).map(box).toArray:_*)
  }
}

object Bytecode{
  import java.lang.{String => jString,
                    Boolean => jBoolean
  }

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

  trait Target[ST<:List,LT<:List] extends F[ST,LT]

  trait F[ST<:List,LT<:List]{
    def stack:ST
    def locals:LT

    def bipush(i1:Int):F[ST**Int,LT]
    def ldc(str:jString):F[ST**jString,LT]
    def target:Target[ST,LT]
    def jmp(t:Target[ST,LT]):Nothing

    def op[STR<:List,LTR<:List](f:F[ST,LT]=>F[STR,LTR]):F[STR,LTR] = f(this)

    def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT]
    def pop_int[R<:List](rest:R):F[R,LT]
    def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT]
    def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2,LT]
    def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1,LT]
    def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def method_int[R<:List,T,U](rest:R,top:T,method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT]
    def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT]
    def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT]
    def ifeq_int[R<:List](rest:R,top:Any,inner:F[R,LT] => Nothing):F[R,LT]
    def aload_int[R<:List,T](rest:R,array:AnyRef/*Array[T]*/,i:Int):F[R**T,LT]
    def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT]
    def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT]

    def newInstance[T](cl:Class[T]):F[ST**T,LT]
    
    def loadI[T](i:Int):F[ST**T,LT]
    def storeI[R<:List,T,NewLT<:List](rest:R,top:T,i:Int):F[R,NewLT]
  }
  trait Int2Stack[ST<:List,LT<:List]{
    def i1:Int
    def i2:Int
    def rest:ST
    def frame:F[_,LT]
    def iadd():F[ST**Int,LT] = frame.iadd_int[ST](rest,i1,i2)
    def isub():F[ST**Int,LT] = frame.isub_int[ST](rest,i1,i2)
    def imul():F[ST**Int,LT] = frame.imul_int[ST](rest,i1,i2)
  }
  trait OneStack[R<:List,T,LT<:List]{
    def pop():F[R,LT]
    def dup():F[R**T**T,LT]
    def method[U](code:scala.reflect.Code[T=>U]):F[R**U,LT]
    def dynMethod[U](method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT]
    def checkcast[U](cl:Class[U]):F[R**U,LT]
  }
  trait TwoStack[R<:List,T2,T1,LT<:List]{
    def method2[U](code:scala.reflect.Code[(T2,T1) => U]):F[R**U,LT]
    def swap():F[R**T1**T2,LT]
    def dup_x1():F[R**T1**T2**T1,LT]
  }
  class BooleanStack[R<:List,LT<:List,X](f:F[R**X,LT]){
    def ifeq(inner:F[R,LT] => Nothing):F[R,LT] =
      f.ifeq_int(f.stack.rest,f.stack.top,inner)
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
      def dynMethod[U](method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT] =
        if (resCl.isAssignableFrom(method.getReturnType))
          f.method_int(f.stack.rest,f.stack.top,method,resCl)
        else
          throw new Error("incompatible Method")
      def checkcast[U](cl:Class[U]):F[R**U,LT] = f.checkcast_int(f.stack.rest,f.stack.top)(cl)
    }
    implicit def twoStack[R<:List,LT<:List,T1,T2](f:F[R**T2**T1,LT]):TwoStack[R,T2,T1,LT] = new TwoStack[R,T2,T1,LT]{
      def method2[U](code:scala.reflect.Code[(T2,T1) => U]):F[R**U,LT] =
        f.method_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top,code)
      def swap():F[R**T1**T2,LT] = f.swap_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
      def dup_x1():F[R**T1**T2**T1,LT] = f.dup_x1_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    }
    implicit def booleanStack[R<:List,LT<:List](f:F[R**Boolean,LT]):BooleanStack[R,LT,Boolean] = new BooleanStack(f)
    implicit def intBooleanStack[R<:List,LT<:List](f:F[R**Int,LT]):BooleanStack[R,LT,Int] = new BooleanStack(f)
    trait ArrayLoadStack[R<:List,T,LT<:List]{
      def aload():F[R**T,LT]
    }
    implicit def arrayLoadStack[R<:List,T,LT<:List](f:F[R**Array[T]**Int,LT]) = new ArrayLoadStack[R,T,LT](){
      def aload() = f.aload_int(f.stack.rest.rest,f.stack.rest.top,f.stack.top)
    }
    trait ArrayStoreStack[R<:List,LT<:List]{
      def astore():F[R,LT]
    }
    implicit def arrayStoreStack[R<:List,T,LT<:List](f:F[R**Array[T]**Int**T,LT]) = new ArrayStoreStack[R,LT]{
      def astore():F[R,LT] = f.astore_int(f.stack.rest.rest.rest,f.stack.rest.rest.top,f.stack.rest.top,f.stack.top)
    }
    trait ArrayLengthStack[R<:List,LT<:List]{
      def arraylength():F[R**Int,LT]
    }
    implicit def arrayLengthStack[R<:List,LT<:List,T](f:F[R**Array[T],LT]) = new ArrayLengthStack[R,LT]{
      def arraylength():F[R**Int,LT] = f.arraylength_int(f.stack.rest,f.stack.top)
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
	  def compile[T<:AnyRef,U<:AnyRef](cl:Class[T])(
                       code: F[Nil**T,Nil] // gets a parameter of type T on the stack
	                      => F[Nil**U,_]   // and uses it and has then a value of type U on the stack
	  ): T => U
  }

  object Interpreter extends ByteletCompiler{
    case class IF[ST<:List,LT<:List](stack:ST,locals:LT) extends F[ST,LT]{
      import CodeTools._
      
      def bipush(i1:Int):F[ST**Int,LT] = IF(stack ** i1,locals)
      def ldc(str:jString):F[ST**jString,LT] = IF(stack ** str,locals)
      def target:Target[ST,LT] = null
      def jmp(t:Target[ST,LT]):Nothing = null.asInstanceOf[Nothing]

      def iadd_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1+i2),locals)
      def isub_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1-i2),locals)
      def imul_int[R<:List](rest:R,i1:Int,i2:Int):F[R**Int,LT] = IF(rest ** (i1*i2),locals)
      def pop_int[R<:List](rest:R):F[R,LT] = IF(rest,locals)
      def dup_int[R<:List,T](rest:R,top:T):F[R**T**T,LT] = IF(rest**top**top,locals)
      def swap_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2,LT] = IF(rest**t1**t2,locals)
      def dup_x1_int[R<:List,T1,T2](rest:R,t2:T2,t1:T1):F[R**T1**T2**T1,LT] = IF(rest**t1**t2**t1,locals)
      def method_int[R<:List,T,U](rest:R,top:T,code:scala.reflect.Code[T=>U]):F[R**U,LT] = 
        IF(rest ** invokeMethod(methodFromTree(code.tree),top).asInstanceOf[U],locals)
      def method_int[R<:List,T,U](rest:R,top:T,method:java.lang.reflect.Method,resCl:Class[U]):F[R**U,LT] =
        IF(rest ** method.invoke(top).asInstanceOf[U],locals)
      def method_int[R<:List,T2,T1,U](rest:R,top2:T2,top1:T1,code:scala.reflect.Code[(T2,T1)=>U]):F[R**U,LT] = 
        IF(rest ** invokeMethod(methodFromCode(code),top2,top1).asInstanceOf[U],locals)
      def checkcast_int[R<:List,T,U](rest:R,top:T)(cl:Class[U]):F[R**U,LT] = IF(rest**top.asInstanceOf[U],locals)
      def ifeq_int[R<:List](rest:R,top:Any,inner:F[R,LT] => Nothing):F[R,LT] = null
      
      import java.lang.reflect.{Array => jArray}
      def aload_int[R<:List,T](rest:R,array:AnyRef,i:Int):F[R**T,LT] = {
        IF(rest**jArray.get(array,i).asInstanceOf[T],locals)
      }
      def astore_int[R<:List,T](rest:R,array:AnyRef,index:Int,t:T):F[R,LT] = {
        jArray.set(array,index,t)
        IF(rest,locals)
      }
      def arraylength_int[R<:List](rest:R,array:AnyRef):F[R**Int,LT] = 
        IF(rest**jArray.getLength(array),locals)

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
      
      def newInstance[T](cl:Class[T]):F[ST**T,LT] = 
        IF(stack**cl.newInstance,locals)
    }

    def compile[T,U](cl:Class[T])(code: F[Nil**T,Nil]=>F[Nil**U,_]): T => U =
      t => code(IF(N**t,N)).stack.top
  }
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
      def ifeq_int[R<:List](rest:R,top:Any,inner:F[R,LT] => Nothing):F[R,LT] = {
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
}

abstract class AbstractFunction1[T,U] extends Function1[T,U]

object Test{
  def main(args:Array[String]):Unit = {
    import Bytecode._
    import Bytecode.Implicits._
    import java.lang.{Integer=>jInt}

    val func = Interpreter.compile(classOf[AnyRef])(_.pop.bipush(12).dup.iadd.l.store.e.l.load.e.dup.imul)
    System.out.println(func(null))

    val func2 = ASMCompiler.compile(classOf[java.lang.String])(
      _.dup
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
    val csucc: jInt => jInt = Bytecode.ASMCompiler.compile(classOf[jInt])(bcs)
    val isucc = csucc

    def testRun(i:Int) {
      System.out.println(String.format("Test for %d interpreted %d compiled %d same %s"
                           ,int2Integer(i.intValue),int2Integer(isucc(i).intValue),int2Integer(csucc(i).intValue),(isucc(i)==csucc(i)).toString))
    }

    testRun(1)
    testRun(2)

    val f = ASMCompiler.compile(classOf[java.lang.String])(
      _.method(_.length).bipush(3).imul.method(Integer.valueOf(_)))
    System.out.println(f("123"))
    System.out.println(f("123456"))
  }
}
