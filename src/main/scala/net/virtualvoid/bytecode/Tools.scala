package net.virtualvoid.bytecode

object CodeTools{
  import _root_.scala.reflect._
  import _root_.java.lang.reflect.{Method=>jMethod, Constructor}

  def arrayClass(cl: java.lang.Class[_]): String =
    java.lang.reflect.Array.newInstance(cl, 0).getClass.getName
  
  def cleanClass(name:String):java.lang.Class[_] = name match{
    case "int" => Integer.TYPE
    case "scala.Int" => Integer.TYPE
    case "boolean" => java.lang.Boolean.TYPE
    case "scala.Boolean" => java.lang.Boolean.TYPE
    case "double" => java.lang.Double.TYPE
    case "scala.Double" => java.lang.Double.TYPE
    case "byte" => java.lang.Byte.TYPE
    case "scala.Byte" => java.lang.Byte.TYPE
    case _ => java.lang.Class.forName(name)
  }
  
  def extractClass(tpe:Type):String = tpe match {
    case PrefixedType(_,Class(name)) => name
    case AppliedType(PrefixedType(_, Class("scala.Array")), List(PrefixedType(_, Class(name)))) => arrayClass(cleanClass(name))
    case AppliedType(tp2,_) => extractClass(tp2)
    case PrefixedType(_,TypeField(_,tp2:PrefixedType)) => extractClass(tp2)
    case NamedType(name) => name
  }
  
  def methodFromCode[T1,T2,U](code:Code[(T1,T2)=>U]) = try { 
    code.tree match{
      // functions like _.call(_)
      case Function(List(p1,p2),Apply(Select(Ident(th),Method(method,MethodType(List(LocalValue(_,_,paramTpe)),_))),List(Ident(x)))) if th == p1 && x == p2 =>{
        val paramClass = extractClass(paramTpe)
      
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
  }catch{
    case e:Exception => throw new Error("Error while calling method: "+code.tree,e);
  }
  
  def forName(clazz:String) = 
    try {
      Some(java.lang.Class.forName(clazz));
    }catch{
      case e:java.lang.ClassNotFoundException => None
    }
  def typeOfQualifier(qual:Tree):Type = qual match {
    case This(symbol) => symbol.tpe
    case Select(_,symbol) => symbol.tpe
  }

  def splitFullMethodName(method:String):(String,String) = {
    val index = method.lastIndexOf(".")
    (method.substring(0,index),method.substring(index+1))
  }
  def classNotFound(clazz:String) = new java.lang.Error("clazz missing: " +clazz)//+" in " + tree.toString) 
  def methodFromTree(tree:Tree):jMethod = try {

	    tree match {/*
	      // method call if receiver is too generic, i.e. only a bounded type parameter in the enclosing scope
	      // like [T,It<:Iterable[T]] (it:It) => it.iterator
	      case Function(
	        Stack(LocalValue(NoSymbol,x,PrefixedType(NoType,NoSymbol))),
                 Apply(Select(Ident(LocalValue(NoSymbol,x1,PrefixedType(NoType,NoSymbol))),
                         Method(method,_)),Stack())) if x == x1 => {
            val (clazz,methodName) = splitFullMethodName(method)
            val cl = forName(clazz).getOrElse(throw classNotFound(clazz))
            cl.getMethod(methodName)
          }*/
          // scala method call to method defined without parameter list
          case Function(List(x@LocalValue(_,_,tpe)),Select(Ident(x1),Method(method,_))) if x==x1 => {
            val clazz = extractClass(tpe)
            val cl = forName(clazz).getOrElse(throw classNotFound(clazz))
            val methodName = method.substring(method.lastIndexOf(".")+1)
            val m = cl.getMethod(methodName)
            m
          }
          // method call with variable receiver like '_.toString'
          case Function(List(x@LocalValue(_,_,tpe)),Apply(Select(Ident(x1),Method(method,_)),List())) if x==x1 => {
            val clazz = extractClass(tpe)
            val cl = forName(clazz).getOrElse(throw classNotFound(clazz))
            val methodName = method.substring(method.lastIndexOf(".")+1)
            val m = cl.getMethod(methodName)
            m
          }
	      // static method call with variable first parameter 'Integer.valueOf(_)'
          case Function(List(x@LocalValue(_,_,argClazz)),Apply(Select(qual,Method(method,MethodType(List(LocalValue(_,_,paramTpe)),_))),List(Ident(x1)))) if x==x1 => {
            val clazz = extractClass(typeOfQualifier(qual))
            val cl = forName(clazz).getOrElse(throw classNotFound(clazz))
            val methodName = method.substring(method.lastIndexOf(".")+1)
            val argCl = cleanClass(extractClass(paramTpe))
            val m = cl.getMethod(methodName,argCl)
            assert ((m.getModifiers & java.lang.reflect.Modifier.STATIC) != 0)
            m
          }
          case _ => throw new Error("Can't match this "+tree)
	    }
  }
  catch{
    case e:Exception => throw new Error("Error while calling method: "+tree,e);
  }

  def constructorFromTree(tree: Tree) = tree match {
    case Function(List(x1@LocalValue(_,_,tpe1)),Apply(Select(New(Ident(Class(clazz))),Method(method, _)),List(Ident(param1)))) if x1 == param1 => 
      //val clazz = extractClass(typeOfQualifier(qual))
      val cl = forName(clazz).getOrElse(throw classNotFound(clazz))
      val methodName = method.substring(method.lastIndexOf(".")+1)
      val argCl = cleanClass(extractClass(tpe1))
      val c = cl.getConstructor(argCl)
      assert (!static_?(c))
      c
    case Function(List(x1@LocalValue(_,_,tpe1), x2@LocalValue(_,_,tpe2)), 
                  Apply(Select(New(Ident(Class(clazz))),Method(method, _)), 
                        List(Ident(param1), Ident(param2))))
        if x1 == param1 && x2 == param2 => 
      val cl = forName(clazz).getOrElse(throw classNotFound(clazz))
      val methodName = method.substring(method.lastIndexOf(".")+1)
      val argCl1 = cleanClass(extractClass(tpe1))
      val argCl2 = cleanClass(extractClass(tpe2))
      val c = cl.getConstructor(argCl1, argCl2)
      assert (!static_?(c))
      c
  }
  
  import java.lang.reflect.{Member,Modifier}
  def static_?(m:Member):Boolean = (m.getModifiers & Modifier.STATIC) == Modifier.STATIC
  def fieldFromTree(tree:Tree):java.lang.reflect.Field = {
    def getField(clazz:String,field:String) = {
    	val cl = forName(clazz).getOrElse(throw new RuntimeException("Clazz not found "+clazz+" in "+tree))
		val fieldName = field.substring(field.lastIndexOf(".")+1)
		val f = cl.getField(fieldName)
		assert(static_?(f))
		f
    }
    tree match {
	    // read access
	    case Function(List(),Select(Ident(Field(clazz,_)),Field(field,_))) => getField(clazz,field)
	    // write access
	    case Function(List(varDecl),Assign(Select(Ident(Field(clazz,_)),Field(field,_)),Ident(varUse))) if varDecl == varUse=> 
	      	getField(clazz,field)
	    case _ => throw new RuntimeException("Can't match this "+tree)
    }
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
