package net.virtualvoid.bytecode

object CodeTools{
  import _root_.scala.reflect._
  import _root_.java.lang.reflect.{Method=>jMethod}
  
  def cleanClass(name:String):java.lang.Class[_] = name match{
    case "int" => Integer.TYPE
    case "scala.Int" => Integer.TYPE
    case "boolean" => java.lang.Boolean.TYPE
    case "scala.Boolean" => java.lang.Boolean.TYPE
    case "double" => java.lang.Double.TYPE
    case "scala.Double" => java.lang.Double.TYPE
    case _ => java.lang.Class.forName(name)
  }
  
  def extractClass(tpe:Type):String = tpe match {
    case PrefixedType(_,Class(name)) => name
    case AppliedType(tp2,_) => extractClass(tp2)
    case PrefixedType(_,TypeField(_,tp2:PrefixedType)) => extractClass(tp2)
    case NamedType(name) => name
  }
  
  def methodFromCode[T1,T2,U](code:Code[(T1,T2)=>U]) = try { 
    code.tree match{
      case Function(List(p1,p2),Apply(Select(Ident(th),Method(method,MethodType(List(param),_))),List(Ident(x)))) if th == p1 && x == p2 =>{
        val paramClass = extractClass(param)
      
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
  
  def methodFromTree(tree:Tree):jMethod = try {
        def classNotFound(clazz:String) = new java.lang.Error("clazz missing: " +clazz+" in " + tree.toString) 
	    tree match{
	      // method call if receiver is too generic, i.e. only a bounded type parameter in the enclosing scope
	      // like [T,It<:Iterable[T]] (it:It) => it.iterator
	      case Function(
	        List(LocalValue(NoSymbol,x,PrefixedType(NoType,NoSymbol))),
                 Apply(Select(Ident(LocalValue(NoSymbol,x1,PrefixedType(NoType,NoSymbol))),
                         Method(method,_)),List())) if x == x1 => {
            val (clazz,methodName) = splitFullMethodName(method)
            val cl = forName(clazz).getOrElse(throw classNotFound(clazz))
            cl.getMethod(methodName)
          }
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
          case Function(List(x),Apply(Select(qual,Method(method,MethodType(List(PrefixedType(_,Class(argClazz))),_))),List(Ident(x1)))) if x==x1 => {
            val clazz = extractClass(typeOfQualifier(qual))
            val cl = forName(clazz).getOrElse(throw classNotFound(clazz))
            val methodName = method.substring(method.lastIndexOf(".")+1)
            val argCl = cleanClass(argClazz)
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
  
  import java.lang.reflect.Modifier
  def fieldFromTree(tree:Tree):java.lang.reflect.Field = {
    def getField(clazz:String,field:String) = {
    	val cl = forName(clazz).getOrElse(throw new RuntimeException("Clazz not found "+clazz+" in "+tree))
		val fieldName = field.substring(field.lastIndexOf(".")+1)
		val f = cl.getField(fieldName)
		assert ((f.getModifiers & Modifier.STATIC) == Modifier.STATIC)
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