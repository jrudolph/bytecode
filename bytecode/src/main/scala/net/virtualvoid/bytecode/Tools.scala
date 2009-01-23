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
  
  def methodFromCode[T1,T2,U](code:Code[(T1,T2)=>U]) = code.tree match{
    case Function(List(p1,p2@LocalValue(_,_,tpe:PrefixedType)),Apply(Select(Ident(th),Method(method,_)),List(Ident(x)))) if th == p1 && x == p2 =>{
      val paramClass = extractClass(tpe)
      
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
  def methodFromTree(tree:Tree):jMethod = try {
	    tree match{
	      // method call if receiver is too generic, i.e. only a bounded type parameter in the enclosing scope
	      // like [T,It<:Iterable[T]] (it:It) => it.iterator
	      case Function(
	        List(LocalValue(NoSymbol,x,PrefixedType(NoType,NoSymbol))),
                 Apply(Select(Ident(LocalValue(NoSymbol,x1,PrefixedType(NoType,NoSymbol))),
                         Method(method,_)),List())) if x == x1 => {
	        val index = method.lastIndexOf(".")
            val clazz = method.substring(0,index)
            val methodName = method.substring(index+1)
            val cl = forName(clazz).getOrElse(throw new java.lang.Error("clazz not found: "+clazz+" in "+tree.toString))
            cl.getMethod(methodName)
          }
          // scala method call to method defined without parameter list
          case Function(List(x@LocalValue(_,_,tpe)),Select(Ident(x1),Method(method,_))) if x==x1 => {
            val clazz = extractClass(tpe)
            val cl = forName(clazz).getOrElse(throw new java.lang.Error(tree.toString))//java.lang.Class.forName(clazz)
            val methodName = method.substring(clazz.length+1)
            val m = cl.getMethod(methodName)
            m
          }
          // method call with variable receiver like '_.toString'
          case Function(List(x@LocalValue(_,_,tpe)),Apply(Select(Ident(x1),Method(method,_)),List())) if x==x1 => {
            val clazz = extractClass(tpe)
            val cl = forName(clazz).getOrElse(throw new java.lang.Error(tree.toString))//java.lang.Class.forName(clazz)
            val methodName = method.substring(clazz.length+1)
            val m = cl.getMethod(methodName)
            m
          }
	      // static method call with variable first parameter 'Integer.valueOf(_)'
          case Function(List(x),Apply(Select(qual,Method(method,MethodType(List(PrefixedType(_,Class(argClazz))),_))),List(Ident(x1)))) if x==x1 => {
            val clazz = extractClass(typeOfQualifier(qual))
            val cl = forName(clazz).getOrElse(throw new java.lang.Error("clazz missing: " +clazz+" in " + tree.toString))//java.lang.Class.forName(clazz)
            val methodName = method.substring(clazz.length+1)
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