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