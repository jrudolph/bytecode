package net.virtualvoid.bytecode

import _root_.org.specs._

object ReflectSpecs extends Specification {
	import scala.reflect.Code.lift
 
	import CodeTools._
 
	"Fields should be correctly infered from scala.reflect.Code" in {
		"Qualified read-accesses" in {
			val field = fieldFromTree(lift(() => StaticVariableContainer.x).tree)
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
		"imported read-accesses" in {
			import StaticVariableContainer.x
			val field = fieldFromTree(lift(() => x).tree)
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
		"Write-accesses" in {
			val field = fieldFromTree(lift(StaticVariableContainer.x = (_:Int)).tree)
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
	}
	"Methods should be correctly infered from scala.reflect.Code tree" in {
	  "static methods of relatively qualified classes" in {
	    val m = methodFromTree(lift(java.lang.Double.valueOf(_:Double)).tree)
	    m.getName must be_==("valueOf")
	    m.getParameterTypes.length must be_==(1)
	  }
	}
        "Constructors should be correctly inferred from scala.reflect.Code tree" in {
          "unary constructors" in {
	    val c = constructorFromTree(lift(new java.lang.StringBuilder(_: String)).tree)
	    c.getName must be_==("java.lang.StringBuilder")
	    c.getParameterTypes.length must be_==(1)
            c.newInstance("bla").asInstanceOf[java.lang.StringBuilder].toString must be_==("bla")
	  }
          "binary constructors" in {
	    val c = constructorFromTree(lift(new java.lang.String(_: Array[Byte], _: java.nio.charset.Charset)).tree)
	    c.getName must be_==("java.lang.String")
	    c.getParameterTypes.length must be_==(2)
            c.newInstance("bla".getBytes, java.nio.charset.Charset.defaultCharset) must be_==("bla")
	  }          
        }
}
