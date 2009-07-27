package net.virtualvoid.bytecode

import _root_.org.specs._

object ReflectSpecs extends Specification {
	def treeOf(code:scala.reflect.Code[_]) = code.tree
 
	import CodeTools._
 
	"Fields should be correctly infered from scala.reflect.Code" in {
		"Qualified read-accesses" in {
			val field = fieldFromTree(treeOf(() => StaticVariableContainer.x))
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
		"imported read-accesses" in {
			import StaticVariableContainer.x
			val field = fieldFromTree(treeOf(() => x))
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
		"Write-accesses" in {
			val field = fieldFromTree(treeOf(StaticVariableContainer.x = (_:Int)))
			field.getName must be_==("x")
			field.getDeclaringClass.getSimpleName must be_==("StaticVariableContainer")
		}
	}
}