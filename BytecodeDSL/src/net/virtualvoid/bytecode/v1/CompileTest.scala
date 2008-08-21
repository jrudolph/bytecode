package net.virtualvoid.bytecode.v1

object ShouldNotCompile{
  val ops:Ops=null
  val f:F[Empty,NoLocal]=null
  import ops._
  def popFromEmptyStack() {
    f.op(pop)
  }
  def implicitPopFromEmptyStack() {
    f.pop
  }
  def addWhenOnlyOneInteger {
    f.op(bipush(5))
     .op(iadd)
  }
  def implicitIAddWhenOnlyOneInteger {
    f.op(bipush(5))
     .iadd
  }
}