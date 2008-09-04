object Test{
	sealed class Nu
	sealed class Nu0 extends Nu
    sealed case class N[C<:Nu](inner:C) extends Nu

    //implicit def tuplizer[IT<:Nu](x:N[IT]):(IT,N[IT]) = null
	implicit def down[IT<:Nu,X[_]<:N[IT]](x:X[IT]):IT = null.asInstanceOf[IT]

    def stufe1 = {
		val x:N[Nu0] = null.asInstanceOf[N[Nu0]]
		//val x1:N[Nu] = x
		//val y:Nu0 = down[Nu0,N[Nu0]](x)
		//val y1:Nu = down((x.inner,x))
        val y2:Nu0 = down(x)
        val z2:Nu0 = x
		val z3:N[N[Nu0]] = x

        null
	 }
    /*def stufe2 = {
      val x:N[N[Nu0]] = null
      val y:N[Nu0] = down(x)
      val z:Nu0 = down(down(x))

      val y1:N[Nu0] = x
      val z1:Nu0 = y
      val z2:Nu0 = x

      null
    }*/

}
