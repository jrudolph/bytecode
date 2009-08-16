package net.virtualvoid.string

object ObjectFormatter extends IObjectFormatterFactory{
  val parser = EnhancedStringFormatParser

  def formatter[T<:AnyRef](clazz:Class[T],fm:String):T => String = {
    val parsed = parser.parse(fm)
    parsed.format(_)
  }
}
