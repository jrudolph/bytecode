package net.virtualvoid.string

object PerformanceTest {
  val count = 10000
  
  def benchmark[T<:AnyRef](formatter:IObjectFormatter[T],o:T) = {
    val start = System.currentTimeMillis 
    for(i<- 1 to count)
      formatter.format(o)
    System.currentTimeMillis - start
  }
  
  class Person{
    def name = "Tester"
  }
  
  def tester[T<:AnyRef](format:String,o:T,clazz:Class[T])(name:String,fac:IObjectFormatterFactory){
    val formatter = fac.formatter(clazz,format)
    for (i<-1 to 10){
      val time = benchmark(formatter,o)
      System.out.println(name+" needed for "+format+" "+time+" ms")
    }
  }
  
  val tests:Seq[(String,AnyRef,Class[_])] = Array(
    ("#name",new Person,classOf[Person])
    ,("test",null,classOf[String])
    ,("#this","Some String",classOf[String])
    ,("#this[#this]{,}*","Some String With blanks".split(" "),classOf[Array[String]])
  )
  
  def main(args:Array[String]){
    for(t<-tests){
      System.out.println(t._1)
      
      val test = tester(t._1,t._2,t._3.asInstanceOf[Class[AnyRef]]) _
      test("interpreter",ObjectFormatter)
      test("compiler",FormatCompiler)
      test("interpreter",ObjectFormatter)
      test("compiler",FormatCompiler)
      test("interpreter",ObjectFormatter)
      
      System.out.println("")
    }
  }
}
