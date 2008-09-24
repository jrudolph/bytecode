package net.virtualvoid.bytecode.v2

object ParallelListOperation {
  import scala.actors.Actor._
  
  def parallelForEach[T](it:Iterable[T],numThreads:Int,func:T => Unit):Unit = {
    val server = self
    for(i<-1 to numThreads)
      actor{
        loop{
	        (server !? "next") match{
	          case Some(t:T) => func(t)
              case None => {server!"exit";exit}
	        }
         }
      }
    val elements = it.elements
    var received = 0
    while(received<numThreads){
      self.receive{
        case "next" => reply{if (elements.hasNext) Some(elements.next) else None}
        case "exit" => received += 1
      }
    }
  }
}

object ParallelTest{
  def main(args:Array[String]):Unit = {
    //ParallelListOperation.parallelForEach(0 to 100000,5,{(x:Int) => System.out.print(currentThread+":");System.out.println(x)})
    val str = "Das ist ein mehrzeiliger Text\nMit genau zwei Zeilen"
    val map = new LineMap(str.toCharArray)
    System.out.println(map.lineToOffset(0))
    System.out.println(map.lineToOffset(1))
    System.out.println(map.offsetToLine(0))
    System.out.println(map.offsetToLine(29)+":"+str(29))
    System.out.println(map.offsetToLine(30)+":"+str(30))
    System.out.println(map.offsetToLine(35)+":"+str(35))
  }
}

class LineMap(content:Array[Char]){
  def mapWithIndex[T,U](x:RandomAccessSeq[T],f:(Int,T)=>U):Array[U] ={
    val res = new Array[U](x.length)
    for (index<-0 to x.length-1)
      res(index) = f(index,x(index))
    res
  }
  
  def findLineEndings():Array[Int] = 
    mapWithIndex(content,(i,c:Char) => if (c==0xa) List(i+1) else List())
      .foldLeft(List(0))(_ ++ _).toArray
  
  val lineOffsets:Array[Int] = findLineEndings
  
  def lineToOffset(line:Int):Int = lineOffsets(line)
  def offsetToLine(offset:Int):Int = 
    findResultToOption(lineOffsets.findIndexOf(_ > offset)).getOrElse(lineOffsets.length)-1
  
  def findResultToOption(i:Int):Option[Int] = if (i<0) None else Some(i)
}

object FastDataWriter{
  def withDOS[T](f:java.io.File,func:java.io.DataOutputStream => T):T = {
    import java.nio._
    import java.io._
    
    val buffer:ByteBuffer = ByteBuffer.allocate(100000)
    
    val dos = new java.io.DataOutputStream(null){
      override def writeShort(i:Int){
        buffer.putShort((i & 0xffff).asInstanceOf[Short])
      }
    }
    val res = func(dos)
    res
  }
}

class Memoized[T,U](generator:T=>U) extends (T => U) {
  val map = new scala.collection.mutable.HashMap[T,U]
  def apply(t:T):U = 
    map.get(t) match{
      case Some(u) => u
      case None => {
        val u = generator(t)
        map(t) = u
        u
      }
    }
}