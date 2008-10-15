package net.virtualvoid.bytecode

object Problem {
  
  def worden(i:Int):String = i match{
    case 1=>"one"
    case 2=>"two"
    case 3=>"three"
    case 4=>"four"
    case 5=>"five"
    case 6=>"six"
    case 7=>"seven"
    case 8=>"eight"
    case 9=>"nine"
    case 10=>"ten"
    case 11=>"eleven"
    case 12=>"twelve"
    case 13=>"thirteen"
    case 14=>"fourteen"
    case 15=>"fifteen"
    case 16=>"sixteen"
    case 17=>"seventeen"
    case 18=>"eighteen"
    case 19=>"nineteen"
    case 20=>"twenty"
    case 30=>"thirty"
    case 40=>"forty"
    case 50=>"fifty"
    case 60=>"sixty"
    case 70=>"seventy"
    case 80=>"eighty"
    case 90=>"ninety"
    case _=>
      if (i<100)
        worden((i/10)*10) + worden(i%10)
      else if (i>=100)
        worden(i/100) + "hundred" +(if (i%100!=0) "and"+worden(i%100) else "")
      else
          throw new Error("not handled "+i)
  }
  
  def divs(i:Int):Seq[Int] =
    (1 to Math.sqrt(i).asInstanceOf[Int]).filter(i%_==0).flatMap(x=> List(x,i/x))
  
  def isSquare(i:Int) = {
    var root = Math.sqrt(i)
    root*root == i
  }
  
  def sigma2(i:Int) = divs(i).map(x=>x*x).foldLeft(0)(_+_)
  
  def fastsig(i:Int) = {
    val ar = new Array[Int](i)
    for (x <- (1 until i);
         sq = x*x;
         y <- (x until i by x))
        ar(y) += sq
    ar
  }
  
  def main(args:Array[String]){
    
  }
}
