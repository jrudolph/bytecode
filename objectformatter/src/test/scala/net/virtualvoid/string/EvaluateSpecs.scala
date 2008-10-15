package net.virtualvoid.string

import _root_.org.specs._

object EvaluateSpecs extends Specification{
  import java.lang.{String=>jString}
  case class Bank(n:String){
    def name():jString = n
  }
  import java.util.GregorianCalendar
  import java.util.Calendar._
  case class Transaction(date:GregorianCalendar,centAmount:Int){
    def isWithdrawal() = centAmount < 0
    def amount():java.lang.Integer = Math.abs(centAmount) // no primitives for now
  }
  case class Account(n:String,b:Bank) {
    def number():jString = n
    def bank() = b
    def transactions():Array[Transaction] = Array(Transaction(new GregorianCalendar(2008,OCTOBER,1),5),Transaction(new GregorianCalendar(2008,OCTOBER,3),-4))
  }
  class Person{
      def name():java.lang.String = "Joe"
      def accountNames():java.util.List[java.lang.String] = java.util.Arrays.asList("a","b")
      val sparkasse = Bank("Sparkasse")
      def accounts():java.util.List[Account] = java.util.Arrays.asList(Account("78910",sparkasse),Account("12345",Bank("Volksbank")))
      def accs():Array[Account] = accounts().toArray(new Array[Account](0))
  }
  val thePerson = new Person

  def evaluate(factory:IObjectFormatterFactory){
    import matcher.Matcher
    def evaluateObjectAs(obj:AnyRef,str:String) = new Matcher[String]{
      def apply(format: =>String) = {
        val res = factory.format(format,obj)
        (res == str,"evaluates as "+str,"does not evaluate as "+str+" but as "+ res)
      }
    }
    def evaluateAs(str:String) = evaluateObjectAs(thePerson,str)

    "literal" in {"literal" must evaluateAs("literal")}
    "property access" in {"#name" must evaluateAs(thePerson.name)}
    "string array access" in {"#accountNames*" must evaluateAs("ab")}
    "string array access with separator" in {"#accountNames{,}*" must evaluateAs("a,b")}
    "object iterable access with inner expression" in {"#accounts[#number]{,}*" must evaluateAs("78910,12345")}
    "object array access with inner expression" in {"#accs[#number]{,}*" must evaluateAs("78910,12345")}
    "deep property access" in {"#accs[#bank.name]{,}*" must evaluateAs("Sparkasse,Volksbank")}
    "format dates properly" in {"#this->date[dd.MM.yyyy]" must evaluateObjectAs(new GregorianCalendar(2008,OCTOBER,1),"01.10.2008")}
    "evaluate conditionals true" in {"#this?[yes|no]" must evaluateObjectAs(java.lang.Boolean.valueOf(true),"yes")}
    "evaluate conditionals false" in {"#this?[yes|no]" must evaluateObjectAs(java.lang.Boolean.valueOf(false),"no")}
    "evaluate primitives" in {"#amount" must evaluateObjectAs(Transaction(null,50),"50")}
    
    "complex test" in {
"""#name has these bank accounts:
  #accs[#number at #bank.name having these transactions:
    #transactions[#isWithdrawal?[Withdrawal|Deposit]: #amount Euros at #date->date[dd.MM.yyyy]]{ 
    }*]{
  }*
""" must evaluateAs(
"""Joe has these bank accounts:
  78910 at Sparkasse having these transactions:
    Deposit: 5 Euros at 01.10.2008 
    Withdrawal: 4 Euros at 03.10.2008
  12345 at Volksbank having these transactions:
    Deposit: 5 Euros at 01.10.2008 
    Withdrawal: 4 Euros at 03.10.2008
""")
    }
  }

  "The format interpreter" should {
    evaluate(ObjectFormatter)
  }
  "The format compiler" should {
    evaluate(FormatCompiler)
  }
}

class EvaluateSpecsTest extends runner.JUnit4(EvaluateSpecs)
