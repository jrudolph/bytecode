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
      def noAddress:Option[String] = None
      def address:Option[String] = Some("Some Street 5")
      def nullAddress:String = null
      def nonNullAddress:String = "Some Other Street 10"
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
    "evaluate conditionals with Some" in {"#address?[Found: #this|no address given]" must evaluateAs("Found: Some Street 5")}
    "evaluate conditionals with None" in {"#noAddress?[Found: #this|no address given]" must evaluateAs("no address given")}
    "evaluate conditionals with non-null condition" in {"#nonNullAddress?[An address: #this|Is null]" must evaluateAs("An address: Some Other Street 10")}
    "evaluate conditionals with null condition" in {"#nullAddress?[An address: #this|Is null]" must evaluateAs("Is null")}
  }

  "The format interpreter" should {
    evaluate(ObjectFormatter)
  }
  "The format compiler" should {
    evaluate(FormatCompiler)
  }
}

object FormatAccountsExample{
  def main(args:Array[String]){
    System.out.println("Hello")
    System.out.println(FormatCompiler.format(
"""#name has these bank accounts:
  #accs[#number at #bank.name having these transactions:
    #transactions[#isWithdrawal?[Withdrawal|Deposit]: #amount Euros at #date->date[dd.MM.yyyy]]{ 
    }*]{
  }*
""",EvaluateSpecs.thePerson))
  }
}

class EvaluateSpecsTest extends runner.JUnit4(EvaluateSpecs)
