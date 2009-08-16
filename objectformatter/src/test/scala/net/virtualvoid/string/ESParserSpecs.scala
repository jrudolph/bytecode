package net.virtualvoid.string

import _root_.org.specs._

object ParserSpecs extends Specification {
  import EnhancedStringFormatParser.parse
  import AST._

  "The parser" should {
    "'test'" in {"test" must beParsedAs(Literal("test"))}
    "'#prop'" in {"#prop" must beParsedAs(Exp("prop"))}
    "'#{prop}'" in {"#{prop}" must beParsedAs(Exp("prop"))}
    "'#{prop.member}'" in {"#{prop.member}" must beParsedAs(ParentExp(Exp("member"),"prop"))}
    "'#prop.member'" in {"#prop.member" must beParsedAs(ParentExp(Exp("member"),"prop"))}
    "'#listProp*'" in {"#listProp*" must beParsedAs(expand(Exp("listProp"),"",ToStringConversion(ThisExp)))}
    "'#listProp{,}*'" in {"#listProp{,}*" must beParsedAs(expand(Exp("listProp"),",",ToStringConversion(ThisExp)))}
    "'#{listProp}{,}*'" in {"#{listProp}{,}*" must beParsedAs(expand(Exp("listProp"),",",ToStringConversion(ThisExp)))}
    "'#listProp[test]{,}*'" in {"#listProp[test]{,}*" must beParsedAs(expand(Exp("listProp"),",",Literal("test")))}

    "#this" in {"#this" must beParsedAs(ThisExp)}
    "#{this}" in {"#{this}" must beParsedAs(ThisExp)}
    "#this[]*" in {"#this[]*" must beParsedAs(expand(ThisExp,""))}

    //escaped square brackets
    "#[abc#]" in {"#[abc#]" must beParsedAs(Literal("[abc]"))}

    //escaped hash
    "##abc" in {"##abc" must beParsedAs(Literal("#abc"))}

    // more complex escape situations
    "###] ####blub ###[" in {"###] ####blub ###[" must beParsedAs(Literal("#] ##blub #["))}

    // test weird control combinations
    "Dots in normal literals 'This is a sentence.'" in {"This is a sentence." must beParsedAs(Literal("This is a sentence."))}
    "Dots after curly braced expressions 'This is a #{exp}.'" in {"This is a #{exp}." must beParsedAs(Literal("This is a "),ToStringConversion(Exp("exp")),Literal("."))}

    "Curly Braces somewhere in between 'This is {braced}'" in {"This is {braced}" must beParsedAs(Literal("This is {braced}"))}
    "question mark after expression" in {"Who lives at #address?" must beParsedAs(Literal("Who lives at "),ToStringConversion(Exp("address")),Literal("?"))}
    
    // conversions
    "date conversion" in {"#this->date[dd.MM.yyyy]" must beParsedAs(DateConversion(ThisExp,"dd.MM.yyyy"))}
    
    // conditionals
    "conditionals" in {"#this?[#this|Nope]" must beParsedAs(Conditional(ThisExp,toks(ToStringConversion(ThisExp)),toks(Literal("Nope"))))}
    "complex conditionals" in {"#{x.getClass.getMethods}?[#it|None]" must beParsedAs(Conditional(
      ParentExp(ParentExp(Exp("getMethods"),"getClass"),"x")
      ,toks(ToStringConversion(Exp("it"))),toks(Literal("None"))))}
    "conditional and expansion" in {"#x[#it?[#it|Leer]]{,}*" must beParsedAs(Expand(Exp("x"),",",toks(Conditional(Exp("it"),toks(ToStringConversion(Exp("it"))),toks(Literal("Leer"))))))}
  }

  // helper methods

  import org.specs.matcher.Matcher
  def beParsedAs(ts:FormatElement*) = new Matcher[String]{
    val tokens = toks(ts:_*)
    def apply(str: => String) = {
      val l = parse(str)
      (l.toString == tokens.toString,"equal",l.toString + " is not equal to the expected " + tokens.toString)
    }
  }
  def beParsedAs(ts:Exp):Matcher[String] = beParsedAs(ToStringConversion(ts))
  
  def expand(e:Exp,sep:String,inner:FormatElement*) = Expand(e,sep,toks(inner:_*))
  def toks(inner:FormatElement*) = FormatElementList(List(inner:_*))

  /* Not working with specs 1.4.3 anymore
  import org.specs.specification.Example
  def parseCorrectly(e: =>Example) = { currentSut.verb += " parse correctly"; e }
  */
}

class ParserSpecsTest extends runner.JUnit4(ParserSpecs)