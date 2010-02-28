package com.github.lp

import scala.util.parsing.combinator._
import java.io.FileReader


object TokenType extends Enumeration{

  val SimpleWord = Value("Word")
  val SpecialWord = Value("TeXWord")


  val Variable = Value("Variable")

  val CBraceOpen = Value("CurlyBraceOpen")
  val CBraceClose = Value("CurlyBraceClose")

  val BraceOpen = Value("BraceOpen")
  val BraceClose = Value("BraceClose")

  val SBraceOpen = Value("SqBraceOpen")
  val SBraceClose = Value("SqBraceClose")

  val StringLiteral = Value("String")
  val CharLiteral = Value("Character")

  val HexaDecimalConstant = Value("Hexadecimal")
  val OctalConstant = Value("Octal")
  val FloatConstant = Value("Float")
  val DecimalConstant = Value("Decimal")
  
  val NewLine = Value("NewLine")
  val Space = Value("Space")

  val CodeBegin = Value("CodeBegin")
  val CodeEnd = Value("CodeEnd")

  val Dollar = Value("$")
  val Hash = Value("#")
  val And = Value("&")
  val Percent = Value("%")
  val UnderScore = Value("_")

  val Symbol = Value("Symbol")

}

class Token(val str:String,
            val fileName:String,
	    val line:Int, 
	    val column:Int, 
	    val tokenType:TokenType.Value){

  val pstr = tokenType match {
    case TokenType.NewLine => "<NLINE" + str.length + ">"
    case TokenType.Space => "<SPACE" + str.length + ">"
    case _              => str
  }

  override def toString():String = "{Token: " + tokenType + 
                                  ",String:" + pstr +
                                   ",Line: " + line +
                                   ",column: " + column +
                                   ",File: " + fileName + 
                                   "}\n"
}

class LitLexer extends JavaTokenParsers{
  var fileName:String = "UnknownFileName"
  var currLine:Int = 1
  var currColumn:Int = 0

  def incrementLine(){
    currLine = currLine + 1
    currColumn = 0
  }
  def incrementColumn(str:String){
    currColumn = currColumn + str.length
  }

  def tokens : Parser[List[Token]] = rep(value)

  def value : Parser[Token] = codeStart | codeEnd | macro |
                              variable |
                              hexaDecimal | octal |
                              floatingNumber |
			      stringConstant | charLiteral|
                              openBracket |
                              closeBracket |
                              copenBracket |
                              ccloseBracket |
                              sopenBracket |
                              scloseBracket |
                              hashSymbol |
                              dollarSymbol |
                              andSymbol |
                              percentSymbol |
                              underScore |                              
                              newLine |
                              space  |
                              splChar |
                              simpleWord 





  override def skipWhitespace = false
  private def makeToken(x:String,t:TokenType.Value):Token = {
    val tok =  new Token(x,fileName,currLine,currColumn,t)
    incrementColumn(x)
    return tok
  }
  
  private def newLineToken(x:String,t:TokenType.Value):Token = {
    val t = new Token(x,fileName,currLine,currColumn,TokenType.NewLine)
    incrementLine()
    t
  }

  def newLine:Parser[Token] =  ("\n" | "\r\n") ^^ ((x) => newLineToken(x,TokenType.NewLine))


  def stringConstant : Parser[Token] = myStringLiteral ^^ ( (x) => makeToken(x,TokenType.StringLiteral))


  def charLiteral : Parser[Token] = charLiteralL ^^ ( (x) => makeToken(x,TokenType.CharLiteral) )

  def macro : Parser[Token] = myIdentLiteral ^^ ( (x) => makeToken(x,TokenType.SpecialWord))




  def  variable : Parser[Token] = variableL ^^ ( (x) => makeToken(x,TokenType.Variable))

  def simpleWord:Parser[Token] = anyChar ^^ ((x) => makeToken(x,TokenType.SimpleWord))

  def space:Parser[Token] = spaces ^^ ((x) => makeToken(x,TokenType.Space))

  def openBracket:Parser[Token] = openBracketL ^^ ((x) => makeToken(x,TokenType.BraceOpen))
  def closeBracket:Parser[Token] = closeBracketL ^^ ((x) => makeToken(x,TokenType.BraceClose))

  def sopenBracket:Parser[Token] = sopenBracketL ^^ ((x) => makeToken(x,TokenType.SBraceOpen))
  def scloseBracket:Parser[Token] = scloseBracketL ^^ ((x) => makeToken(x,TokenType.SBraceClose))

  def copenBracket:Parser[Token] = copenBracketL ^^ ((x) => makeToken(x,TokenType.CBraceOpen))
  def ccloseBracket:Parser[Token] = ccloseBracketL ^^ ((x) => makeToken(x,TokenType.CBraceClose))

  def hexaDecimal:Parser[Token] = hexNumberLiteral ^^ ((x) => makeToken(x,TokenType.HexaDecimalConstant))
  def octal:Parser[Token] = octalNumberLiteral ^^ ((x) => makeToken(x,TokenType.OctalConstant))
  def floatingNumber:Parser[Token] = floatingPointNumber ^^ ((x) => makeToken(x,TokenType.FloatConstant))


  def hashSymbol:Parser[Token] = "#" ^^ ((x) => makeToken(x,TokenType.Hash))
  def dollarSymbol:Parser[Token] = "$" ^^ ((x) => makeToken(x,TokenType.Dollar))

  def andSymbol:Parser[Token] = "&" ^^ ((x) => makeToken(x,TokenType.And))
  def percentSymbol:Parser[Token] = "%" ^^ ((x) => makeToken(x,TokenType.Percent))
  def underScore : Parser[Token] = "_" ^^ ((x) => makeToken(x,TokenType.UnderScore))

  def codeStart:Parser[Token] = """\\begin\{code\}""".r ^^ ((x) => makeToken(x,TokenType.CodeBegin))
  def codeEnd:Parser[Token] = """\\end\{code\}""".r ^^ ((x) => makeToken(x,TokenType.CodeEnd))
//  def codeStart:Parser[Token] = """\\begin""".r ^^ ((x) => makeToken(x,TokenType.SplBegin))
//  def codeEnd:Parser[Token] = """\\end""".r ^^ ((x) => makeToken(x,TokenType.SplEnd))


  def splChar:Parser[Token] = nonWordChar ^^ ((x) => makeToken(x,TokenType.Symbol))


//  def decimalConstant:Parser[Token] = decimalLiteral ^^ ((x) => makeToken(x,TokenType.DecimalConstant))
//  def zero:Parser[Token] = zeroL ^^ ((x) => makeToken(x,TokenType.DecimalConstant))


  val myIdentLiteral = """\\[a-zA-Z][a-zA-Z0-9]*""".r


  val variableL = """[a-zA-Z][a-zA-Z0-9.\-]*""".r
  val charLiteralL = "'([^'\\\\]|\\\\.)'".r
  val myStringLiteral = "\"([^\"\\\\]|\\\\.)*\"".r  


  val nonWordChar = """\W""".r
  val anyChar = """\S+""".r
  val spaces = "[ \t\f]".r

  val openBracketL = "("
  val closeBracketL = ")"

  val copenBracketL = "{"
  val ccloseBracketL = "}"

  val sopenBracketL = "["
  val scloseBracketL = "]"


  val hexNumberLiteral = """0[xX][0-9A-Fa-f]+""".r
  val octalNumberLiteral = """0[0-7]+""".r
  val floatingPointLiteral= """[0-9]*\.[0-9]*([e|E][0-9]+)?""".r
  val decimalLiteral = """[1-9][0-9]*""".r
  val zeroL = "0"



}


object Tokenizer extends LitLexer {
  def main(args: Array[String]) {
    println(getTokens(args(0)))
  }

  def getTokens(fileNaam:String):List[Token] = {
    val reader = new FileReader(fileNaam)
    fileName = fileNaam
    parseAll(tokens,reader) match {
      case x:NoSuccess   => throw new Exception(x.toString)
      case p => p.get

    }
  }
}

