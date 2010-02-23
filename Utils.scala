package com.github.lp

import scala.io
import java.io.File
import java.io.FileWriter
import scala.collection.immutable.HashMap


object RunMode extends Enumeration{
  val FinalMode = Value
  val ScalaMode = Value
  val HaskellMode = Value
}


object Utils{


  def splitTillToken(tokentype:TokenType.Value, allTokens : List[Token],left:List[Token]) : (List[Token],List[Token]) = allTokens match {
    case List() => (left.reverse,List())
    case (x::rest) => if (x.tokenType == tokentype) 
                            (left.reverse,rest)
                      else splitTillToken(tokentype,rest,x::left)
  }



  def dropOnlySpaces(ctoks:List[Token]):List[Token] =  ctoks.dropWhile((x) => x.tokenType == TokenType.Space)

  def dropEmptySpaces(ctoks:List[Token]):List[Token] = ctoks match {
    case List() => ctoks
    case x :: y => x.tokenType match {
      case TokenType.Space => dropEmptySpaces(y)
      case TokenType.NewLine => dropEmptySpaces(y)
      case _                => (x :: y)
    }
  }

  def handleNewLine(token:Token,mode:RunMode.Value):String = mode match {
    case RunMode.FinalMode => token.str
    case RunMode.HaskellMode => "  {- file:" + token.fileName + ",Line: "+ token.line + " -}" + token.str
    case _ => "  /* file:" + token.fileName + ", Line: "+ token.line + " */" + token.str
  }

    def getDirectoryName(fileName:String):(Option[String],String) = {
    val words = fileName.split("/")
    if(words.length == 1){
      return (None,fileName)
    }
    var initDir = words(0)
    for(i <- 1 to (words.length - 2)){
      initDir = initDir + "/" + words(i)
      println(initDir)
      println(i)
    }
      println(initDir)
    return (Some(initDir),words(words.length - 1))
   
  }
  def writeToFile(fileName:String,contents:String){
    val fileObj:File =  getDirectoryName(fileName) match {
      case (None,_) => new File(fileName)
      case (Some(x),f) => {
	(new File(x)).mkdirs()
	new File(x + "/" + f)
      }
    }
    val fileWriter:FileWriter = new FileWriter(fileObj)
    fileWriter.write(contents,0,contents.length)
    fileWriter.close()
  }

}
