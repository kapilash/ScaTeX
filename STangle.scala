package com.github.lp

import scala.io
import java.io.File
import java.io.FileWriter
import scala.collection.immutable.HashMap

class TangleEnv(val runMode:RunMode.Value){
  var fileContentsMap:Map[String,List[String]] = new HashMap[String,List[String]]()
  var currentBlock:List[String] = List()
  var currentFile = "STangle.out"

  def addToCurrentBlock(str:String) {
    currentBlock = str :: currentBlock
  }

  def appendBlockToCurrentFile(){
    appendBlockToFile(currentFile)
  }

  def appendBlockToFile(fileName:String){
    currentFile = fileName
    fileContentsMap.get(currentFile) match {
      case None => {
	fileContentsMap = fileContentsMap + ((currentFile,currentBlock.reverse))
      }
      case Some(x) => {
	fileContentsMap = fileContentsMap.update(currentFile,x ::: currentBlock.reverse)
      }
    }
    currentBlock = List()
  }

}

object TangleEnv{
  def apply(mode:String):TangleEnv = if (mode.equalsIgnoreCase("final")){
    new TangleEnv(RunMode.FinalMode) 
  }else if(mode.equalsIgnoreCase("haskell")) {
    new TangleEnv(RunMode.HaskellMode) 
  }else{
    new TangleEnv(RunMode.ScalaMode) 
  }
}

object STangle{

  def runTokens(t:List[Token],env:TangleEnv) {
    t match {
      case List() => return
      case allTokens => {
	val (_,rest1) = Utils.splitTillToken(TokenType.CodeBegin,allTokens,List())
	val (code,rest) = Utils.splitTillToken(TokenType.CodeEnd,rest1,List())
	doCodeSection(code,env)
	val checklst:List[Token] = Utils.dropEmptySpaces(rest)
        checklst match {
	  case (isFileToken :: obrace :: fileName :: cbrace :: others) => {
           if( (isFileToken.tokenType == TokenType.SpecialWord) &&
               (obrace.tokenType == TokenType.CBraceOpen) &&
               (cbrace.tokenType == TokenType.CBraceClose) &&
               (fileName.tokenType == TokenType.StringLiteral) &&
               (isFileToken.str.equalsIgnoreCase("\\file"))) {
		 env.appendBlockToFile(fileName.str.substring(1,fileName.str.length - 1))
		 runTokens(others,env)
	       }else{
		 env.appendBlockToCurrentFile()
		 runTokens(rest,env)
		 
	       }
               
               
	  }
	  case _ => runTokens(rest,env)
	}
      }
    }
  }
 
  def doCodeSection(codeTokens:List[Token],env:TangleEnv){
    codeTokens match {
      case List() => return
      case x::tokens => x.tokenType match {
	case TokenType.NewLine =>{
	  env.addToCurrentBlock( Utils.handleNewLine(x,env.runMode))
	  doCodeSection(tokens,env)
	}
	case _ =>  {
	  env.addToCurrentBlock(x.str)
	  doCodeSection(tokens,env)
	}
      }
    }
  }


  def usage(){
    println("Usage:")
    println("scala com.github.lp.STangle <fileName>")
  }


 def main(args: Array[String]) {
   var isValid:Boolean = true
   val tuple:(String,String) = args.length match {
       case 1 => ("scala",args(0))
       case 2 => (args(0),args(1))
       case _ => {
	 isValid = false
	 ("","")
       }
     }
    if(!isValid){
      usage()
      return
    }


   val codeFile = tuple._2
   val mode = tuple._1
   
   val allTokens:List[Token] = Tokenizer.getTokens(codeFile)

   val environment = TangleEnv(mode)
   runTokens(allTokens,environment)
   for((file,contents) <- environment.fileContentsMap){
     Utils.writeToFile(file,("" /: contents) (_ + _))
   }

 }

}
