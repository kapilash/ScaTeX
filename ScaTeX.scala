package com.github.lp

import scala.io
import java.io.File
import java.io.FileWriter
import scala.collection.immutable.HashMap
import scala.collection.immutable.HashSet




class SKEnvironment(val ctx:ContextInfo, val mode:RunMode.Value){
  var revNodes:List[Node] = List()
  def addToken(token:Token){
    revNodes = new TokenNode(token) :: revNodes
  }


  def genDecorate(str:String,mapKey:String,defalt:String):String = {
    val color = ctx.wordColorMap.getOrElse(mapKey,defalt)
    "\\color["+ color + "]{" + str + "}"
  }


  def decorateVariable(str:String):String = if (ctx.keywords.contains(str)){
    genDecorate(str,"Keyword","darkred")
  }else  ctx.wordColorMap.get(str) match {
       case None =>    if (str.charAt(0) <= 'Z'){
			 genDecorate(str,"TypeVariable","darkgreen")
		       } else
                       str
	case Some(clr) => "\\color["+ clr + "]{" + str + "}"

  }
  

  def decorateStringLiteral(str:String):String = genDecorate("\\quotation{" + str + "}","String","magenta")
//genDecorate(str,"String","magenta")

  def decorateCharLiteral(str:String):String = genDecorate(str,"Character","middlemagenta")

  def decorateNumber(str:String):String = genDecorate(str,"Number","darkmagenta")


  def addNode(node:Node){
    revNodes = node :: revNodes
  }
  def addTokens(tokens:List[Token]){
    tokens match {
      case List() => return
      case x::rest => {
	addToken(x)
	addTokens(rest)
      }
    }
  }


  
}


object SKEnvironment{
  def apply(ctxFileName:String,modeStr:String):SKEnvironment = {
    val mode:RunMode.Value = if(modeStr.equalsIgnoreCase("final")){
      RunMode.FinalMode
    }else if(modeStr.equalsIgnoreCase("haskell")){
      RunMode.HaskellMode
    }else{
      RunMode.ScalaMode
    }
    val contextInfo = CtxTest.getContextInfo(ctxFileName)
    new SKEnvironment(contextInfo,mode)
  }
  def apply(ctxFileName:String):SKEnvironment = new SKEnvironment(CtxTest.getContextInfo(ctxFileName),RunMode.ScalaMode)
}

object ScaTeX{
  def runTokens(t:List[Token],env:SKEnvironment) {
      t match {
	case List() => return
	case allTokens => {
	  val (comms,rest1) = Utils.splitTillToken(TokenType.CodeBegin,allTokens,List())
	  env.addTokens(comms)
	  val (code,rest) = Utils.splitTillToken(TokenType.CodeEnd,rest1,List())
	  env.addNode(new StringNode("\\setupbackground[background=screen,corner=round] \\startbackground \\startlines"))
	  runCodeTokens(code,env,1)
	  env.addNode(new StringNode("\\stoplines \\stopbackground"))
	  runTokens(rest,env)
	}
      }
  }
  
/*  def runCodeTokens(cs:List[Token],env:SKEnvironment,l:Int){
    env.add(new StringNode
    cs match {
      case List() => {

	return
      }
      case codeTokens => {
	handleCodeTokens(cs,env)
      }
    }
  }*/

  def runCodeTokens(codeLineTokens:List[Token],env:SKEnvironment, lineNo: Int){
    var currLine = lineNo
    codeLineTokens match {
      case List() => return
      case currToken :: rest => {
	currToken.tokenType match {
	  case TokenType.NewLine =>{ env.addNode(new StringNode(currToken.str + currLine + ".  "))
				    currLine = currLine + 1
				  }
	  case TokenType.SimpleWord => env.addNode(new StringNode("\\type{" + currToken.str + "}"))
	  case TokenType.Variable =>   env.addNode(new StringNode(env.decorateVariable(currToken.str)))
	  case TokenType.CBraceOpen => env.addNode(new StringNode("{\\bf \\{}"))
	  case TokenType.CBraceClose => env.addNode(new StringNode("{\\bf \\}}"))
	  case TokenType.SBraceOpen => env.addNode(new StringNode("{\\bf " +currToken.str + "}"))
	  case TokenType.SBraceClose => env.addNode(new StringNode("{\\bf " +currToken.str + "}"))
          case TokenType.StringLiteral => {
	    val str = currToken.str.substring(1,currToken.str.length - 1)
	    val quoted = "\\type{"+str + "}"
	    val decstr = env.decorateStringLiteral(quoted)
	    env.addNode(new StringNode(decstr))
	  }
	  case TokenType.CharLiteral =>  {
	    val str = currToken.str.substring(1,currToken.str.length - 1)
	    val quoted = "\\quote{"+str + "}"
	    val decstr = env.decorateCharLiteral(quoted)
	    env.addNode(new StringNode(decstr))
	  }
	  case TokenType.FloatConstant =>   {
	   val dollared = "$"+currToken.str + "$"
	    val decstr = env.decorateNumber(dollared)
	    env.addNode(new StringNode(decstr))
	  }
	  case _  => env.addNode(new StringNode("\\type{" + currToken.str + "}"))
	}
	runCodeTokens(rest,env,currLine)
      }
    }
  }

  def usage(){
    println("Usage:")
    println("scala com.github.lp.ScaTeX <fileName> or ")
    println("scala com.github.lp.ScaTeX mode <fileName> or ")
    println("scala com.github.lp.ScaTeX mode <fileName> config-fileName ")
  }


 def main(args: Array[String]) {

   var isValid:Boolean = true

   val triple:Tuple3[String,String,String] = args.length match {
       case 1 => ("scala",args(0),"Scala.sct")
       case 2 => (args(0),args(1),"Scala.sct")
       case 3 => (args(0),args(1),args(2))
       case _ => {
	 isValid = false
	 ("","","")
       }
     }
    if(!isValid){
      usage()
      return
    }
   
   val mode = triple._1
   val infile = triple._2
   val outfile = triple._2.split("[ !.,]+")(0) + ".tex"
   val configFile = triple._3
   
   val allTokens:List[Token] = Tokenizer.getTokens(infile)
   val environment = SKEnvironment(configFile,mode)

   runTokens(allTokens,environment)
   val nodes = environment.revNodes.reverse
   Utils.writeToFile(outfile,("" /: nodes) (_ + _))
   println("Thats it")
  }


  
}

