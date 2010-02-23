package com.github.lp

import scala.util.parsing.combinator._
import java.io.FileReader
import scala.collection.immutable.HashSet
import scala.collection.immutable.HashMap

class ContextInfo(val keywords:Set[String],
		  val wordColorMap:Map[String,String]) {

  def print(){
    println("Keywords are ")
    for(w <- keywords){
      println(w)
    }
    println("color coding")
    for(w <- wordColorMap.keys){
      println(w + " => " + wordColorMap.get(w).get)
    }
  }
}

class CtxLexer extends JavaTokenParsers{
  def contextInfoParser:Parser[ContextInfo] = keyWordSet~wordColorMap ^^ 
                                             { case st~mp => new ContextInfo(st,mp) }

  def keyWordSet:Parser[Set[String]] = "Keywords"~"are"~repsep(word,",")~"." ^^
                                { case _~_~lst~_ => HashSet.empty ++ lst }

  def wordColorMap:Parser[Map[String,String]] = "colors"~"{"~rep(typeColor)~"}" ^^ 
                                                { case _~_~lst~_ => HashMap.empty ++ lst }

  def typeColor:Parser[(String,String)] = word~"=>"~word ^^
                                          { case w1~_~w2 => (w1,w2) }

  def word : Parser[String] = """\S+""".r
}


object CtxTest extends CtxLexer {
  def main(args: Array[String]) {
    getContextInfo(args(0)).print()
  }

  def getContextInfo(fileNaam:String):ContextInfo = {
    val parsed = parseAll(contextInfoParser,new FileReader(fileNaam));
    parsed match {
      case x:NoSuccess   => throw new Exception(x.toString)
      case p => p.get
    }
  }
}
