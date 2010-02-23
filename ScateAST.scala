package com.github.lp

abstract class Node(val str:String){
  override def toString():String = str
}

class TokenNode(val token:Token) extends Node(token.str){

}

class StringNode(str:String) extends Node(str){
}

class NodeList(val lst:List[Node]) extends Node((("{" /: lst) (_ + _)) + "}"){
}

