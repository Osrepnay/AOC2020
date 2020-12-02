package com.adventofcode

import scala.io.Source

object Puz1{
	def main(args: Array[String]): Unit = {
		val lines=Source.fromURL(getClass.getResource("/input1.txt")).mkString("").split( "\n").map(_.toDouble).toSet
		for(num <- lines) {
			for(num2 <- lines
				if lines contains 2020-num-num2){
				println((2020-num-num2)*num*num2)
				return ()
			}
		}
	}
}
