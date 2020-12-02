package com.adventofcode

import scala.io.Source

object Puz2{
	def main(args: Array[String]): Unit = {
		val fileLines=Source.fromURL(getClass.getResource("/input2.txt")).mkString("").split("\n").toList
		val fileLinesSplit=fileLines.map(line => line.split("\\-|: | ").toList)
		val numRight=fileLinesSplit.count(
			line => line.last.charAt(line(0).toInt-1).toString==line(2) ^
				line.last.charAt(line(1).toInt-1).toString==line(2)
		)
		println(numRight)
	}
}
