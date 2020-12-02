package com.adventofcode

import scala.io.Source

object Puz2{
	def main(args: Array[String]): Unit = {
		val fileLines=Source.fromFile("input.txt").mkString("").split("\n").toList
		val fileLinesSplit=fileLines.map(line => line.split(" ").toList)
		val rightPasswords=
			for(line <- fileLinesSplit;
				range=line(0).split("-").toList.map(_.toInt-1);
				letter=line(1).head;
				pass=line(2);
				if pass.charAt(range(0))==letter ^ pass.charAt(range(1))==letter)
				yield line
		println(rightPasswords.length)
	}
}
