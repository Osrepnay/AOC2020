package com.adventofcode

import scala.io.Source

object Puz3{

	def main(args: Array[String]): Unit = {
		val lines=(Source fromURL getClass.getResource("/input3.txt")).
		 	mkString("").split("\n").map(_.split("").toList).toList
		println(slopeTrees(lines, 1, 1)*slopeTrees(lines, 1, 3)*slopeTrees(lines, 1, 5)*slopeTrees(lines, 1, 7)*slopeTrees(lines, 2, 1))
	}

	def slopeTrees(lines: List[List[String]], down: Int, right: Int): Long = {
		(for{
			row <- lines.indices by down
			column=getOffset((row.toDouble*(right.toDouble/down.toDouble)).toInt, lines(row).length)
			if lines(row)(column)=="#"
		}yield "X").length.toLong
	}

	def getOffset(row: Int, width: Int): Int = {
		if(row<width){
			row
		}else{
			row-(width*(row/width))
		}
	}

}
