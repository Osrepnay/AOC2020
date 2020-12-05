package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz5{

	def main(args: Array[String]): Unit = {
		val passes=Using(Source fromURL getClass.getResource("/input5.txt")){
			source => source.mkString("").split("\n").map(_.map{
				case 'F' => false
				case 'B' => true
				case 'L' => false
				case 'R' => true
			}.toList).toList
		}
		passes match{
			case Success(passes) =>
				val list=passes.map(pass => (divNum(pass.take(7), (0 until 128).toList)*8)+
					divNum(pass.takeRight(3), (0 until 8).toList)).sorted
				println(for(num <- list(1) until list.last
					if !list.contains(num) && list.contains(num-1) && list.contains(num+1)) yield num)
			case Failure(e)	=>
				println("Unable to read file.")
		}

	}

	@tailrec
	def divNum(divide: List[Boolean], numRange: List[Int]): Int = {
		if(divide.head){
			if(divide.length<=1){
				numRange.last
			}else{
				divNum(divide.takeRight(divide.length-1), numRange.grouped(numRange.length/2).toList.last)
			}
		}else{
			if(divide.length<=1){
				numRange.head
			}else{
				divNum(divide.takeRight(divide.length-1), numRange.grouped(numRange.length/2).toList.head)
			}
		}
	}

}
