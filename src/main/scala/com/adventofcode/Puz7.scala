package com.adventofcode

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz7{

	def main(args: Array[String]): Unit = {
		val bags=Using(Source.fromURL(getClass.getResource("/input7.txt"))){
			source => source.mkString.split("\n").map(line => line.take(line.length-1).
				split(" contain |, ").map(_.replaceAll(" bags| bag", "").trim).toList).toList
		}
		bags match{
			case Success(bags) =>
				println{
					count(bags, "shiny gold")
				}
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	def count(bags: List[List[String]], bagCheck: String): Int = {
		val idx=bags.indexOf(bags.filter(_.head==bagCheck).head)
		if(bags(idx).last=="no other") {
			0
		}else{
			val num=bags(idx).takeRight(bags(idx).length-1).map(_.take(1).toInt)
			val inner=bags(idx).takeRight(bags(idx).length-1).
				map(str => count(bags, str.replaceAll("\\d ", "")))
			num.indices.map(i => num(i)+(num(i)*inner(i))).sum
		}
	}

}
