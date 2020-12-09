package com.adventofcode

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz9{

	def main(args: Array[String]): Unit = {
		val nums = Using(Source.fromURL(getClass.getResource("/input9.txt"))){
			source => source.mkString("").split("\n").map(_.toLong).toList
		}
		nums match {
			case Success(nums) =>
				val idxs = nums.indices.map{
					idxS =>
						(idxS, nums.takeRight(nums.length - idxS - 1).indices.
							filter(idxL => nums.slice(idxS, idxL + idxS + 1).sum == 542529149).map(_ + idxS + 1))
				}.filter(_._2.nonEmpty)(0)
				val range = nums.slice(idxs._1, idxs._2(0))
				println(s"${range.min + range.max}")
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

}
