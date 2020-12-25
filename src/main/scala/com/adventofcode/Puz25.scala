package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz25 {

	def main(args: Array[String]): Unit = {
		val pubKeys = Using(Source.fromURL(getClass.getResource("/input25.txt"))) {
			_.mkString.split("\n").map(_.toInt).toList
		}
		pubKeys match {
			case Success(pubKeys) =>
				val loopSize = calcLoopSize(7, pubKeys.head)
				println(transform(pubKeys.last, loopSize))
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def calcLoopSize(subjectNum: Long, tryFor: Long, startLoopSize: Long = 1, counter: Int = 0): Int = {
		if(startLoopSize == tryFor) {
			counter
		} else {
			calcLoopSize(subjectNum, tryFor, (startLoopSize * subjectNum) % 20201227, counter + 1)
		}
	}

	@tailrec
	def transform(subjectNum: Long, loopSize: Int, startWith: Long = 1): Long = {
		if(loopSize == 0) {
			startWith
		} else {
			transform(subjectNum, loopSize - 1, (startWith * subjectNum) % 20201227)
		}
	}

}
