package com.adventofcode

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz15 {

	def main(args: Array[String]): Unit = {
		val startingInputs = Using(Source.fromURL(getClass.getResource("/input15.txt"))){
			source => source.mkString.split(",").map(_.toLong).toList
		}
		startingInputs match {
			case Success(startingInputs) =>
				println(doRounds(HashMap.empty ++ (startingInputs zip startingInputs.indices.map(List(_))).toMap,
					startingInputs.last, startingInputs.length))
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def doRounds(indices: Map[Long, List[Int]], lastInput: Long, counter: Long): Long = {
		if(counter >= 30000000) {
			lastInput
		} else {
			val idxs = indices(lastInput).sortWith(_ > _)
			val add = if(idxs.length > 1) (idxs.head - idxs(1)).toLong else 0L
			if(indices.contains(add)) {
				doRounds(indices.updated(add, (counter.toInt :: indices(add)).take(2)), add, counter + 1)
			} else {
				doRounds(indices + (add -> List(counter.toInt)), add, counter + 1)
			}
		}
	}

}
