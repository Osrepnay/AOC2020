package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz12 {

	def main(args: Array[String]): Unit = {
		val directions = Using(Source.fromURL(getClass.getResource("/input12.txt"))) {
			source => source.mkString.split("\n").map(_.split("", 2).toList).toList
		}
		directions match {
			case Success(directions) =>
				val newPos = doDirections(directions, (10, 1), (0, 0))
				println(math.abs(newPos._1) + math.abs(newPos._2))
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def doDirections(directions: List[List[String]], pos: (Int, Int), shipPos: (Int, Int)): (Int, Int) = {
		if(directions.length <= 0) {
			shipPos
		} else {
			val currDirection = directions.head
			val newDirections = directions.takeRight(directions.length - 1)
			currDirection.head match {
				case "R" =>
					doDirections(newDirections, turn(pos, currDirection(1).toInt / 90, "R"), shipPos)
				case "L" =>
					doDirections(newDirections, turn(pos, currDirection(1).toInt / 90, "L"), shipPos)
				case "N" =>
					doDirections(newDirections, (pos._1, pos._2 + currDirection(1).toInt), shipPos)
				case "S" =>
					doDirections(newDirections, (pos._1, pos._2 - currDirection(1).toInt), shipPos)
				case "E" =>
					doDirections(newDirections, (pos._1 + currDirection(1).toInt, pos._2), shipPos)
				case "W" =>
					doDirections(newDirections, (pos._1 - currDirection(1).toInt, pos._2), shipPos)
				case "F" =>
					doDirections(newDirections, pos, (shipPos._1 + pos._1 * currDirection(1).toInt,
						shipPos._2 + pos._2 * currDirection(1).toInt))
				case "B" =>
					doDirections(newDirections, pos, (shipPos._1 + pos._1 * currDirection(1).toInt,
						shipPos._2 + pos._2 * currDirection(1).toInt))
			}
		}
	}

	@tailrec
	def turn(turnPos: (Int, Int), turnTimes: Int, turnDirection: String): (Int, Int) = {
		if(turnDirection == "R") {
			if(turnTimes <= 1) {
				(turnPos._2, -turnPos._1)
			} else {
				turn((turnPos._2, -turnPos._1), turnTimes - 1, turnDirection)
			}
		} else {
			if(turnTimes <= 1) {
				(-turnPos._2, turnPos._1)
			} else {
				turn((-turnPos._2, turnPos._1), turnTimes - 1, turnDirection)
			}
		}
	}

}
