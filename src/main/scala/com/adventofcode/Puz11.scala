package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz11 {

	def main(args: Array[String]): Unit = {
		val seats = Using(Source.fromURL(getClass.getResource("/input11.txt"))) {
			source => source.mkString.split("\n").map(_.split("").toList).toList
		}
		seats match {
			case Success(seats) =>
				println(doRounds(seats).map(_.count(_ == "#")).sum)
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def doRounds(seats: List[List[String]]): List[List[String]] = {
		val newSeats = seats.indices.
			map(row => seats(row).indices.map(column => changeTo(seats, row, column)).toList).toList
		if(newSeats == seats) {
			seats
		} else {
			doRounds(newSeats)
		}
	}

	def changeTo(seats: List[List[String]], row: Int, column: Int): String = {
		val seat = seats(row)(column)
		if(seat == "L") {
			if(seatSurroundedNum(seats, row, column, "#") == 0) {
				"#"
			} else {
				"L"
			}
		} else if(seat == "#") {
			if(seatSurroundedNum(seats, row, column, "#") >= 5) {
				"L"
			} else {
				"#"
			}
		} else {
			seat
		}
	}

	def seatSurroundedNum(seats: List[List[String]], row: Int, column: Int, surroundWith: String): Int = {
		val offsets = List((1, 0), (0, 1), (1, 1), (-1, 0), (0, -1), (-1, -1), (-1, 1), (1, -1))
		offsets.map(offset => if(applyOffset(seats, row, column, offset, surroundWith).isEmpty) 0 else 1).sum
	}

	@tailrec
	def applyOffset(seats: List[List[String]], row: Int, column: Int, offset: (Int, Int), find: String): Option[(Int, Int)] = {
		val newRow = row + offset._1
		val newCol = column + offset._2
		if(newRow >= 0 && newRow < seats.length && newCol >= 0 && newCol < seats.head.length) {
			if(seats(newRow)(newCol) == find) {
				Option((newRow, newCol))
			} else if(seats(newRow)(newCol) == reverse(find)) {
				None
			} else {
				applyOffset(seats, newRow, newCol, offset, find)
			}
		} else {
			None
		}
	}

	def reverse(seat: String): String = {
		if(seat == "L") "#" else if(seat == "#") "L" else "."
	}

}
