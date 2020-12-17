package com.adventofcode

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz17 {

	def main(args: Array[String]): Unit = {
		val grid = Using(Source.fromURL(getClass.getResource("/input17.txt"))) {
			source => (source.mkString.split("\n").map(_.split("").toList).toList :: Nil) :: Nil
		}
		grid match {
			case Success(grid) =>
				println(doRound(doRound(doRound(doRound(doRound(doRound(grid)))))).flatten.flatten.flatten.
					count(_ == "#"))
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	//WARNING: Read these two functions at your own risk. May cause: nausea, vomiting, and sudden death.
	def doRound(grid: List[List[List[List[String]]]]): List[List[List[List[String]]]] = {
		val expandedGrid = (0 to grid.length + 1).map(_ => (0 to grid.head.length + 1).map(_ =>
			(0 to grid.head.head.length + 1).map(_ => (0 to grid.head.head.head.length + 1).map(_ => "."))))
		val neighbors = expandedGrid.indices.map(cubeIdx => expandedGrid(cubeIdx).indices.map(planeIdx =>
			expandedGrid(cubeIdx)(planeIdx).indices.map(rowIdx => expandedGrid(cubeIdx)(planeIdx)(rowIdx).indices.
				map(idx => getNeighbors(grid, (cubeIdx - 1, planeIdx - 1, rowIdx - 1, idx - 1))))))
		neighbors.indices.map(cubeIdx => neighbors(cubeIdx).indices.map(planeIdx => neighbors(cubeIdx)(planeIdx).
			indices.map(rowIdx => neighbors(cubeIdx)(planeIdx)(rowIdx).indices.map(idx => {
			try {
				if(grid(cubeIdx - 1)(planeIdx - 1)(rowIdx - 1)(idx - 1) == "#") {
					if(neighbors(cubeIdx)(planeIdx)(rowIdx)(idx) == 2 ||
						neighbors(cubeIdx)(planeIdx)(rowIdx)(idx) == 3) {
						"#"
					} else {
						"."
					}
				} else {
					if(neighbors(cubeIdx)(planeIdx)(rowIdx)(idx) == 3) {
						"#"
					} else {
						"."
					}
				}
			} catch {
				case e: IndexOutOfBoundsException =>
					if(neighbors(cubeIdx)(planeIdx)(rowIdx)(idx) == 3) {
						"#"
					} else {
						"."
					}
			}
		}).toList).toList).toList).toList
	}

	def getNeighbors(grid: List[List[List[List[String]]]], pos: (Int, Int, Int, Int)): Int = {
		val tryVals = (-1 to 1).map(num => (num, (-1 to 1).map(num2 => (num2, (-1 to 1).map(num3 => (num3, -1 to 1))))))
		tryVals.indices.map(idx => {
			tryVals.head._2.indices.map(idx2 => {
				tryVals.head._2.head._2.indices.map(idx3 => {
					tryVals.head._2.head._2.head._2.indices.map(idx4 => {
						val changeBy = (tryVals(idx)._1, tryVals(idx)._2(idx2)._1, tryVals(idx)._2(idx2)._2(idx3)._1,
							tryVals(idx)._2(idx2)._2(idx3)._2(idx4))
						if(changeBy != (0, 0, 0, 0)) {
							val newPos = (pos._1 + changeBy._1, pos._2 + changeBy._2, pos._3 + changeBy._3,
								pos._4 + changeBy._4)
							if(newPos._1 >= 0 && newPos._1 < grid.length && newPos._2 >= 0 && newPos._2 <
								grid.head.length && newPos._3 >= 0 && newPos._3 < grid.head.head.length &&
								newPos._4 >= 0 && newPos._4 < grid.head.head.head.length) {
								if(grid(newPos._1)(newPos._2)(newPos._3)(newPos._4) == "#") 1 else 0
							} else {
								0
							}
						} else {
							0
						}
					}).sum
				}).sum
			}).sum
		}).sum
	}

}
