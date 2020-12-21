package com.adventofcode

import scala.io.Source
import scala.util.{Failure, Success, Using}

//TODO Cleanup much
object Puz20 {

	def main(args: Array[String]): Unit = {
		val tiles = Using(Source.fromURL(getClass.getResource("/input20.txt"))) {
			_.mkString.split("\n\n").map(_.split("\n").toList).map(group =>
				(group.tail, group.head.split("Tile |:")(1).toInt)).toList
		}
		tiles match {
			case Success(tiles) =>
				val size = math.sqrt(tiles.length).toInt
				val list = List.fill(size)(List.fill(size)((Nil, 0)))
				val endGrid = tiles.indices.flatMap(tileIdx => transformTile(tiles(tileIdx)).map(transformedTile => {
					setTiles(tiles.patch(tileIdx, Nil, 1), list.updated(0, list.head.updated(0, transformedTile)))
				})).filter(_ != Nil)
				val endGridLists = endGrid.map(endGridElem => removeBorders(mergeGrid(endGridElem),
					endGridElem.head.head.length))
				println(endGridLists.head.map(_.count(_ == '#')).sum -
					transformTile(endGridLists.head).map(findSeaMonsters).filter(_ != 0).head * 15)
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	def findSeaMonsters(grid: List[String]): Int = {
		val monsterRegexes =
			"..................#." ::
			"#....##....##....###" ::
			".#..#..#..#..#..#..." :: Nil
		grid.dropRight(2).indices.count {rowIdx =>
			grid(rowIdx).indices.dropRight(19).exists {colIdx =>
				monsterRegexes.indices.forall(idx => grid(rowIdx + idx).slice(colIdx, colIdx + 20).
					matches(monsterRegexes(idx)))
			}
		}
	}

	def removeBorders(grid: List[String], interval: Int): List[String] = {
		val gridNoHorizBorder = grid.zipWithIndex.filterNot(elem =>
			elem._2 % interval == 0 || (elem._2 + 1) % interval == 0).map(_._1)
		gridNoHorizBorder.map(row => row.zipWithIndex.filterNot(elem =>
			elem._2 % interval == 0 || (elem._2 + 1) % interval == 0).map(_._1).mkString)
	}

	def mergeGrid(grid: List[List[List[String]]]): List[String] = {
		grid.indices.flatMap(row => grid(row).head.indices.map(tileRow => grid(row).indices.map(col =>
			grid(row)(col)(tileRow)).mkString)).toList
	}

	def setTiles(tiles: List[(List[String], Int)], grid: List[List[(List[String], Int)]]):  List[List[List[String]]] = {
		if(tiles.isEmpty) {
			grid.map(_.map(_._1))
		} else {
			val transformTiles = tiles.zipWithIndex.flatMap(tile => transformTile(tile._1).map((_, tile._2)))
			val position = grid.indices.map(idx1 => (idx1, grid(idx1).indices.find(idx2 => grid(idx1)(idx2)._1.
				isEmpty && getNeighbors(grid.map(_.map(_._1)), (idx1, idx2)).nonEmpty).getOrElse(Int.MinValue))).
				find(_._2 != Int.MinValue).get
			val validTransformVals = transformTiles.filter(tile =>
				checkInsert(grid.map(_.map(_._1)), tile._1._1, position)).map{tile =>
				val newTiles = tiles.patch(tile._2, Nil, 1)
				val newGrid = grid.updated(position._1, grid(position._1).updated(position._2, tile._1))
				setTiles(newTiles, newGrid)
			}.find(_ != Nil)
			if(validTransformVals.isEmpty) Nil else validTransformVals.get
		}
	}

	def checkInsert(grid: List[List[List[String]]], tile: List[String], pos: (Int, Int)): Boolean = {
		val newGrid = grid.updated(pos._1, grid(pos._1).updated(pos._2, tile))
		val neighbors = getNeighbors(newGrid, pos)
		if(neighbors.isEmpty) {
			false
		} else {
			neighbors.forall(neighbor => isNeighbors(tile, newGrid(neighbor._1)(neighbor._2), neighbor._3))
		}
	}

	def isNeighbors(tile1: List[String], tile2: List[String], direction: String): Boolean = {
		val linesCheck = direction match {
			case "U" => (tile1.head, tile2.last)
			case "D" => (tile1.last, tile2.head)
			case "R" => (tile1.map(_.last).mkString, tile2.map(_.head).mkString)
			case "L" => (tile1.map(_.head).mkString, tile2.map(_.last).mkString)
		}
		linesCheck._1 == linesCheck._2
	}

	def getNeighbors(grid: List[List[List[String]]], pos: (Int, Int)): List[(Int, Int, String)] = {
		val addTo = List((1, 0, "D"), (-1, 0, "U"), (0, 1, "R"), (0, -1, "L"))
		addTo.map(add => {
			val newPos = (pos._1 + add._1, pos._2 + add._2, add._3)
			if(newPos._1 >= 0 && newPos._2 >= 0 && newPos._1 < grid.length && newPos._2 < grid.head.length) {
				if(grid(newPos._1)(newPos._2).nonEmpty) {
					newPos
				} else {
					(-1, -1, "Z")
				}
			} else {
				(-1, -1, "Z")
			}
		}).filter(_ != (-1, -1, "Z"))
	}

	def transformTile(tile: List[String]): List[List[String]] = {
		(tile :: flipH(tile) :: flipV(tile) :: rotL(tile) :: rotR(tile) :: flipV(rotL(tile)) :: flipH(rotL(tile)) ::
			flipH(rotR(tile)) :: flipV(rotR(tile)) :: flipV(flipH(tile)) :: Nil).distinct
	}

	def transformTile(tile: (List[String], Int)): List[(List[String], Int)] = {
		(tile :: (flipH(tile._1), tile._2) :: (flipV(tile._1), tile._2) :: (rotL(tile._1), tile._2) ::
			(rotR(tile._1), tile._2) :: (flipV(rotL(tile._1)), tile._2) :: (flipH(rotL(tile._1)), tile._2) ::
			(flipH(rotR(tile._1)), tile._2) :: (flipV(rotR(tile._1)), tile._2) :: Nil).distinct
	}

	def flipV(tile: List[String]): List[String] = {
		tile.reverse
	}

	def flipH(tile: List[String]): List[String] = {
		tile.map(_.reverse)
	}

	def rotL(tile: List[String]): List[String] = {
		flipH(tile.indices.map(rowIdx => tile(rowIdx).indices.map(colIdx => tile(colIdx)(rowIdx)).mkString).toList)
	}

	def rotR(tile: List[String]): List[String] = {
		flipV(tile.indices.map(rowIdx => tile(rowIdx).indices.map(colIdx => tile(colIdx)(rowIdx)).mkString).toList)
	}

}
