package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz24 {

	def main(args: Array[String]): Unit = {
		val tiles = Using(Source.fromURL(getClass.getResource("/input24.txt"))) {
			_.mkString.split("\n").map(line => calcTile(parseTiles(line.split("").toList), (0, 0))).toList
		}
		tiles match {
			case Success(tiles) =>
				println(simplify(doRounds(simplify(tiles))).length)
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def doRounds(tiles: List[(Int, Int)], rounds: Int = 100): List[(Int, Int)] = {
		if(rounds == 0) {
			tiles
		} else {
			val newTiles = tiles.flatMap(possNeighbors).distinct.filterNot(tiles.contains(_)).
				filter(tile => numNeighbors(tile, tiles) == 2)
			val oldTiles = tiles.filterNot(tile => numNeighbors(tile, tiles) == 0 || numNeighbors(tile, tiles) > 2)
			doRounds(newTiles ::: oldTiles, rounds - 1)
		}
	}

	def simplify(tiles: List[(Int, Int)]): List[(Int, Int)] = {
		tiles.distinct.filterNot(tile => tiles.count(_ == tile) % 2 == 0)
	}

	def possNeighbors(tile: (Int, Int)): List[(Int, Int)] = {
		List("e", "w", "nw", "sw", "ne", "se").map(moveTile(tile, _))
	}

	def moveTile(tile: (Int, Int), direction: String): (Int, Int) = {
		direction match {
			case "e" => (tile._1 + 2, tile._2)
			case "w" => (tile._1 - 2, tile._2)
			case "nw" => (tile._1 - 1, tile._2 + 2)
			case "sw" => (tile._1 - 1, tile._2 - 2)
			case "ne" => (tile._1 + 1, tile._2 + 2)
			case "se" => (tile._1 + 1, tile._2 - 2)
		}
	}

	def numNeighbors(tile: (Int, Int), tiles: List[(Int, Int)]): Int = {
		tiles.count(areNeighbors(tile, _))
	}

	def areNeighbors(tile1: (Int, Int), tile2: (Int, Int)): Boolean = {
		val neighbors = (tile1._1 - tile2._1).abs + (tile1._2 - tile2._2).abs
		neighbors <= 3 && neighbors > 0
	}

	@tailrec
	def calcTile(directions: List[String], startPos: (Int, Int)): (Int, Int) = {
		if(directions.isEmpty) {
			startPos
		} else {
			directions.head match {
				case "e" => calcTile(directions.tail, (startPos._1 + 2, startPos._2))
				case "w" => calcTile(directions.tail, (startPos._1 - 2, startPos._2))
				case "nw" => calcTile(directions.tail, (startPos._1 - 1, startPos._2 + 2))
				case "sw" => calcTile(directions.tail, (startPos._1 - 1, startPos._2 - 2))
				case "ne" => calcTile(directions.tail, (startPos._1 + 1, startPos._2 + 2))
				case "se" => calcTile(directions.tail, (startPos._1 + 1, startPos._2 - 2))
			}
		}
	}

	def parseTiles(directions: List[String]): List[String] = {
		if(directions.isEmpty){
			Nil
		} else {
			val validDirections = List("e", "se", "sw", "w", "nw", "ne")
			if(validDirections.contains(directions.head)) {
				directions.head :: parseTiles(directions.tail)
			} else {
				directions.take(2).mkString :: parseTiles(directions.drop(2))
			}
		}
	}

}
