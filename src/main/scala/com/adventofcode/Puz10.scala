package com.adventofcode

import scala.collection.immutable.HashMap
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz10 {

	def main(args: Array[String]): Unit = {
		val adapters=Using(Source.fromURL(getClass.getResource("/input10.txt"))){
			source => source.mkString.split("\n").map(_.toInt).toList.sorted
		}
		adapters match{
			case Success(adapters) =>
				val adaptersFixed = 0 :: (adapters :+ adapters.last+3)
				val diffs=adaptersFixed.take(adaptersFixed.length-1).indices.
					map(adapterIdx => adaptersFixed(adapterIdx+1)-adaptersFixed(adapterIdx)).toList
				val ones=diffs.mkString.split("3+").toList
				val onesToPos=HashMap(1 -> 1L, 2 -> 2L, 3 -> 4L, 4 -> 7L)
				println(ones.map(one => onesToPos(one.length)).product)
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

}
