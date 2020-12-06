package com.adventofcode

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz6{

	def main(args: Array[String]): Unit = {
		val answers=Using(Source.fromURL(getClass.getResource("/input6.txt"))){
			source => source.mkString.split("\n\n").
				map(_.split("\n").toList).toList
		}
		answers match{
			case Success(answers) =>
				println{
					answers.map(group => group.mkString.groupBy(_.toLower).keys.
						filter(char => group.count(_.contains(char))==group.length).toList.length).sum
				}
			case Failure(e) =>
				println(f"Unable to load class: $e")
		}
	}

}
