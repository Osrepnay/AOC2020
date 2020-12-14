package com.adventofcode

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz13 {

	def main(args: Array[String]): Unit = {
		val lines = Using(Source.fromURL(getClass.getResource("/input13.txt"))) {
			source => source.mkString.split("\n").toList
		}
		lines match {
			case Success(lines) =>
				val buses = lines.last.split(",").filterNot(_ == "x").map(_.toLong).toList
				val busesRaw = lines.last.split(",").toList
				val remainders = buses.map(bus => (bus - busesRaw.indexOf(bus.toString)) % bus)
				println(solveCRT(buses, remainders))
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	def solveCRT(mods: List[Long], remainders: List[Long]): Long = {
		val z = mods.map(mod => mods.product / mod)
		val y = z.indices.map(idx => modularInverse(z(idx), mods(idx)))
		val w = y.indices.map(idx => (y(idx) * z(idx)) % mods.product)
		remainders.indices.map(idx => remainders(idx) * w(idx)).sum % mods.product
	}

	def modularInverse(num1: Long, num2: Long): Long = (BigInt(num1) modInverse num2).toLong

}
