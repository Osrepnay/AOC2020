package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz14 {

	def main(args: Array[String]): Unit = {
		val commands = Using(Source.fromURL(getClass.getResource("/input14.txt"))){
			source => source.mkString.split("\n").map(_.split("\\[|] = | = ").toList).toList
		}
		commands match {
			case Success(commands) =>
				val commandResult = doCommands(commands, List(), List.fill(36)("X").mkString)
				println(commandResult.map(num => num._2.toLong).sum)
			case Failure(e)	=>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def doCommands(commands: List[List[String]], memory: List[(String, String)],
		mask: String): List[(String, String)] = {
		if(commands.length <= 0) {
			memory
		} else {
			val command = commands.head
			val newCommands = commands.drop(1)
			command.head match {
				case "mask" =>
					doCommands(newCommands, memory, command.last)
				case "mem" =>
					val memBin = command(1).toInt.toBinaryString
					val memBinPadded = List.fill(36 - memBin.length)("0").mkString + memBin
					val newMemIdxs = getPossibilities(maskStr(memBinPadded, mask))
					doCommands(newCommands, updateMult(memory, newMemIdxs, command.last), mask)
			}
		}
	}

	//horrible
	def getPossibilities(bits: String): List[String] = {
		val numXs = bits.count(_ == 'X')
		val range = (0 until math.pow(2, numXs).toInt).
			map(_.toBinaryString.reverse).map(bin => bin + List.fill(36 - bin.length)("0").mkString)
		val bitsReversed = bits.reverse
		val indicesOfXs = bits.indices.filter(idx => bitsReversed(idx) == 'X')
		range.indices.map(binIdx => bitsReversed.indices.map(idx =>
			if(indicesOfXs contains idx) range(binIdx)(indicesOfXs indexOf idx)
			else bitsReversed(idx)).mkString.reverse).toList
	}

	def maskStr(maskend: String, mask: String): String = {
		maskend.indices.map(idx => if(mask(idx) == '0') maskend(idx) else mask(idx)).mkString
	}

	def updateMult(update: List[(String, String)], indices: List[String], updateVal: String): List[(String, String)] = {
		val addTo = indices.filter(num => !update.exists(_._1 == num)).map(num => (num, updateVal))
		update.map(item => {
			val contains = indices.indexOf(item._1)
			if(contains == -1) {
				item
			} else {
				(indices(contains), updateVal)
			}
		}) ::: addTo
	}

}
