package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz16 {

	def main(args: Array[String]): Unit = {
		val lines = Using(Source.fromURL(getClass.getResource("/input16.txt"))) {
			_.mkString.split("\n").toList
		}
		lines match {
			case Success(lines) =>
				val otherTickets = lines.drop(lines.indexOf("nearby tickets:") + 1).map(_.split(",").toList)
				val rules = lines.take(lines.indexOf("your ticket:") - 1).map(_.split(" or |: |-").toList)
				val validTickets = otherTickets.filter(_.forall(char => canValidChar(rules, char)))
				val rearrangedTickets = validTickets.head.indices.map(idx => validTickets.map(_ (idx))).toList
				val rulePos = rearrangedTickets.map(col => testRule(rules, col))
				val ruleOrder = getRuleOrder(rulePos)
				val myTicket = lines(lines.indexOf("your ticket:") + 1).split(",").map(_.toLong).toList
				val departureRules = ruleOrder.indices.filter(idx =>
					rules(ruleOrder(idx)).head.split(" ").head == "departure")
				println(departureRules.map(idx => myTicket(idx)).product)
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def getRuleOrder(rulePos: List[List[Int]]): List[Int] = {
		if(rulePos.forall(_.length <= 1)) {
			rulePos.map(_.head)
		} else {
			val singleChars = rulePos.filter(_.length == 1).map(_.head)
			getRuleOrder(rulePos.map(pos => pos.filter(idx => pos.length == 1 || !singleChars.contains(idx))))
		}
	}

	def testRule(rules: List[List[String]], ticketCol: List[String]): List[Int] = {
		rules.indices.filter(idx => validCharsRule(rules(idx), ticketCol)).toList
	}

	def validCharsRule(rule: List[String], chars: List[String]): Boolean = {
		val ruleNums = rule.drop(1).map(_.toInt)
		chars.forall(char => {
			val charInt = char.toInt
			(ruleNums.head <= charInt && ruleNums(1) >= charInt) || (ruleNums(2) <= charInt && ruleNums.last >= charInt)
		})
	}

	def canValidChar(rules: List[List[String]], char: String): Boolean = {
		val charInt = char.toInt
		rules.exists(rule => {
			val ruleNums = rule.drop(1).map(_.toInt)
			(ruleNums.head <= charInt && ruleNums(1) >= charInt) || (ruleNums(2) <= charInt && ruleNums.last >= charInt)
		})
	}

}
