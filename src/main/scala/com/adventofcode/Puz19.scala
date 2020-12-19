package com.adventofcode

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz19 {

	def main(args: Array[String]): Unit = {
		val lines = Using(Source.fromURL(getClass.getResource("/input19.txt"))) {
			source => source.mkString.split("\n\n").map(group => group.split("\n").toList).toList
		}
		lines match {
			case Success(lines) =>
				val rulesNoRegex = lines.head.sortWith(_.head.toInt < _.head.toInt)
				//*vomits*
				val thirtyRegexes = s"""(${(1 to 30).map(times => s"(${toRegex(rulesNoRegex,
					findRule(rulesNoRegex, 42))}){$times}" +
					s"(${toRegex(rulesNoRegex, findRule(rulesNoRegex, 31))}){$times}").mkString("|")})"""
				val uberRegex = toRegex(rulesNoRegex, rulesNoRegex.head.drop(3)) + thirtyRegexes
				val messages = lines.last
				println(messages.count(message => {
					message.matches(uberRegex)
				}))
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	def toRegex(rules: List[String], rule: String): String = {
		if(rule.contains("\"")) {
			rule.slice(1, rule.length - 1)
		} else if(rule.contains("|")) {
			rule.split(" \\| ").map(half => s"(${toRegex(rules, half)})").mkString("|")
		} else {
			rule.split(" ").map(ruleNum => {
				//hacky but idc
				if(ruleNum == "8") {
					s"((${toRegex(rules, findRule(rules, 42))})+)"
				} else if(ruleNum == "11") {
					""
				} else {
					s"(${toRegex(rules, findRule(rules, ruleNum.toInt))})"
				}
			}).mkString
		}
	}

	def findRule(rules: List[String], ruleNum: Int): String = {
		rules.find(rule => rule.split(": ").head == ruleNum.toString).get.split(": ").last
	}

}
