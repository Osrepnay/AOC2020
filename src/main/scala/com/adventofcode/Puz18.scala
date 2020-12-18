package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz18 {

	def main(args: Array[String]): Unit = {
		val expressions = Using(Source.fromURL(getClass.getResource("/input18.txt"))) {
			source => source.mkString.split("\n").map(expression =>
				expression.replace(" ", "").split("").toList).toList
		}
		expressions match {
			case Success(expressions) =>
				println(expressions.map(expression => solveExpression(expression)).sum)
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	def solveExpression(expression: List[String]): Long = {
		val firstParens = expression.indexOf("(")
		if(firstParens != -1) {
			val lastParens = endParensIdx(expression, firstParens + 1)
			val solvedInner = solveExpression(expression.slice(firstParens + 1, lastParens))
			solveExpression((expression.take(firstParens) :+ solvedInner.toString) :::
				expression.takeRight(expression.length - lastParens - 1))
		} else {
			solveExpressionNoParens(expression)
		}
	}

	@tailrec
	def solveExpressionNoParens(expression: List[String]): Long = {
		if(expression.length == 1) {
			expression.head.toLong
		} else {
			val firstAdd = expression.indexOf("+")
			if(firstAdd == -1) {
				val firstMult = expression.indexOf("*")
				val result = expression(firstMult - 1).toLong * expression(firstMult + 1).toLong
				solveExpressionNoParens((expression.take(firstMult - 1) :+ result.toString) :::
					expression.takeRight(expression.length - firstMult - 2))
			} else {
				val result = expression(firstAdd - 1).toLong + expression(firstAdd + 1).toLong
				solveExpressionNoParens((expression.take(firstAdd - 1) :+ result.toString) :::
					expression.takeRight(expression.length - firstAdd - 2))
			}
		}
	}

	@tailrec
	def endParensIdx(expression: List[String], counter: Int = 0, ignoreNum: Int = 0): Int = {
		if(expression(counter) == "(") {
			endParensIdx(expression, counter + 1, ignoreNum + 1)
		} else if(expression(counter) == ")") {
			if(ignoreNum == 0) {
				counter
			} else
				endParensIdx(expression, counter + 1, ignoreNum - 1)
		} else {
			endParensIdx(expression, counter + 1, ignoreNum)
		}
	}

}
