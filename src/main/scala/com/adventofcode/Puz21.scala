package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz21 {

	def main(args: Array[String]): Unit = {
		val ingredientsList = Using(Source.fromURL(getClass.getResource("/input21.txt"))) {
			_.mkString.split("\n").map(_.split(" \\(contains ").map(_.filter(_ != ')').
				split(", | ").toList).toList).toList
		}
		ingredientsList match {
			case Success(ingredientsList) =>
				val ingredientsTuple = (ingredientsList.map(_.head), ingredientsList.map(_.last))
				val allIngredients = ingredientsTuple._1.flatten.distinct
				val allAllergens = ingredientsTuple._2.flatten.distinct.sorted
				val possCausers = allAllergens.map(allergen => possibleIngredients(ingredientsTuple,
					allIngredients, allergen))
				println(getCauserOrder(possCausers).mkString(","))
			case Failure(e) =>
		}
	}

	@tailrec
	def getCauserOrder(possCausers: List[List[String]]): List[String] = {
		if(possCausers.forall(_.length == 1)) {
			possCausers.map(_.head)
		} else {
			val certainCausers = possCausers.filter(_.length == 1).map(_.head)
			getCauserOrder(possCausers.indices.map(idx => possCausers(idx).filter(causer =>
				possCausers(idx).length == 1 || !certainCausers.contains(causer))).toList)
		}
	}

	def possibleIngredients(ingredients: (List[List[String]], List[List[String]]), allIngredients: List[String],
		allergen: String): List[String] = {
		if(allIngredients.isEmpty) {
			Nil
		} else {
			val possIngredient = allIngredients.head
			val canCause = ingredients._1.indices.filter(ingredients._2(_).contains(allergen)).forall(idx =>
				ingredients._1(idx).contains(possIngredient))
			if(canCause) {
				possIngredient :: possibleIngredients(ingredients, allIngredients.drop(1), allergen)
			} else {
				possibleIngredients(ingredients, allIngredients.drop(1), allergen)
			}
		}
	}

}
