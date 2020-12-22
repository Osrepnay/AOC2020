package com.adventofcode

import scala.collection.immutable.{HashSet, Queue}
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz22 {

	def main(args: Array[String]): Unit = {
		val cardsList = Using(Source.fromURL(getClass.getResource("/input22.txt"))) {
			_.mkString.split("\n\n").map(_.split("\n").tail.map(_.toInt).toList).toList
		}
		cardsList match {
			case Success(cardsList) =>
				val cards = (Queue(cardsList.head: _*), Queue(cardsList.last: _*))
				println(doRounds(cards, HashSet.empty)._1)
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	def doRounds(cards: (Queue[Int], Queue[Int]), prevRounds: Set[(Queue[Int], Queue[Int])]): (Long, Int) = {
		if(cards._1.isEmpty) {
			(calcScore(cards._2), 2)
		} else if(cards._2.isEmpty) {
			(calcScore(cards._1), 1)
		} else if(prevRounds.contains(cards)) {
			(calcScore(cards._1), 1)
		} else {
			val newDecks = {
				val cardDequeues = (cards._1.dequeue, cards._2.dequeue)
				if(cardDequeues._1._1 < cards._1.length && cardDequeues._2._1 < cards._2.length) {
					val winner = doRounds((cardDequeues._1._2.take(cardDequeues._1._1),
						cardDequeues._2._2.take(cardDequeues._2._1)), HashSet.empty)._2
					if(winner == 1) {
						(cardDequeues._1._2.enqueue(cardDequeues._1._1).enqueue(cardDequeues._2._1), cardDequeues._2._2)
					} else {
						(cardDequeues._1._2, cardDequeues._2._2.enqueue(cardDequeues._2._1).enqueue(cardDequeues._1._1))
					}
				} else if(cardDequeues._1._1 > cardDequeues._2._1) {
					(cardDequeues._1._2.enqueue(cardDequeues._1._1).enqueue(cardDequeues._2._1), cardDequeues._2._2)
				} else {
					(cardDequeues._1._2, cardDequeues._2._2.enqueue(cardDequeues._2._1).enqueue(cardDequeues._1._1))
				}
			}
			doRounds(newDecks, prevRounds + cards)
		}
	}

	def calcScore(cardStack: Queue[Int]): Long = {
		cardStack.indices.map(idx => cardStack(idx).toLong * (cardStack.length - idx)).sum
	}

}
