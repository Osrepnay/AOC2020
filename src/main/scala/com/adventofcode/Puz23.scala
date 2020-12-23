package com.adventofcode

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz23 {

	def main(args: Array[String]): Unit = {
		val cups = Using(Source.fromURL(getClass.getResource("/input23.txt"))) {
			_.mkString.split("").map(_.toInt).toList
		}
		cups match {
			case Success(cups) =>
				val maps = listToMaps(cups)
				val list = CircularLinkedList(maps._1, maps._2).insertElems((maps._1.keysIterator.max + 1 to 1000000).
					toList, cups.last)
				val result = doMoves(list, cups.head, 10000000)
				println(result.getElementAhead(1, 1).toLong *
					result.getElementAhead(1, 2).toLong)
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def doMoves(cups: CircularLinkedList, currCup: Int, moveNum: Int = 100): CircularLinkedList = {
		if(moveNum == 0) {
			cups
		} else {
			val pickUp = toPickup(cups, currCup)
			val newCups = cups.removeElems(pickUp)
			val dest = findDest(newCups.removeElem(currCup), currCup)
			val newCurrCup = newCups.elems(currCup)
			val insertedCups = newCups.insertElems(pickUp, dest)
			doMoves(insertedCups, newCurrCup, moveNum - 1)
		}
	}

	@tailrec
	def findDest(cups: CircularLinkedList, currCup: Int): Int = {
		if(currCup - 1 < 1) {
			findDest(cups, 1000001)
		} else {
			val hasLabel = cups.elems.contains(currCup - 1)
			if(!hasLabel) {
				findDest(cups, currCup - 1)
			} else {
				currCup - 1
			}
		}
	}

	def toPickup(cups: CircularLinkedList, currCup: Int): List[Int] = {
		cups.elems(currCup) :: cups.getElementAhead(currCup, 2) :: cups.getElementAhead(currCup, 3) ::
			Nil
	}

	def listToMaps(list: List[Int]): (HashMap[Int, Int], HashMap[Int, Int]) = {
		(HashMap.empty ++ list.indices.dropRight(1).map(idx => list(idx) -> list(idx + 1)) + (list.last -> list.head),
			HashMap.empty ++ list.indices.dropRight(1).map(idx => list(idx + 1) -> list(idx)) +
				(list.head -> list.last))
	}

	def lListToList(llist: CircularLinkedList): List[Int] = {
		(0 until llist.elems.size).map(idx => llist.getElementAhead(3, idx)).toList
	}

	case class CircularLinkedList(elems: HashMap[Int, Int], revElems: HashMap[Int, Int]) {

		@tailrec
		final def insertElems(toInsertElems: List[Int], insertAfter: Int): CircularLinkedList = {
			if(toInsertElems.isEmpty) {
				this
			} else {
				val newList = insertElem(toInsertElems.head, insertAfter)
				newList.insertElems(toInsertElems.tail, toInsertElems.head)
			}
		}

		def insertElem(elem: Int, insertAfter: Int): CircularLinkedList = {
			val newMap = elems + (insertAfter -> elem) + (elem -> elems(insertAfter))
			val newRevMap = revElems + (elems(insertAfter) -> elem) + (elem -> insertAfter)
			CircularLinkedList(newMap, newRevMap)
		}

		@tailrec
		final def removeElems(toRemoveElems: List[Int]): CircularLinkedList = {
			if(toRemoveElems.isEmpty) {
				this
			} else {
				val newList = removeElem(toRemoveElems.head)
				newList.removeElems(toRemoveElems.tail)
			}
		}

		def removeElem(elem: Int): CircularLinkedList = {
			val newMap = (elems - elem) + (revElems(elem) -> elems(elem))
			val newRevMap = (revElems - elem) + (elems(elem) -> revElems(elem))
			CircularLinkedList(newMap, newRevMap)
		}

		@tailrec
		final def getElementAhead(elem: Int, aheadBy: Int): Int = if(aheadBy == 0) elem else
			getElementAhead(elems(elem), aheadBy - 1)

	}

}
