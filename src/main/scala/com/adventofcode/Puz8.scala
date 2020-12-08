package com.adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz8{

	def main(args: Array[String]): Unit = {
		val instructions=Using(Source.fromURL(getClass.getResource("/input8.txt"))){
			source => source.mkString.split("\n").map(instruction =>
				(instruction.split(" ").head, instruction.split(" ").last.toInt)).toList
		}
		instructions match{
			case Success(instructions) =>
				val idxNop=instructions.indices.filter{idx => instructions(idx)._1=="nop" &&
					(doInstruction(
						instructions.updated(idx, ("jmp", instructions(idx)._2)), 0, 0, List()
					).getOrElse(Integer.MIN_VALUE) match{
						case Integer.MIN_VALUE =>
							false
						case num: Int =>
							true
					})
				}
				if(idxNop.nonEmpty){
					println(doInstruction(instructions.updated(idxNop(0), ("jmp",
						instructions(idxNop(0))._2)), 0, 0, List()).getOrElse(""))
				}else{
					val idxJmp=instructions.indices.filter{idx =>
						instructions(idx)._1=="jmp" &&
							(doInstruction(
								instructions.updated(idx, ("nop", instructions(idx)._2)), 0, 0, List()
							).getOrElse(Integer.MIN_VALUE) match {
								case Integer.MIN_VALUE =>
									false
								case num: Int =>
									true
							})
					}
					if(idxJmp.nonEmpty){
						println(doInstruction(instructions.updated(idxJmp(0), ("nop",
							instructions(idxJmp(0))._2)), 0, 0, List()).getOrElse(""))
					}else{
						println("Could not find jmp/nop to flip.")
					}
				}
			case Failure(e) =>
				println(s"Unable to open file: $e")
		}
	}

	@tailrec
	def doInstruction(instructions: List[(String, Int)], idx: Int, accumulator: Int, visitedIndices: List[Int]): Option[Int] = {
		if(visitedIndices.contains(idx)){
			None
		}else{
			instructions(idx)._1 match {
				case "nop" =>
					if(idx<instructions.length-1){
						doInstruction(instructions, idx+1, accumulator, idx :: visitedIndices)
					}else{
						Some(accumulator)
					}
				case "acc" =>
					if(idx<instructions.length-1){
						doInstruction(instructions, idx+1, accumulator+instructions(idx)._2, idx :: visitedIndices)
					}else{
						Some(accumulator+instructions(idx)._2)
					}
				case "jmp" =>
					if(idx+instructions(idx)._2>=0 && idx+instructions(idx)._2<instructions.length){
						doInstruction(instructions, idx+instructions(idx)._2, accumulator, idx :: visitedIndices)
					}else{
						Some(accumulator)
					}
			}
		}
	}

}
