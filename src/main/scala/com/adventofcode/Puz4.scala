package com.adventofcode

import scala.io.Source
import scala.util.{Failure, Success, Using}

object Puz4{

	def main(args: Array[String]): Unit = {
		val passports=Using(
			Source fromURL getClass.getResource("/input4.txt")
		){
			source => source.mkString("").
				split("\n\n").toList.
				map(_.split("\n|\\s")
					.map(item => item.split(":")(0) -> item.split(":")(1)).toMap)
		}
		val neededKeysOptional=("byr" :: "iyr" :: "eyr" :: "hgt" :: "hcl" :: "ecl" :: "pid" :: Nil).toSet
		val neededKeys=("byr" :: "iyr" :: "eyr" :: "hgt" :: "hcl" :: "ecl" :: "pid" :: "cid" :: Nil).toSet
		passports match{
			case Success(passports) =>
				println(passports.count{passport =>
					(passport.keySet==neededKeys || passport.keySet==neededKeysOptional) &&
						passport.forall(item => validateField(item._2, item._1))
				})
				println(passports.length)
			case Failure(e) =>
				println("Unable to read file: %s", e)
		}
	}

	def validateField(field: String, key: String): Boolean = {
		try{
			key match {
				case "byr" => field.toInt>=1920 && field.toInt<=2002
				case "iyr" => field.toInt>=2010 && field.toInt<=2020
				case "eyr" => field.toInt>=2020 && field.toInt<=2030
				case "hgt" => if((field takeRight 2)=="cm"){
					val num=field.substring(0, field.length-2).toInt
					num>=150 && num<=193
				}else if((field takeRight 2)=="in"){
					val num=field.substring(0, field.length-2).toInt
					num>=59 && num<=76
				}else{
					false
				}
				case "hcl" => field.matches("#[0-9a-f]{6}")
				case "ecl" => ("amb" :: "blu" :: "brn" :: "gry" :: "grn" :: "hzl" :: "oth" :: Nil).
					contains(field)
				case "pid" => field.toInt>=0 && field.length==9
				case "cid" => true
			}
		}catch{
			case _: Throwable => false
		}
	}

}
