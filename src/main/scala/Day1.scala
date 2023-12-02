// https://www.reddit.com/r/adventofcode/comments/1884fpl/2023_day_1for_those_who_stuck_on_part_2/
// https://stackoverflow.com/questions/41243815/overlapping-matches-in-regex-scala

import scala.util.matching.Regex

object Day1 extends App:

  def example1 =
    import scala.language.unsafeNulls
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin('|').split("\n").iterator

  def example2 =
    import scala.language.unsafeNulls
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin('|').split("\n").iterator

  def input = scala.io.Source.fromFile("data/day1Input.txt").getLines()

  val digits1: Regex = "([0-9])".r
  val digits2 = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val digits3 = (0 to 9).map(_.toString)
  val digits4 = digits3 ++ digits2
  val digitToStringMap = digits2.zip(1 to 9).toMap

  println(digits4)

  extension (s: String) def digitToInt: Int = s match
    case digits1(d) => d.toInt
    case s => digitToStringMap(s)

  def calibration(data: Iterator[String], digits: Seq[String]): Int =
    data.map:
      s =>
        // The right calibration values for string "eighthree" is 83 and for "sevenine" is 79.
        // KL this was too complicated with regex
        val first = digits.map(d => (d, s.indexOf(d))).filter(_._2 >= 0).minBy(_._2)._1.digitToInt
        val last = digits.map(d => (d, s.lastIndexOf(d))).filter(_._2 >= 0).maxBy(_._2)._1.digitToInt
        val result = 10 * first + last
        result
    .sum

  println(s"Day 1 part 1 example:  ${calibration(example1, digits3)}")
  println(s"Day 1 part 1 solution: ${calibration(input, digits3)}")
  println(s"Day 1 part 2 example:  ${calibration(example2, digits4)}")
  println(s"Day 1 part 2 solution: ${calibration(input, digits4)}")
