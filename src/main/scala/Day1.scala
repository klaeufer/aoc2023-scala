// https://www.reddit.com/r/adventofcode/comments/1884fpl/2023_day_1for_those_who_stuck_on_part_2/
// https://stackoverflow.com/questions/41243815/overlapping-matches-in-regex-scala

object Day1:

  def main(args: Array[String]): Unit =
    runCalibration(1, simpleDigits)
    runCalibration(2, allDigits)

  def runCalibration(part: Int, digits: Seq[String]) =
    scala.util.Using(scala.io.Source.fromFile("data/day1Input.txt")):
      source =>
        val result = calibration(source.getLines(), digits)
        println(s"Day 1 part $part solution: $result")
    .foreach(identity)

  val simpleDigits: Seq[String] = (0 to 9).map(_.toString)
  val wordDigits: Seq[String] = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  val allDigits: Seq[String] = simpleDigits ++ wordDigits

  extension (s: String) def digitToInt: Int =
    val digitToStringMap = wordDigits.zip(1 to 9).toMap
    val digit: scala.util.matching.Regex = "(\\d)".r
    s match
      case digit(d) => d.toInt
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
