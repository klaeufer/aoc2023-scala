object Day6:

  type Race = (Long, Long)

  val number = """(\d+)""".r

  def readLongs(input: Iterator[String]): Iterator[Long] =
    number.findAllIn(input.next()).map(_.toLong)

  def readLineAsLong(input: Iterator[String]): Long =
    number.findAllIn(input.next()).foldLeft("")(_ + _).toLong

/*
  Solve inequality
  b(t - b) > r
  Want interval between roots
  b^2 - bt + r = 0
  Roots
  b = t/2 +/- sqrt((t/2)^2 - r)
 */

  def solveOne(race: Race) =
    val (t, d) = race
    val t_half = t / 2.0
    val r = math.sqrt(math.pow(t_half, 2) - d)
    math.ceil(t_half + r - 1).toLong - math.floor(t_half - r + 1).toLong + 1

  def main(args: Array[String]): Unit =
//    println(readLongs(example).zip(readLongs(example)).map(solveOne).product)
//    println(readLongs(input).zip(readLongs(input)).map(solveOne).product)
    println(solveOne(readLineAsLong(example), readLineAsLong(example)))
    println(solveOne(readLineAsLong(input), readLineAsLong(input)))

  val input = scala.io.Source.fromFile("data/day6input.txt").getLines()

  val example: Iterator[String] =
    import scala.language.unsafeNulls
    """Time:      7  15   30
      |Distance:  9  40  200
      |""".stripMargin.split("\n").iterator
