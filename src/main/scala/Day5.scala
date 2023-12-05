object Day5:

  val number = """(\d+)""".r

  def makeSeq(input: Iterator[String]): Seq[Long] =
    val list = number.findAllIn(input.next()).map(_.toLong).toSeq
    input.next() // skip blank line
    list

  def makeMap(input: Iterator[String]): Option[Long => Long] =
    Option.when(input.hasNext):
      input.next() // skip section header
      val ranges = input
        .takeWhile(_.trim.nonEmpty)
        .map: line =>
          val numbers = number.findAllIn(line).map(_.toLong)
          (numbers.next(), numbers.next(), numbers.next())
        .toSeq
      i => ranges
        .find((_, s, l) => (s until s + l).contains(i))
        .map(r => r._1 + i - r._2)
        .getOrElse(i)

  def processInput(input: Iterator[String]) =
    val seeds = makeSeq(input)
    val allMaps = Iterator.continually(makeMap(input)).takeWhile(_.nonEmpty)
    val seedToLocation = allMaps.map(_.get).toSeq.reverse.reduce(_.compose(_))
    seeds.map(seedToLocation).min

  def main(args: Array[String]): Unit =
    println(processInput(example))
    println(processInput(input))

  val input = scala.io.Source.fromFile("data/day5Input.txt").getLines()

  val example: Iterator[String] =
    import scala.language.unsafeNulls
    """seeds: 79 14 55 13
      |
      |seed-to-soil map:
      |50 98 2
      |52 50 48
      |
      |soil-to-fertilizer map:
      |0 15 37
      |37 52 2
      |39 0 15
      |
      |fertilizer-to-water map:
      |49 53 8
      |0 11 42
      |42 0 7
      |57 7 4
      |
      |water-to-light map:
      |88 18 7
      |18 25 70
      |
      |light-to-temperature map:
      |45 77 23
      |81 45 19
      |68 64 13
      |
      |temperature-to-humidity map:
      |0 69 1
      |1 0 69
      |
      |humidity-to-location map:
      |60 56 37
      |56 93 4
      |""".stripMargin.split("\n").iterator
