// https://www.reddit.com/r/adventofcode/comments/18b560a/2023_day_5_part_2_cpu_goes_brrr/

// TODO It is because with ranges, everything inside the range are valid. 
// I only needed to know the smallest continuous chunk and look from there. 
// It is a bet since all my ranges are in high millions and any continuous 
// chunks should not be smaller than 1000. 

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
        .takeWhile(_.trim.nn.nonEmpty)
        .map: line =>
          val numbers = number.findAllIn(line).map(_.toLong)
          (numbers.next(), numbers.next(), numbers.next())
        .toSeq
      i => ranges
        .find((_, s, l) => (s until s + l).contains(i))
        .map((b, s, _) => b + i - s)
        .getOrElse(i)

  def processPart1(input: Iterator[String]) =
    val seeds = makeSeq(input)
    val allMaps = Iterator.continually(makeMap(input)).takeWhile(_.nonEmpty)
    val seedToLocation = allMaps.map(_.get).toSeq.reverse.reduce(_.compose(_))
    seeds.map(seedToLocation).min

  def processPart2(input: Iterator[String]) =
    val seeds = makeSeq(input)
    val allMaps = Iterator.continually(makeMap(input)).takeWhile(_.nonEmpty)
    val seedToLocation = allMaps.map(_.get).toSeq.reverse.reduce(_.compose(_))
    seeds.sliding(2, 2).map: p =>
      (p.head until p.head + p.last)
//        .tapEach(println)
        .map(seedToLocation).min
//    .tapEach(println)
    .min

  def main(args: Array[String]): Unit =
    println(processPart1(example))
    println(processPart1(input))
    println(processPart2(example))
    println(processPart2(input))

  def input = scala.io.Source.fromFile("data/day5input.txt").getLines()

  def example: Iterator[String] =
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
