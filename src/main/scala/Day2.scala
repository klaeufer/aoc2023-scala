// https://dotty.epfl.ch/api/scala/util/matching/Regex.html
// https://stackoverflow.com/questions/54787540/scala-pattern-match-repeated-regex-groups

object Day2:

  val RED = "red"
  val GREEN = "green"
  val BLUE = "blue"

  def example1 =
    import scala.language.unsafeNulls
    """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
      |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
      |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
      |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
      |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""
    .stripMargin.split("\n").iterator

  type Bag = Map[String, Int]
  type Game = (Int, Seq[Bag])

  def lineToGame(line: String): Game =
    val game = """Game (\d+):(.*)""".r
    val subset = """([^;]+)""".r
    val cube = """(\d+) +(red|green|blue)""".r
    val game(n, r) = line
    (n.toInt,
      subset.findAllIn(r).map:
        cube.findAllIn(_).matchData.map:
          m => (m.group(2), m.group(1).toInt)
        .toMap
      .toSeq
    )

  def isGamePossible(bag: Bag, game: Game) =
    game._2.forall:
      _.forall:
        (k, v) => v <= bag.getOrElse(k, 0)

  def minCubesPower(game: Game): Int =
    Seq(RED, GREEN, BLUE).map:
      col =>
        game._2.map(_.getOrElse(col, 0))
    .map:
      _.max
    .product

  val bag1 = Map(RED -> 12, GREEN -> 13, BLUE -> 14)

  def main(args: Array[String]): Unit =
    println(example1.map(lineToGame).filter(isGamePossible(bag1, _)).map(_._1).sum) // 8
    println(example1.map(lineToGame).map(minCubesPower).sum) // 2286
    scala.util.Using(scala.io.Source.fromFile("data/day2Input.txt")):
      source =>
        val result = source.getLines().map(lineToGame).filter(isGamePossible(bag1, _)).map(_._1).sum
        println(s"Day 1 part 1 solution: $result")
    .foreach(identity)
    scala.util.Using(scala.io.Source.fromFile("data/day2Input.txt")):
      source =>
        val result = source.getLines().map(lineToGame).map(minCubesPower).sum
        println(s"Day 1 part 2 solution: $result")
    .foreach(identity)
