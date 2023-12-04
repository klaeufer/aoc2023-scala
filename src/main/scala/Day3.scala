import scala.util.matching.Regex.Match

object Day3:

  type Schematic = Seq[String]
  type Pos = (Int, Int)

  val example: Schematic =
    import scala.language.unsafeNulls
    """467..114..
      |...*......
      |..35..633.
      |......#...
      |617*......
      |.....+.58.
      |..592.....
      |......755.
      |...$.*....
      |.664.598..""".stripMargin.split("\n").toIndexedSeq

  val input: Schematic = scala.util.Using(scala.io.Source.fromFile("data/day3Input.txt"))(_.getLines().toSeq).get

  def isSymbol(c: Char) = """!\"#$%&'()*+,-/:;<=>?@[\\]^_`{|}~""".contains(c)

  def isAdjacentToSymbol(schematic: Seq[String], row: Int, m: Match) =
    val start = m.start
    val end = m.end
    val line = schematic(row)
    val height = schematic.size
    val width = line.size
    0 < start && isSymbol(line(start - 1)) || end < width && isSymbol(line(end))
      || (math.max(0, start - 1) to math.min(width - 1, end))
           .exists(i => row > 0 && isSymbol(schematic(row - 1)(i)) || row < height - 1 && isSymbol(schematic(row + 1)(i)))

  val number = """\d+""".r

  def addPartNumbers(schematic: Seq[String]): Int =
    schematic
      .zipWithIndex
      .flatMap:
        (line, row) => number
          .findAllIn(line)
          .matchData
          .withFilter(isAdjacentToSymbol(schematic, row, _))
      .map(_.toString.toInt)
      .sum

  def starPositions(schematic: Schematic): Seq[Pos] =
    schematic
      .zipWithIndex
      .flatMap:
        (line, row) => line
          .zipWithIndex
          .withFilter(_._1 == '*')
          .map((_, col) => (row, col))

  def adjacentPartNumbers(schematic: Schematic, pos: Pos) =
    val (row, col) = pos
    val line = schematic(row)
    val matches =
      (if row > 0 then number.findAllIn(schematic(row - 1)).matchData else Iterator.empty)
        ++ (if row + 1 < schematic.size then number.findAllIn(schematic(row + 1)).matchData else Iterator.empty)
        ++ number.findAllIn(line).matchData
    matches
//      .map(m => { println(s"[$row,$col] ${m.start}:${m.end} ${m.toString}") ; m })
      .filter(m => m.start - 1 <= col && col <= m.end)
      .map(_.toString.toInt)

  def gearRatios(schematic: Schematic) =
    starPositions(schematic)
      .map(adjacentPartNumbers(schematic, _).toSeq)
      .withFilter(_.size == 2)
      .map(_.product)
      .sum

  def main(args: Array[String]): Unit =
    println(addPartNumbers(example))
    println(addPartNumbers(input))
    println(starPositions(example))
    println(adjacentPartNumbers(example, (1, 3)).toSeq)
    println(adjacentPartNumbers(example, (4, 3)).toSeq)
    println(adjacentPartNumbers(example, (8, 5)).toSeq)
    println(gearRatios(example))
    println(gearRatios(input))
