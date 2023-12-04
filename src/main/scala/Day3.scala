import scala.util.matching.Regex.Match

object Day3:

  val example =
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
      |.664.598..""".stripMargin.split("\n")

  val input = scala.util.Using(scala.io.Source.fromFile("data/day3Input.txt"))(_.getLines().toSeq).get

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

  def addPartNumbers(schematic: Seq[String]): Int =
    schematic
      .zipWithIndex
      .flatMap((line, row) => """\d+""".r.findAllIn(line).matchData.withFilter(isAdjacentToSymbol(schematic, row, _)))
      .map(_.toString.toInt)
      .sum

  def main(args: Array[String]): Unit =
    println(addPartNumbers(example))
    println(addPartNumbers(input))
