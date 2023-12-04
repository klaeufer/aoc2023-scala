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
    val size = line.size
    0 < start && isSymbol(line(start - 1))
      || end < size && isSymbol(line(end))
      || 0 < row && (math.max(0, start - 1) to math.min(size - 1, end)).exists(i => isSymbol(schematic(row - 1)(i)))
      || row < size - 1 && (math.max(0, start - 1) to math.min(size - 1, end)).exists(i => isSymbol(schematic(row + 1)(i)))

  val number = """\d+""".r

  def main(args: Array[String]): Unit =
    println(input
      .zipWithIndex
      .flatMap((line, row) => number.findAllIn(line).matchData.withFilter(isAdjacentToSymbol(input, row, _)))
      .map(_.toString.toInt)
      .sum
    )