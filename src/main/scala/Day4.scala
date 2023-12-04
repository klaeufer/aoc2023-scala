import scala.collection.immutable.Queue

object Day4:

  def example =
    import scala.language.unsafeNulls
    """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
      |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
      |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
      |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
      |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
      |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"""
    .stripMargin.split("\n").iterator.toIndexedSeq

  type Card = (Set[Int], Set[Int])

  def lineToCard(line: String): Card =
    val card = """Card *(\d+):(.*)\|(.*)""".r
    val number = """(\d+)""".r
    val card(n, w, a) = line: @unchecked
    (number.findAllIn(w).map(_.toInt).toSet, number.findAllIn(a).map(_.toInt).toSet)

  def addCardValues(input: IndexedSeq[String]): Int =
    input
      .map(lineToCard)
      .map((w, a) => math.pow(2, w.intersect(a).size - 1).toInt)
      .sum

  def processCards(cards: IndexedSeq[Card]): Int =
    Iterator.iterate(Queue(cards.zipWithIndex*)): q0 =>
        val (((w, a), k), q1) = q0.dequeue
        q1 ++ (1 to w.intersect(a).size).map(j => (cards(k + j), k + j))
      .takeWhile(_.nonEmpty)
      .length

  def input = scala.io.Source.fromFile("data/day4Input.txt").getLines().toIndexedSeq

  def main(args: Array[String]): Unit =
    println(s"day 4 part 1 example: ${addCardValues(example)}")
    println(s"day 4 part 1 solution: ${addCardValues(input)}")
    println(s"day 4 part 2 example: ${processCards(example.map(lineToCard))}")
    println(s"day 4 part 2 solution: ${processCards(input.map(lineToCard))}")
