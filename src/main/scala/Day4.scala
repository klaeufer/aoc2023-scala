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
    val card(n, w, a) = line
    (number.findAllIn(w).map(_.toInt).toSet, number.findAllIn(a).map(_.toInt).toSet)

  def addCardValues(input: IndexedSeq[String]): Int =
    input
      .map(lineToCard(_))
      .map((w, a) => math.pow(2, w.intersect(a).size - 1).toInt)
      .sum

  def processCard(cards: IndexedSeq[Card], i: Int): Int =
    Iterator.iterate(Queue((i, cards(i)))): queue =>
      val ((k, (w, a)), q) = queue.dequeue
//      println(((w, a), q).toString)
      q ++ (1 to w.intersect(a).size).map(j => (k + j, cards(k + j)))
    .takeWhile(_.nonEmpty)
    .length

  def input = scala.io.Source.fromFile("data/day4Input.txt").getLines().toIndexedSeq

  def main(args: Array[String]): Unit =
    println(addCardValues(example))
    println(addCardValues(input))
    println((0 until example.size).map(processCard(example.map(lineToCard(_)), _)).sum)
    println((0 until input.size).map(processCard(input.map(lineToCard(_)), _)).sum)
