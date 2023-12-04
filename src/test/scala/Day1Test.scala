import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should

class Day1Test extends AnyFunSuite, should.Matchers:

  import Day1.*

  def example1 =
    import scala.language.unsafeNulls
    """1abc2
      |pqr3stu8vwx
      |a1b2c3d4e5f
      |treb7uchet""".stripMargin.split("\n").iterator

  def example2 =
    import scala.language.unsafeNulls
    """two1nine
      |eightwothree
      |abcone2threexyz
      |xtwone3four
      |4nineeightseven2
      |zoneight234
      |7pqrstsixteen""".stripMargin.split("\n").iterator

  test("Day 1 part 1"):
    assert(calibration(example1, simpleDigits) == 142)

  test("Day 1 part 2"):
    assert(calibration(example2, allDigits) == 281)
