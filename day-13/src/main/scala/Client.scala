import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    def compare(left: String, right: String): Boolean = (left.head, right.head) match {
      case (a, b) if a == b => compare(left.tail, right.tail)
      case (']', _)         => true
      case (_, ']')         => false
      case ('[', b)         => compare(left.tail, b + "]" + right.tail)
      case (a, '[')         => compare(a + "]" + left.tail, right.tail)
      case (a, b)           => a < b
    }

    input
      .filterNot(_.isBlank)
      .map(_.replace("10", "A"))
      .grouped(2)
      .zipWithIndex
      .collect { case (Seq(left, right), index) if compare(left, right) => index + 1 }
      .sum
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    def compare(left: String, right: String): Boolean = (left.head, right.head) match {
      case (a, b) if a == b => compare(left.tail, right.tail)
      case (']', _)         => true
      case (_, ']')         => false
      case ('[', b)         => compare(left.tail, b + "]" + right.tail)
      case (a, '[')         => compare(a + "]" + left.tail, right.tail)
      case (a, b)           => a < b
    }

    val dividers = List("[[2]]", "[[6]]")

    val sorted = (input ++ dividers)
      .filterNot(_.isBlank)
      .map(_.replace("10", "A"))
      .sortWith(compare)

    dividers.map(sorted.indexOf(_) + 1).product
  }
}
