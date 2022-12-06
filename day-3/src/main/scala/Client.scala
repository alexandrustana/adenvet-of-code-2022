import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val input = Source.fromFile("src/main/resources/input.txt")
    val priority = (
      ('a' to 'z').zipWithIndex.map { case (c, i) => (c, i + 1) } ++
        ('A' to 'Z').zipWithIndex.map { case (c, i) => (c, i + 27) }
    ).toMap

    input
      .getLines()
      .map { items =>
        val left = items.take(items.length / 2)
        val right = items.drop(items.length / 2)
        val dup = left.intersect(right)
        priority(dup.head)
      }
      .toList
      .sum
  }

  private def part2() = {
    val input = Source.fromFile("src/main/resources/input.txt")
    val priority = (
      ('a' to 'z').zipWithIndex.map { case (c, i) => (c, i + 1) } ++
        ('A' to 'Z').zipWithIndex.map { case (c, i) => (c, i + 27) }
    ).toMap

    input
      .getLines()
      .grouped(3)
      .map(_.toList)
      .map { case elf1 :: elf2 :: elf3 :: _ =>
        val dup = elf1.intersect(elf2).intersect(elf3)
        priority(dup.head)
      }
      .toList
      .sum
  }

}
