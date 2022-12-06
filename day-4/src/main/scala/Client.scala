import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val input = Source.fromFile("src/main/resources/input.txt")
    input
      .getLines()
      .map { case s"$l1-$r1,$l2-$r2" =>
        val elf1 = (l1.toLong) to (r1.toLong)
        val elf2 = (l2.toLong) to (r2.toLong)
        elf1.containsSlice(elf2) || elf2.containsSlice(elf1)
      }
      .count(identity)
  }

  private def part2() = {
    val input = Source.fromFile("src/main/resources/input.txt")
    input
      .getLines()
      .map { case s"$l1-$r1,$l2-$r2" =>
        val elf1 = (l1.toLong) to (r1.toLong)
        val elf2 = (l2.toLong) to (r2.toLong)
        elf1.intersect(elf2).nonEmpty
      }
      .count(identity)
  }

}
