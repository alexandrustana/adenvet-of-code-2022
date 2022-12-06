import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = getCalories.max

  private def part2() = getCalories.sortWith(_ > _).take(3).sum

  private def getCalories = {
    val input = Source.fromFile("src/main/resources/input.txt")

    val calories = input
      .getLines()
      .foldLeft(List(List.empty[Long])) { case (head :: tail, elem) =>
        if (elem.isEmpty) List.empty[Long] :: head :: tail
        else ((elem.toLong) :: head) :: tail
      }
      .map(_.sum)

    input.close()

    calories
  }
}
