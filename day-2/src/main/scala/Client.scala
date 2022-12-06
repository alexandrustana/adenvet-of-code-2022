import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val score = Map('X' -> 1, 'Y' -> 2, 'Z' -> 3)
    val draw = 3
    val win = 6
    val lose = 0

    def doIWin(op: Char, me: Char) = {
      val outcome = op match {
        case 'A' => me match {
            case 'X' => draw
            case 'Y' => win
            case 'Z' => lose
          }
        case 'B' => me match {
            case 'X' => lose
            case 'Y' => draw
            case 'Z' => win
          }
        case 'C' => me match {
            case 'X' => win
            case 'Y' => lose
            case 'Z' => draw
          }
      }
      score(me) + outcome
    }

    val input = Source.fromFile("src/main/resources/input.txt")

    input
      .getLines()
      .map(line => doIWin(line.head, line.last))
      .sum
  }

  private def part2() = {
    val score = Map('X' -> 0, 'Y' -> 3, 'Z' -> 6)
    val rock = 1
    val paper = 2
    val scissor = 3

    def doIWin(op: Char, me: Char) = {
      val outcome = op match {
        case 'A' => me match {
            case 'X' => scissor
            case 'Y' => rock
            case 'Z' => paper
          }
        case 'B' => me match {
            case 'X' => rock
            case 'Y' => paper
            case 'Z' => scissor
          }
        case 'C' => me match {
            case 'X' => paper
            case 'Y' => scissor
            case 'Z' => rock
          }
      }
      score(me) + outcome
    }

    val input = Source.fromFile("src/main/resources/input.txt")

    input
      .getLines()
      .map(line => doIWin(line.head, line.last))
      .sum
  }

}
