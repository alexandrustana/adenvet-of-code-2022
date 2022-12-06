import scala.annotation.tailrec
import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList.head
    source.close()

    val stream = input.zipWithIndex.sliding(4).toList
    @tailrec
    def getStart(stream: List[IndexedSeq[(Char, Int)]]): (Boolean, Int) =
      stream match {
        case ::(head, tail) =>
          val data = head.map(_._1)
          if (data.distinct.size == data.size) {
            val res = true -> head.last._2
            res
          }
          else getStart(tail)
        case Nil => false -> -1
      }

    getStart(stream)._2 + 1
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList.head
    source.close()

    val stream = input.zipWithIndex.sliding(14).toList

    @tailrec
    def getStart(stream: List[IndexedSeq[(Char, Int)]]): (Boolean, Int) =
      stream match {
        case ::(head, tail) =>
          val data = head.map(_._1)
          if (data.distinct.size == data.size) {
            val res = true -> head.last._2
            res
          }
          else getStart(tail)
        case Nil => false -> -1
      }

    getStart(stream)._2 + 1
  }

}
