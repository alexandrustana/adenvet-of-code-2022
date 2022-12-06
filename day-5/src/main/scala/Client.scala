import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    val rows = input
      .takeWhile(_.nonEmpty)
      .map(_.grouped(4).toList.map(_.filterNot(_.isSpaceChar)))
      .init

    val length = rows.map(_.length).max

    val crates = rows
      .map(_.padTo(length, ""))
      .transpose
      .map(_.filter(_ != ""))

    input
      .dropWhile(_.nonEmpty)
      .tail
      .foldLeft(crates) { case (crates, s"move $q from $s to $d") =>
        val srcI = (s.toInt) - 1
        val dstI = (d.toInt) - 1
        val (elems, srcPile) = crates(srcI).splitAt(q.toInt)
        val dstPile = (elems.reverse) ::: crates(dstI)

        crates
          .updated(srcI, srcPile)
          .updated(dstI, dstPile)
      }
      .map(_.head.replace("[", "").replace("]", ""))
      .mkString
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    val rows = input
      .takeWhile(_.nonEmpty)
      .map(_.grouped(4).toList.map(_.filterNot(_.isSpaceChar)))
      .init

    val length = rows.map(_.length).max

    val crates = rows
      .map(_.padTo(length, ""))
      .transpose
      .map(_.filter(_ != ""))

    input
      .dropWhile(_.nonEmpty)
      .tail
      .foldLeft(crates) { case (crates, s"move $q from $s to $d") =>
        val srcI = (s.toInt) - 1
        val dstI = (d.toInt) - 1
        val (elems, srcPile) = crates(srcI).splitAt(q.toInt)
        val dstPile = elems ::: crates(dstI)

        crates
          .updated(srcI, srcPile)
          .updated(dstI, dstPile)
      }
      .map(_.head.replace("[", "").replace("]", ""))
      .mkString
  }

}
