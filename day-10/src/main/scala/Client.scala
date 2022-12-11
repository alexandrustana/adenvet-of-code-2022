import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    var cycle = 0
    var X = 1
    var signalStrength = 0

    input.foreach {
      case "noop" =>
        cycle = cycle + 1
        updateSignalStrength()
      case s"addx $n" =>
        cycle = cycle + 1
        updateSignalStrength()
        cycle = cycle + 1
        updateSignalStrength()
        X = n.toInt + X
    }

    def updateSignalStrength(): Unit = {
      val condition = List(20, 60, 100, 140, 180, 220)
      if (condition.contains(cycle)) {
        signalStrength = signalStrength + cycle * X
      } else ()
    }

    signalStrength
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    var cycle = 1
    var crt = List("")
    var X = 1

    input.foreach {
      case "noop" =>
        updateCRTAndSprite()
        cycle = cycle + 1
      case s"addx $n" =>
        updateCRTAndSprite()
        cycle = cycle + 1
        println(s"Start cycle $cycle: begin executing addx $n")
        updateCRTAndSprite()
        cycle = cycle + 1
        X = n.toInt + X
        println(s"End of cycle $cycle: finish executing addx $n (Register X is now $X)")
        println(s"Sprite position: ${(0 until X).map(_ => ".").mkString}###${(X + 3 until 39).map(_ => ".").mkString}")
        println()
    }

    def updateCRTAndSprite(): Unit = {
      val pos = cycle % 40
      if (pos >= X && pos < (X + 3)) {
        crt = ("#" + crt.head) :: crt.tail
      } else {
        crt = ("." + crt.head) :: crt.tail
      }

      println(s"During cycle  $cycle: CRT draws pixel in position $pos")
      println(s"Current CRT row: ${crt.head.reverse.mkString}")
      println()

      if (cycle % 40 == 0) {
        crt = "" :: crt
      } else ()
    }

    crt.map(_.reverse).reverse.mkString("\n")
  }
}
