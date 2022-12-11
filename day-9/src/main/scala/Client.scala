import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()
    val headMoves = List(0 -> 0)
    val tailMoves = List(0 -> 0)

    def isClose(src: (Int, Int), dst: (Int, Int)): Boolean = {
      val (y1, x1) = src
      val (y2, x2) = dst
      val dy = Math.abs(y1 - y2)
      val dx = Math.abs(x1 - x2)
      dy <= 1 && dx <= 1
    }

    def findDirection(src: (Int, Int), dst: (Int, Int)): (Int, Int) = {
      val (srcY, srcX) = src
      val (dstY, dstX) = dst
      val dy = dstY - srcY
      val dx = dstX - srcX
      val mvY = if (dy != 0) dy.sign * 1 else 0
      val mvX = if (dx != 0) dx.sign * 1 else 0
      (mvY, mvX)
    }

    def generateMoves(
      heads: List[(Int, Int)],
      tails: List[(Int, Int)],
      moves: List[(Int, Int)]
    ): (List[(Int, Int)], List[(Int, Int)]) = {
      val newHeads = moves ::: heads
      val newTails = moves.foldRight(tails) { case (move, acc) =>
        val tail @ (ty, tx) = acc.head
        if (isClose(tail, move)) acc
        else {
          val (dy, dx) = findDirection(tail, move)
          val newAcc = ((ty + dy) -> (tx + dx)) :: acc
          newAcc
        }
      }
      newHeads -> newTails
    }

    val (_, tails) = input.foldLeft(headMoves, tailMoves) { case ((heads, tails), move) =>
      move match {
        case s"L $n" =>
          val (hy, hx) = heads.head
          val moves = (1 to n.toInt).map(i => hy -> (hx - i)).toList.reverse

          generateMoves(heads, tails, moves)
        case s"R $n" =>
          val (hy, hx) = heads.head
          val moves = (1 to n.toInt).map(i => hy -> (hx + i)).toList.reverse

          generateMoves(heads, tails, moves)
        case s"U $n" =>
          val (hy, hx) = heads.head
          val moves = (1 to n.toInt).map(i => (hy - i) -> hx).toList.reverse

          generateMoves(heads, tails, moves)
        case s"D $n" =>
          val (hy, hx) = heads.head
          val moves = (1 to n.toInt).map(i => (hy + i) -> hx).toList.reverse

          generateMoves(heads, tails, moves)
      }
    }

    (tails.distinct.size)
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()
    val headMoves = List(0 -> 0)
    val tailMoves = List(0 -> 0)

    def isClose(src: (Int, Int), dst: (Int, Int)): Boolean = {
      val (y1, x1) = src
      val (y2, x2) = dst
      val dy = Math.abs(y1 - y2)
      val dx = Math.abs(x1 - x2)
      dy <= 1 && dx <= 1
    }

    def findDirection(src: (Int, Int), dst: (Int, Int)): (Int, Int) = {
      val (srcY, srcX) = src
      val (dstY, dstX) = dst
      val dy = dstY - srcY
      val dx = dstX - srcX
      val mvY = if (dy != 0) dy.sign * 1 else 0
      val mvX = if (dx != 0) dx.sign * 1 else 0
      (mvY, mvX)
    }

    def generateMoves(
      heads: List[(Int, Int)],
      tails: List[(Int, Int)],
      moves: List[(Int, Int)]
    ): (List[(Int, Int)], List[(Int, Int)]) = {
      val newHeads = moves ::: heads
      val newTails = moves.foldRight(tails) { case (move, acc) =>
        val tail @ (ty, tx) = acc.head
        if (isClose(tail, move)) acc
        else {
          val (dy, dx) = findDirection(tail, move)
          val newAcc = ((ty + dy) -> (tx + dx)) :: acc
          newAcc
        }
      }
      newHeads -> newTails
    }

    val (_, tails1) = input.foldLeft(headMoves, tailMoves) { case ((heads, tails), move) =>
      move match {
        case s"L $n" =>
          val (hy, hx) = heads.head
          val moves = (1 to n.toInt).map(i => hy -> (hx - i)).toList.reverse

          generateMoves(heads, tails, moves)
        case s"R $n" =>
          val (hy, hx) = heads.head
          val moves = (1 to n.toInt).map(i => hy -> (hx + i)).toList.reverse

          generateMoves(heads, tails, moves)
        case s"U $n" =>
          val (hy, hx) = heads.head
          val moves = (1 to n.toInt).map(i => (hy - i) -> hx).toList.reverse

          generateMoves(heads, tails, moves)
        case s"D $n" =>
          val (hy, hx) = heads.head
          val moves = (1 to n.toInt).map(i => (hy + i) -> hx).toList.reverse

          generateMoves(heads, tails, moves)
      }
    }

    val (_, tails2) = generateMoves(headMoves, tailMoves, tails1)
    val (_, tails3) = generateMoves(headMoves, tailMoves, tails2)
    val (_, tails4) = generateMoves(headMoves, tailMoves, tails3)
    val (_, tails5) = generateMoves(headMoves, tailMoves, tails4)
    val (_, tails6) = generateMoves(headMoves, tailMoves, tails5)
    val (_, tails7) = generateMoves(headMoves, tailMoves, tails6)
    val (_, tails8) = generateMoves(headMoves, tailMoves, tails7)
    val (_, tails9) = generateMoves(headMoves, tailMoves, tails8)

    tails9.distinct.size
  }
}
