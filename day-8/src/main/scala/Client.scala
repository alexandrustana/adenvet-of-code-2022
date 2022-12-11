import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    val rows = input.length - 1
    val columns = input.head.length - 1

    def isVisible(
      row: Int,
      column: Int,
      curr: Int
    )(
      map: Map[(Int, Int), Int]
    ): Boolean =
      (row, column) match {
        case (0, _)         => true
        case (`rows`, _)    => true
        case (_, 0)         => true
        case (_, `columns`) => true
        case _ =>
          val isLeftSideVisible =
            ((row - 1) to 0 by (-1)).map { left => map(left -> column) < curr }.find(!_).getOrElse(true)
          val isRightSideVisible =
            ((row + 1) to rows by (+1)).map { right => map(right -> column) < curr }.find(!_).getOrElse(true)
          val isUpSideVisible = ((column - 1) to 0 by (-1)).map { up => map(row -> up) < curr }.find(!_).getOrElse(true)
          val isDownSideVisible =
            ((column + 1) to columns by (+1)).map { down => map(row -> down) < curr }.find(!_).getOrElse(true)

          isLeftSideVisible || isRightSideVisible || isUpSideVisible || isDownSideVisible
      }

    val map = (
      for {
        i <- 0 to rows
        j <- 0 to columns
      } yield (i -> j) -> input(i)(j).asDigit
    ).toMap

    map.count { case ((row, column), e) => isVisible(row, column, e)(map) }
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    val rows = input.length - 1
    val columns = input.head.length - 1

    def checkView(
      x: Int,
      y: Int,
      dx: Int,
      dy: Int,
      xLimit: Int,
      yLimit: Int
    )(
      curr: Int,
      map: Map[(Int, Int), Int]
    ): Int = {
      val newX = x + dx
      val newY = y + dy
      if (newX > xLimit || newY > yLimit || newX < 0 || newY < 0) 0
      else {
        if (map(newX -> newY) < curr) 1 + checkView(newX, newY, dx, dy, xLimit, yLimit)(curr, map)
        else 1
      }
    }

    def isVisible(
      row: Int,
      column: Int,
      curr: Int
    )(
      map: Map[(Int, Int), Int]
    ): Int =
      (row, column) match {
        case (0, _)         => 0
        case (`rows`, _)    => 0
        case (_, 0)         => 0
        case (_, `columns`) => 0
        case _ =>
          val isLeftSideVisible = checkView(row, column, -1, 0, rows, columns)(curr, map)
          val isRightSideVisible = checkView(row, column, +1, 0, rows, columns)(curr, map)
          val isUpSideVisible = checkView(row, column, 0, -1, rows, columns)(curr, map)
          val isDownSideVisible = checkView(row, column, 0, +1, rows, columns)(curr, map)

          isLeftSideVisible * isRightSideVisible * isUpSideVisible * isDownSideVisible
      }

    val map = (
      for {
        i <- 0 to rows
        j <- 0 to columns
      } yield (i -> j) -> input(i)(j).asDigit
    ).toMap

    map.map { case ((row, column), e) => isVisible(row, column, e)(map) }.max
  }
}
