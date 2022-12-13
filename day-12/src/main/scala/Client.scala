import scala.collection.mutable
import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    val rows = input.length
    val columns = input.head.length

    val map =
      (
        for {
          y <- 0 until rows
          x <- 0 until columns
        } yield (y -> x) -> input(y)(x)
      ).toMap

    val neighbours = List((1, 0), (-1, 0), (0, 1), (0, -1))

    def height(point: (Int, Int)): Char = map(point) match {
      case 'S' => 'a'
      case 'E' => 'z'
      case h   => h
    }
    def bfs(startingPoints: List[(Int, Int)]) = {
      val end = map.map(_.swap)('E')

      val cost = collection.mutable.Map(startingPoints.map(_ -> 0): _*)
      val toVisit = mutable.PriorityQueue.empty(Ordering.by(cost)).reverse
      toVisit.enqueue(startingPoints: _*)

      var visiting = toVisit.dequeue()
      while (!cost.contains(end)) {
        neighbours
          .map { case (ny, nx) => (ny + visiting._1) -> (nx + visiting._2) }
          .filter(map.contains)
          .filterNot(cost.contains)
          .filter(height(_) - height(visiting) <= 1)
          .foreach { next =>
            cost(next) = cost(visiting) + 1
            toVisit.enqueue(next)
          }
        visiting = toVisit.dequeue()
      }
      cost(end)
    }

    val start = map.map(_.swap)('S')

    bfs(List(start))
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    val rows = input.length
    val columns = input.head.length

    val map =
      (
        for {
          y <- 0 until rows
          x <- 0 until columns
        } yield (y -> x) -> input(y)(x)
      ).toMap

    val neighbours = List((1, 0), (-1, 0), (0, 1), (0, -1))

    def height(point: (Int, Int)): Char = map(point) match {
      case 'S' => 'a'
      case 'E' => 'z'
      case h   => h
    }
    def bfs(startingPoints: List[(Int, Int)]) = {
      val end = map.map(_.swap)('E')

      val cost = collection.mutable.Map(startingPoints.map(_ -> 0): _*)
      val toVisit = mutable.PriorityQueue.empty(Ordering.by(cost)).reverse
      toVisit.enqueue(startingPoints: _*)

      var visiting = toVisit.dequeue()
      while (!cost.contains(end)) {
        neighbours
          .map { case (ny, nx) => (ny + visiting._1) -> (nx + visiting._2) }
          .filter(map.contains)
          .filterNot(cost.contains)
          .filter(height(_) - height(visiting) <= 1)
          .foreach { next =>
            cost(next) = cost(visiting) + 1
            toVisit.enqueue(next)
          }
        visiting = toVisit.dequeue()
      }
      cost(end)
    }

    val lowPoints = map.filter(point => height(point._1) == 'a').keys.toList

    bfs(lowPoints)
  }
}
