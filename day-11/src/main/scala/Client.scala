import scala.annotation.tailrec
import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    case class Monkey(
      items: mutable.Queue[Long],
      operation: Long => Long,
      test: Long => Boolean,
      ifTrue: Int,
      ifFalse: Int,
      var itemsCounter: Long)

    @tailrec
    def readInput(input: Iterator[String], monkeys: TreeMap[Int, Monkey] = TreeMap.empty): Map[Int, Monkey] = {
      var line = input.next()
      while (!line.startsWith("Monkey")) {
        line = input.next()
      }

      val index = line match {
        case s"Monkey $n:" => n.toInt
      }
      val items = input.next.trim match {
        case s"Starting items: $n" => n.split(",").map(_.trim).map(_.toLong)
      }
      val operation = input.next.trim match {
        case s"Operation: new = old * old" => (item: Long) => item * item
        case s"Operation: new = old + old" => (item: Long) => item + item
        case s"Operation: new = old * $n"  => (item: Long) => item * (n.toLong)
        case s"Operation: new = old + $n"  => (item: Long) => item + (n.toLong)
      }
      val test = input.next.trim match {
        case s"Test: divisible by $n" => (item: Long) => item % (n.toLong) == 0
      }
      val ifTrue = input.next.trim match {
        case s"If true: throw to monkey $n" => n.toInt
      }
      val ifFalse = input.next.trim match {
        case s"If false: throw to monkey $n" => n.toInt
      }
      val monkey = index -> Monkey(mutable.Queue.from(items), operation, test, ifTrue, ifFalse, 0)
      val newGang = monkeys + monkey
      if (input.hasNext) readInput(input, newGang)
      else newGang
    }

    val monkeys = readInput(input.iterator)

    (1 to 20).foreach { _ =>
      monkeys.foreach { case (id, monkey) =>
        monkey.items.dequeueAll { item =>
          val worry = monkey.operation(item)
          val relief = worry / 3
          val test = monkey.test(relief)
          val nextMonkeyId = if (test) monkey.ifTrue else monkey.ifFalse
          monkeys(nextMonkeyId).items.enqueue(relief)
          monkey.itemsCounter = monkey.itemsCounter + 1
          true
        }
      }
    }

    monkeys.values.toList.map(_.itemsCounter).sorted.reverse.take(2).product
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    case class Monkey(
      items: mutable.Queue[Long],
      operation: Long => Long,
      test: Long => Boolean,
      mod: Long,
      ifTrue: Int,
      ifFalse: Int,
      var itemsCounter: Long)

    @tailrec
    def readInput(input: Iterator[String], monkeys: TreeMap[Int, Monkey] = TreeMap.empty): Map[Int, Monkey] = {
      var line = input.next()
      while (!line.startsWith("Monkey")) {
        line = input.next()
      }

      val index = line match {
        case s"Monkey $n:" => n.toInt
      }
      val items = input.next.trim match {
        case s"Starting items: $n" => n.split(",").map(_.trim).map(_.toLong)
      }
      val operation = input.next.trim match {
        case s"Operation: new = old * old" => (item: Long) => item * item
        case s"Operation: new = old + old" => (item: Long) => item + item
        case s"Operation: new = old * $n"  => (item: Long) => item * (n.toLong)
        case s"Operation: new = old + $n"  => (item: Long) => item + (n.toLong)
      }
      val (mod, test) = input.next.trim match {
        case s"Test: divisible by $n" => { n.toLong -> ((item: Long) => item % (n.toLong) == 0) }
      }
      val ifTrue = input.next.trim match {
        case s"If true: throw to monkey $n" => n.toInt
      }
      val ifFalse = input.next.trim match {
        case s"If false: throw to monkey $n" => n.toInt
      }
      val monkey = index -> Monkey(mutable.Queue.from(items), operation, test, mod, ifTrue, ifFalse, 0)
      val newGang = monkeys + monkey
      if (input.hasNext) readInput(input, newGang)
      else newGang
    }

    val monkeys = readInput(input.iterator)
    val commonDivisor = monkeys.map(_._2.mod).product

    (1 to 10000).foreach { _ =>
      monkeys.foreach { case (_, monkey) =>
        monkey.items.dequeueAll { item =>
          val worry = monkey.operation(item)
          val relief = worry % commonDivisor
          val test = monkey.test(relief)
          val nextMonkeyId = if (test) monkey.ifTrue else monkey.ifFalse
          monkeys(nextMonkeyId).items.enqueue(relief)
          monkey.itemsCounter = monkey.itemsCounter + 1
          true
        }
      }
    }
    val x = monkeys.map { case (id, monkey) => s"Monkey $id: ${monkey.items.dequeueAll(_ => true)}" }.mkString("\n")
    val y = monkeys.toList
      .sortBy(_._2.itemsCounter)
      .reverse
      .map { case (id, monkey) => s"Monkey $id inspected items ${monkey.itemsCounter} times." }
      .mkString("\n")

//    println(x)
    println(y)
    monkeys.values.toList.map(_.itemsCounter).sorted.reverse.take(2).product
  }
}
