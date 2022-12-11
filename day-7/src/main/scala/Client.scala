import scala.annotation.tailrec
import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  sealed trait FS {
    def name: String
    def parent: Option[Dir]
    def size: Long
  }

  case class Dir(
    parent: Option[Dir],
    var nodes: List[FS],
    name: String)
      extends FS {
    override def size: Long = nodes.map(node => node.size).sum
    override def toString: String = s"Dir($name)"
  }

  case class File(
    parent: Option[Dir],
    size: Long,
    name: String)
      extends FS {
    override def toString: String = s"File($name, $size)"
  }

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    @tailrec
    def changeDirectory(input: List[String], curr: Dir): FS = input match {
      case ::(head, next) => head match {
          case "$ cd .."      => changeDirectory(next, curr.parent.get)
          case s"$$ cd $name" => changeDirectory(next, curr.nodes.find(_.name == name).get.asInstanceOf[Dir])
          case "$ ls"         => list(next, curr)
        }
      case Nil => curr
    }
    @tailrec
    def list(input: List[String], curr: Dir): FS = input match {
      case ::(head, next) => head match {
          case s"$$ cd $_" =>
            changeDirectory(input, curr)
          case s"dir $name" =>
            list(next, { curr.nodes = Dir(Some(curr), List.empty, name) :: curr.nodes; curr })
          case s"$size $name" =>
            list(next, { curr.nodes = File(Some(curr), size.toLong, name) :: curr.nodes; curr })
        }
      case Nil => curr
    }

    def getSize(
      root: FS,
      limit: Long,
      acc: List[Long]
    ): List[Long] = {
      root match {
        case dir: Dir =>
          val size = dir.size
          val newAcc = if (size < limit) size :: acc else acc
          val nodesAccs = dir.nodes.flatMap(node => getSize(node, limit, List.empty))
          newAcc ::: nodesAccs
        case _ => acc
      }
    }

    val root = Dir(None, List.empty, "/")
    changeDirectory(input.tail, root)
    getSize(root, 100000, List.empty).sum
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()

    @tailrec
    def changeDirectory(input: List[String], curr: Dir): FS = input match {
      case ::(head, next) => head match {
          case "$ cd .."      => changeDirectory(next, curr.parent.get)
          case s"$$ cd $name" => changeDirectory(next, curr.nodes.find(_.name == name).get.asInstanceOf[Dir])
          case "$ ls"         => list(next, curr)
        }
      case Nil => curr
    }

    @tailrec
    def list(input: List[String], curr: Dir): FS = input match {
      case ::(head, next) => head match {
          case s"$$ cd $_" =>
            changeDirectory(input, curr)
          case s"dir $name" =>
            list(
              next, {
                curr.nodes = Dir(Some(curr), List.empty, name) :: curr.nodes; curr
              })
          case s"$size $name" =>
            list(
              next, {
                curr.nodes = File(Some(curr), size.toLong, name) :: curr.nodes; curr
              })
        }
      case Nil => curr
    }

    def getSize(
      root: FS,
      limit: Long,
      acc: List[Long]
    ): List[Long] = {
      root match {
        case dir: Dir =>
          val size = dir.size
          val newAcc = if (size < limit) size :: acc else acc
          val nodesAccs = dir.nodes.flatMap(node => getSize(node, limit, List.empty))
          newAcc ::: nodesAccs
        case _ => acc
      }
    }

    val root = Dir(None, List.empty, "/")
    changeDirectory(input.tail, root)
    val limit = 70000000
    val min = 30000000
    val diff = min - (limit - root.size)
    getSize(root, limit, List.empty).sorted.dropWhile(_ < diff).head
  }
}
