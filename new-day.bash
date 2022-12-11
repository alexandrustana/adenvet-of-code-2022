mkdir $1
echo '.bsp/
.idea/
target/' > $1/.gitignore
mkdir -p $1/src/{main,test}/{scala,resources} $1/project
echo 'sbt.version=1.6.2' > $1/project/build.properties
echo 'name := "MyProject"
version := "0.1.0"
scalaVersion := "2.13.10"' > $1/build.sbt
echo 'import scala.io.Source

object Client extends App {
  println(part1())
  println(part2())

  private def part1() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()
    input
  }

  private def part2() = {
    val source = Source.fromFile("src/main/resources/input.txt")
    val input = source.getLines().toList
    source.close()
    input
  }
}
' > $1/src/main/scala/Client.scala
echo 'AOC' > $1/src/main/resources/input.txt
echo '
version = "3.4.3"
runner.dialect = scala213
align.preset = some
maxColumn = 120

align.tokens = [
  {code = "%", owner = "Term.ApplyInfix"},
  {code = "%%", owner = "Term.ApplyInfix"},
  {code = ":=", owner = "Term.ApplyInfix"},
  {code = "=>", owner = "Case"}
]

align.openParenCallSite = false

newlines.topLevelStatements = [before]
newlines.beforeMultiline = keep
newlines.topLevelStatementsMinBreaks = 1

optIn.breakChainOnFirstMethodDot = true

continuationIndent.defnSite = 2

verticalMultiline.atDefnSite = true
verticalMultiline.arityThreshold = 3
verticalMultiline.newlineAfterOpenParen = true

danglingParentheses.callSite = false
' > $1/.scalafmt.conf
