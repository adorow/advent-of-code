/*
--- Day 13: Transparent Origami ---
You reach another volcanically active part of the cave. It would be nice if you could do some kind of thermal imaging so you could tell ahead of time which caves are too hot to safely enter.

Fortunately, the submarine seems to be equipped with a thermal camera! When you activate it, you are greeted with:

Congratulations on your purchase! To activate this infrared thermal imaging
camera system, please enter the code found on page 1 of the manual.
Apparently, the Elves have never used this feature. To your surprise, you manage to find the manual; as you go to open it, page 1 falls out. It's a large sheet of transparent paper! The transparent paper is marked with random dots and includes instructions on how to fold it up (your puzzle input). For example:

6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
The first section is a list of dots on the transparent paper. 0,0 represents the top-left coordinate. The first value, x, increases to the right. The second value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0, and the coordinate 0,7 is below 0,0. The coordinates in this example form the following pattern, where # is a dot on the paper and . is an empty, unmarked position:

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
...........
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
Then, there is a list of fold instructions. Each instruction indicates a line on the transparent paper and wants you to fold the paper up (for horizontal y=... lines) or left (for vertical x=... lines). In this example, the first fold instruction is fold along y=7, which designates the line formed by all of the positions where y is 7 (marked here with -):

...#..#..#.
....#......
...........
#..........
...#....#.#
...........
...........
-----------
...........
...........
.#....#.##.
....#......
......#...#
#..........
#.#........
Because this is a horizontal line, fold the bottom half up. Some of the dots might end up overlapping after the fold is complete, but dots will never appear exactly on a fold line. The result of doing this fold looks like this:

#.##..#..#.
#...#......
......#...#
#...#......
.#.#..#.###
...........
...........
Now, only 17 dots are visible.

Notice, for example, the two dots in the bottom left corner before the transparent paper is folded; after the fold is complete, those dots appear in the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot just below them in the result (at 0,3) remains visible, as it can be seen through the transparent paper.

Also notice that some dots can end up overlapping; in this case, the dots merge together and become a single dot.

The second fold instruction is fold along x=5, which indicates this line:

#.##.|#..#.
#...#|.....
.....|#...#
#...#|.....
.#.#.|#.###
.....|.....
.....|.....
Because this is a vertical line, fold left:

#####
#...#
#...#
#...#
#####
.....
.....
The instructions made a square!

The transparent paper is pretty big, so for now, focus on just completing the first fold. After the first fold in the example above, 17 dots are visible - dots that end up overlapping after the fold is completed count as a single dot.

How many dots are visible after completing just the first fold instruction on your transparent paper?
 */

// run with: `scala ./2021/src/Day13_TransparentOrigami_Part1.scala`

import Axis.{Axis, X, Y}

import scala.io.Source
import scala.util.control.Breaks
import scala.util.control.Breaks.break

object Main {

  def main(args: Array[String]): Unit = {
    val Input(grid, foldInstructions) = readInput()

    println(foldInstructions)
    //grid.printGrid()
    println(s"Number of dots: ${grid.numberOfDots()}")

    val firstInstruction = foldInstructions.head
    println(s"Applying: ${firstInstruction}")
    println(s"Number of dots: ${grid.fold(firstInstruction).numberOfDots()}")
  }

  def readInput(): Input = {
    val filename = "./2021/resources/day13.in"
    // read grid
    val linesIterator = Source.fromFile(filename).getLines
    println("Reading grid:")

    val grid = new Grid(width = 1311, height = 895)

    var maxX, maxY = 0

    Breaks.breakable {
      for (line <- linesIterator) {
        println(line)
        if (line == "") break

        val Array(x, y) = line.split(",").map(str => str.toInt)

        maxX = math.max(x, maxX)
        maxY = math.max(y, maxY)

        grid.addDot(x, y)

      }
    }
    println("Reading fold instructions:")
    val listBuilder = List.newBuilder[FoldInstruction]
    for (line <- linesIterator) {
      val Array(strAxis, strPosition) = line.substring(11).split("=") // remove the "fold along " part, then separate the parameters
      println(line)

      listBuilder.addOne(FoldInstruction(Axis.fromString(strAxis), strPosition.toInt))
    }

    println(s"Max X: $maxX")
    println(s"Max Y: $maxY")

    Input(grid, listBuilder.result())
  }

}

case class Input(grid: Grid, instructions: List[FoldInstruction])

class Grid(val width: Int, val height: Int) {
  private val dot = '#'
  private val unmarked = '.'
  private val innerGrid: Array[Array[Char]] = Array.fill(height) { Array.fill(width) { unmarked } }

  def addDot(x: Int, y: Int): Unit =
    innerGrid(y)(x) = dot

  def hasDot(x: Int, y: Int): Boolean =
    innerGrid(y)(x) == dot

  def printGridDimensions(): Unit =
    println(s"Grid(w=$width, h=$height)")

  def printGrid(): Unit = {
    println("Grid: ")
    0 until height foreach { y =>
      0 until width foreach { x =>
        print(innerGrid(y)(x))
      }
      println()
    }
  }

  def numberOfDots(): Int =
    (0 until height map { y =>
      (0 until width map { x =>
        if (hasDot(x, y)) 1 else 0
      }).sum
    }).sum

  def fold(foldInstruction: FoldInstruction): Grid =
    foldInstruction match {
      case FoldInstruction(X, x) => foldOnX(x)
      case FoldInstruction(Y, y) => foldOnY(y)
    }

  def mirrorOnX(): Grid = {
    val newGrid = new Grid(width = width, height = height)
    0 until height foreach { y =>
      0 until width foreach { x =>
        val mirroredX = width - x - 1
        newGrid.innerGrid(y)(mirroredX) = this.innerGrid(y)(x)
      }
    }
    newGrid
  }

  private def overlapDots(otherGrid: Grid, width: Int = width, height: Int = height, startingX: Int = 0, startingY: Int = 0): Unit = {
    0 until height foreach { y =>
      0 until width foreach { x =>
        if (otherGrid.hasDot(startingX + x, startingY + y)) {
          this.addDot(x, y)
        }
      }
    }
  }

  def copySubGrid(startingX: Int, width: Int, startingY: Int, height: Int): Grid = {
    val newGrid = new Grid(width = width, height = height)
    newGrid.overlapDots(otherGrid = this, width = width, height = height, startingX = startingX, startingY = startingY)
    newGrid
  }

  private def foldOnX(x: Int): Grid = {
    val leftGrid = copySubGrid(startingX = 0, width = x, startingY = 0, height = height)
    val rightGrid = copySubGrid(startingX = x+1, width = x, startingY = 0, height = height)

    leftGrid.printGridDimensions()
    rightGrid.printGridDimensions()

    println(s"Number of dots on this: ${this.numberOfDots()}")
    println(s"Number of dots on left: ${leftGrid.numberOfDots()}")
    println(s"Number of dots on right: ${rightGrid.numberOfDots()}")

    leftGrid.overlapDots(otherGrid = rightGrid.mirrorOnX(), width = x, height = height)
    println(s"After overlap: ${leftGrid.numberOfDots()}")

    leftGrid
  }

  private def foldOnY(y: Int): Grid = {
    // not necessary in this task
    ???
  }

}

object Axis extends Enumeration {
  type Axis = Value
  val X, Y = Value

  def fromString(s: String): Axis = values.find(_.toString == s.toUpperCase()).get // simplifying here, usually would return Option[] or use getOrElse
}

case class FoldInstruction(axis: Axis, position: Int)

