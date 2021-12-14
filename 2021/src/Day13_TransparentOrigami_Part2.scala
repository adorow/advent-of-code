/*
--- Part Two ---
Finish folding the transparent paper according to the instructions. The manual says the code is always eight capital letters.

What code do you use to activate the infrared thermal imaging camera system?
 */

// run with: `scala ./2021/src/Day13_TransparentOrigami_Part1.scala`

import Axis.{Axis, X, Y}

import scala.io.Source
import scala.util.control.Breaks
import scala.util.control.Breaks.break

object Main {

  def main(args: Array[String]): Unit = {
    val Input(grid, foldInstructions) = readInput()

    println()

    var currentGrid = grid
    currentGrid.printGridDimensions()
    println(s"Grid has ${currentGrid.numberOfDots()} dots")

    var index = 0
    foldInstructions.toIndexedSeq foreach { foldInstruction =>
      index += 1

      println(s"Applying ${foldInstruction}")
      currentGrid = currentGrid.fold(foldInstruction)
      currentGrid.printGridDimensions()
      println(s"Grid has ${currentGrid.numberOfDots()} dots")
      if (currentGrid.height < 10) {
        currentGrid.printGrid()
      }

      println()
    }
  }

  def readInput(): Input = {
    val filename = "./2021/resources/day13.in"
    // read grid
    val linesIterator = Source.fromFile(filename).getLines
    //println("Reading grid:")

    val grid = new Grid(width = 1311, height = 895)

    var maxX, maxY = 0

    Breaks.breakable {
      for (line <- linesIterator) {
        //println(line)
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
      //println(line)

      listBuilder.addOne(FoldInstruction(Axis.fromString(strAxis), strPosition.toInt))
    }

    //println(s"Max X: $maxX")
    //println(s"Max Y: $maxY")

    Input(grid, listBuilder.result())
  }

}

case class Input(grid: Grid, instructions: List[FoldInstruction])

class Grid(val width: Int, val height: Int) {
  private val dot = '#'
  private val unmarked = '.'
  private val innerGrid: Array[Array[Char]] = Array.fill(height) {
    Array.fill(width) {
      unmarked
    }
  }

  def addDot(x: Int, y: Int): Unit =
    innerGrid(y)(x) = dot

  def hasDot(x: Int, y: Int): Boolean =
    innerGrid(y)(x) == dot

  def printGridDimensions(): Unit =
    println(s"Grid dimensions: (w=$width, h=$height), total cells=${width * height}")

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
      case FoldInstruction(X, x) => foldedOnX(x)
      case FoldInstruction(Y, y) => foldedOnY(y)
    }

  def mirroredOnX(): Grid = {
    val newGrid = new Grid(width = width, height = height)
    0 until height foreach { y =>
      0 until width foreach { x =>
        val mirroredX = width - x - 1
        newGrid.innerGrid(y)(mirroredX) = this.innerGrid(y)(x)
      }
    }
    newGrid
  }

  def mirroredOnY(): Grid = {
    val newGrid = new Grid(width = width, height = height)
    0 until height foreach { y =>
      0 until width foreach { x =>
        val mirroredY = height - y - 1
        newGrid.innerGrid(mirroredY)(x) = this.innerGrid(y)(x)
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

  def subGrid(startingX: Int = 0, width: Int = width, startingY: Int = 0, height: Int = height): Grid = {
    val newGrid = new Grid(width = width, height = height)
    newGrid.overlapDots(otherGrid = this, width = width, height = height, startingX = startingX, startingY = startingY)
    newGrid
  }

  private def foldedOnX(x: Int): Grid = {
    val leftGrid = subGrid(startingX = 0, width = x, startingY = 0, height = height)
    val rightGrid = subGrid(startingX = x + 1, width = x, startingY = 0, height = height)

    leftGrid.overlapDots(otherGrid = rightGrid.mirroredOnX(), width = x, height = height)

    leftGrid
  }

  private def foldedOnY(y: Int): Grid = {
      val topGrid = subGrid(startingX = 0, width = width, startingY = 0, height = y)
      val bottomGrid = subGrid(startingX = 0, width = width, startingY = y + 1, height = y)

      topGrid.overlapDots(otherGrid = bottomGrid.mirroredOnY(), width = width, height = y)

      topGrid
  }

}

object Axis extends Enumeration {
  type Axis = Value
  val X, Y = Value

  def fromString(s: String): Axis = values.find(_.toString == s.toUpperCase()).get // simplifying here, usually would return Option[] or use getOrElse
}

case class FoldInstruction(axis: Axis, position: Int) {
  override def toString: String = s"fold on $axis=$position"
}

