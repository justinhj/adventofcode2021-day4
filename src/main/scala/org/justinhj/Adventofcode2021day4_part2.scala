package org.justinhj

import zio.prelude.NonEmptyList

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Adventofcode2021day4_part2 extends App {

  import AOCUtil._

  val boardHeight = 5

  case class Square(value: Int, var marked: Boolean = false)

  case class Board(squares: Array[Array[Square]], var won: Boolean = false) {

    def checkRowWin(ri: Int): Boolean = {
      squares(ri).forall(_.marked)
    }

    def checkColWin(ci: Int): Boolean = {
      for(ri <- squares.indices) {
        if(!squares(ri)(ci).marked) return false
      }
      true
    }

    def checkWin(): Boolean = {
      for (ri <- squares.indices) {
        if(checkRowWin(ri)) {
          won = true
          return true
        }
      }

      for (ci <- squares(0).indices) {
        if(checkColWin(ci)) {
          won = true
          return true
        }
      }
     
      false  
    }

    def score(): Int = {
     if(checkWin()) {
       var sum = 0
       for (ri <- squares.indices)
         for (ci <- squares(0).indices) {
           if(!squares(ri)(ci).marked) sum += squares(ri)(ci).value
         }
       sum
     }
     else 0
    }

    def mark(ball: Int): Boolean = {
      for (ri <- squares.indices)
        for (ci <- squares(0).indices) {
          if(squares(ri)(ci).value == ball) {
            squares(ri)(ci).marked = true
            return true
          }
        }
      false  
    }
  }

  def parseInput(input: NonEmptyList[String]): (List[Int], List[Board]) = {
    val numbers = input.head.split(',').map(_.toInt)
    val boardInput = input.tail.iterator
    val boards = mutable.ListBuffer.empty[Board]
    while(boardInput.hasNext) {
      boardInput.next()
      val rows = mutable.ArrayBuffer.empty[Array[Square]]
      for (_ <- 1 to boardHeight) {
        val line = boardInput.next()
        val newNums = line.stripLeading().split("\\s+").toList.map(n => Square(n.toInt))
        rows += newNums.toArray
      }
      boards += Board(rows.toArray)
    }
    (numbers.toList, boards.result())
  }

  // For part 2 we are interested in the order of winners. Let's track the winners in a stack and
  // and return the last one. Note that we must not count boards multiple times, so I need a win flag
  def solve(numbers: List[Int], boards: List[Board]): Int = {
    val winners = mutable.Stack.empty[Int]
    numbers.zipWithIndex.foreach {
      case (ball,i) => 
        boards.foreach {
          board =>
            if(board.mark(ball)) {
              val alreadyWon = board.won
              val score = board.score()
              if(score > 0 && !alreadyWon) {
                winners += (score * ball)
              }
            }
        }
    }
    winners.head
  }

  val exampleInput = inputToStrings("example.txt") 
  val (numbers, boards) = parseInput(exampleInput)
  val exampleSolution = solve(numbers,boards)
  println(s"exampleSolution $exampleSolution")

  val part1Input = inputToStrings("day4.txt")
  val (numbers1, boards1) = parseInput(part1Input)
  val part1Solution = solve(numbers1,boards1)
  println(s"part1Solution $part1Solution")

}
