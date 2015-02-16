package kamiya.parse

import kamiya.parse.expr._
import kamiya.parse.brainfuck._
import kamiya.parse.brainfuck.BrainfuckParser._

object Main {
  def main(args: Array[String]) = {
    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |repeat(i ; 1 -> 30) {
        |    if(i % 15 == 0) {
        |        print("fizzbuzz")
        |    } else if(i % 5 == 0) {
        |        print("buzz")
        |    } else if(i % 3 == 0) {
        |        print("fizz")
        |    } else {
        |        print(i)
        |    }
        |}
      """.stripMargin))

    val hello = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+."
    val bp = new BrainfuckProcessor
    BrainfuckParser(hello).get.run(bp)
  }
} 