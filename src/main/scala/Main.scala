package kamiya.parse

import kamiya.parse.expr._
import kamiya.parse.expr.ExprConverter._
import kamiya.parse.expr.CodeGenerator._
import kamiya.parse.brainfuck._
import kamiya.parse.brainfuck.BrainfuckParser._
import kamiya.util.ConsoleColor._
object Main {
  def main(args: Array[String]): Unit = {

    def bftest(code:String) = {
      val ast = ExprParser(code)
      println(ast)
      implicit val cs = new ConverterStatus
      val asm = ast.getOrElse(List.empty).convert

      println(asm)
      val bfasm = asm.toBf
      bfasm.foreach(println)
      val bfcode = bfasm.mkCode
      println(bfcode.magenta)

      val bp = new BrainfuckDebugger
      println("### Brainfuck Run ###".yellow)
      BrainfuckParser(bfcode).get.run(bp)
      println()
    }

    bftest("print('@')")
    bftest("print(\"Hello\")")
  }
}