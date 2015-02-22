package kamiya.parse

import kamiya.parse.expr._
import kamiya.parse.expr.ExprConverter._
import kamiya.parse.expr.CodeGenerator.ILAsmAssembler._

import kamiya.parse.brainfuck._
import kamiya.parse.brainfuck.BrainfuckParser._
import kamiya.util.ConsoleColor._
object Main {
  def main(args: Array[String]): Unit = {

    def bftest(code:String) = {
      println(code)
      val ast = ExprParser(code)
      println(ast)
      implicit val cs = new ConverterStatus
      val asm = ast.getOrElse(List.empty).convert

      println(asm)
      val bfasm = asm.toBf
      bfasm.foreach(println)
      val bfcode = bfasm.mkDbgCode("i")
      println(bfcode.magenta)

      val bp = new BrainfuckDebugger
      println("### Brainfuck Run ###".yellow)
      BrainfuckParser(bfcode).get.run(bp)
      println()
    }

    //bftest("print('@')")
    bftest("print(\"Hello World\")")
    //bftest("print('1' + 5 - 1)")
    //bftest("print(1 + 2 * 3 + 4 + 5 * 6 + 7 + 8 + 9 * 10)")
    /*
    bftest(
      """
        |string hoge = "Hello World!"
        |print(hoge)
      """.stripMargin)
*/
    bftest(
      """
        |int a = 1
        |int b = 3
        |string hoge = "Hello!"
        |print(a + b + 'a')
        |print(hoge)
      """.stripMargin)
    /*
    bftest(
      """
        |if(true) {
        |    print("True")
        |} else {
        |    print("False")
        |}
      """.stripMargin)
      */
  }
}