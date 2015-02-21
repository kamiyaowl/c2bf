package kamiya.parse

import kamiya.parse.expr._
import kamiya.parse.expr.ExprConverter._
import kamiya.parse.brainfuck._
import kamiya.parse.brainfuck.BrainfuckParser._

object Main {
  def main(args: Array[String]): Unit = {
    println(ExprParser(
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

    val hello = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>b.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+.>>>>>>"
    val bp = new BrainfuckDebugger
    //BrainfuckParser(hello).get.run(bp)

    implicit val cs = new ConverterStatus
    println(ExprParser("true").get.convert)
    println(ExprParser("100").get.convert)
    println(ExprParser("0xfe").get.convert)
    println(ExprParser("'@'").get.convert)
    println(ExprParser("\"hogehoge\"").get.convert)
    println(ExprParser("int x = 5").get.convert)
    println(ExprParser("bool z = false").get.convert)
    println(ExprParser("string z = \"can it parse?\"").get.convert)
    println(ExprParser("++x").get.convert)
    println(ExprParser("--y").get.convert)
    println(ExprParser("!y").get.convert)
    println(ExprParser("!!!true").get.convert)
    println(ExprParser("~~~x").get.convert)
    println(ExprParser("1 + 2").get.convert)
    println(ExprParser("int x = 1 + 2").get.convert)
    println(ExprParser("int x = 1 + 2 * y").get.convert)
    println(ExprParser("int x = 1 / 1 + 2 * y").get.convert)
    println(ExprParser("int x = 1 / 1 + 2 * y % 8").get.convert)
    println(ExprParser("1 < 2").get.convert)
    println(ExprParser("1 <= 2").get.convert)
    println(ExprParser("1 == 2").get.convert)
    println(ExprParser("1 != 2").get.convert)
    println(ExprParser("1 >= 2").get.convert)
    println(ExprParser("1 > x").get.convert)
    println(ExprParser("true || x").get.convert)
    println(ExprParser("y && x && z").get.convert)
    println(ExprParser("x << 1").get.convert)
    println(ExprParser("x >> 1").get.convert)
    println(ExprParser("x ^ 0x55").get.convert)
    println(ExprParser("x | 0xf0").get.convert)
    println(ExprParser("x & 0x0f").get.convert)
    println(ExprParser("print(x)").get.convert)
    println(ExprParser("print(\"hello\")").get.convert)
    println(ExprParser("print(\"Hello\" + x)").get.convert)
    println(ExprParser("print(add(1,mul(5,3),3))").get.convert)
    println(ExprParser(
      """
        |int x = 5
        |if(x < 5) {
        |    print("<")
        |} else if(x > 5) {
        |    print(">")
        |} else {
        |    print("eq")
        |}
      """.stripMargin).get.convert)
    println(ExprParser(
      """
        |int x = 5
        |if(x < 5) {
        |    print("<")
        |} else if(x > 5) {
        |    print(">")
        |}
      """.stripMargin).get.convert)
    println(ExprParser(
      """
        |int x = 5
        |if(x < 5) {
        |    print("<")
        |} else  {
        |    print("eq")
        |}
      """.stripMargin).get.convert)

    println(ExprParser(
      """
        |int x = 5
        |if(x < 5) {
        |    print("<")
        |}
      """.
        stripMargin).get.convert)
    println(ExprParser(
      """
        |while(true) {
        |    print("A")
        |}
      """.
        stripMargin).get.convert)
    println(ExprParser(
      """
        |for(int i = 1 ; i < 30 ; ++i) {
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
      """.stripMargin).get.convert)

    println(ExprParser(
      """
        |string x = ""
        |switch(3) {
        |   case(0) {
        |      x = "zero"
        |   }
        |   case (2) {
        |       x = "two"
        |   }
        |   default {
        |       x = "default"
        |   }
        |}
        |print(x)
      """.stripMargin).get.convert)
    println(ExprParser(
      """
        |int hoge = 100
        |switch(hoge) {
        |   case(10) {
        |      x = "zero"
        |   }
        |   case (20) {
        |       x = "two"
        |   }
        |}
        |print(x)
      """.stripMargin).get.convert)
    println(ExprParser(
      """
        |if(true) {
        |    if(true){
        |        if(true) {
        |            print("nested if")
        |        } else {
        |            print("nested else")
        |        }
        |    }
        |} else {
        |    print("root else")
        |}
      """.stripMargin).get.convert)
    println(ExprParser(
      """
        |x[5] = 100
      """.stripMargin).get.convert)
    println(ExprParser(
      """
        |int x[10] = {1,2,3,4,5,6,7,8,9,10}
        |x[5] = 100
      """.stripMargin).get.convert)
    println(ExprParser(
      """
        |int x[10] = {1,
        | 2 / 1,
        | 3 * 3 + 1,
        | 4 > 1,
        | 5 == 1,
        | false,
        | hoge,
        | foo,
        | bar
        |}
        |x[5] = 100
      """.stripMargin).get.convert)
    println(ExprParser(
      """
        |int x[1] = {}
      """.stripMargin).get.convert)
    //NOT SUPPORT
    println(ExprParser(
      """
        |int x[] = {{1},{2,3},{{4,5},{6}}}
      """.stripMargin).get.convert)

    println(ExprParser("int x = 5").get.convert)
    println(ExprParser(
      """
        |int n = 100
        |int arr[100] = {0,0}
        |for(int i = 2 ; i < n ; ++i) {
        |    arr[i] = 1
        |}
        |for(int i = 2 ; i < n ; ++i) {
        |    for(int j = i + i ; arr[i] == 1 && j < n; j = j + i) {
        |      arr[j] = 0
        |    }
        |}
        |for(int i = 2 ; i < n ; ++i){
        |    if(arr[i] == 1) {
        |        print(i)
        |    }
        |}
      """.stripMargin).get.convert)
  }
}