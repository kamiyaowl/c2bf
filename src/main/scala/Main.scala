package kamiya.parse

object Main {
  def main(args: Array[String]) = {
    println(ExprParser.parseAll(ExprParser.statement, "123"))
    println(ExprParser.parseAll(ExprParser.statement, "123.456"))
    println(ExprParser.parseAll(ExprParser.statement, "deltaX"))
    println(ExprParser.parseAll(ExprParser.statement, "123"))
    println(ExprParser.parseAll(ExprParser.statement, "123.456"))
    println(ExprParser.parseAll(ExprParser.statement, "deltaX"))

    println(ExprParser.parseAll(ExprParser.statement, "deltaX * 100"))
    println(ExprParser.parseAll(ExprParser.statement, "deltaX * 100 / n"))
    println(ExprParser.parseAll(ExprParser.statement, "a * a + b * b"))
    println(ExprParser.parseAll(ExprParser.statement, "a + b + 10 * b"))
    println(ExprParser.parseAll(ExprParser.statement, "a + b + 10 * b == z"))
    println(ExprParser.parseAll(ExprParser.statement, "a < b + 10 * b == z > f"))
    println(ExprParser.parseAll(ExprParser.statement, "1 < 2 <= 3 == 4 + 5 != 6 > 7 > 8 + 9 * x"))
    println(ExprParser.parseAll(ExprParser.statement, "1 <= 2 + 3"))
    println(ExprParser.parseAll(ExprParser.statement, "1 == 2 && 3 < 1 * 5"))
    println(ExprParser.parseAll(ExprParser.statement, "true || false && true"))
    println(ExprParser.parseAll(ExprParser.statement, "1 < 2 || 3 || 4 > 5 && 6"))
    println(ExprParser.parseAll(ExprParser.statement, "true || False"))
    println(ExprParser.parseAll(ExprParser.statement, "False && True"))
    println(ExprParser.parseAll(ExprParser.statement, "x << 5"))
    println(ExprParser.parseAll(ExprParser.statement, "x << 5 >> 5"))
    println(ExprParser.parseAll(ExprParser.statement, "x << 5 >> 5 ^ y"))
    println(ExprParser.parseAll(ExprParser.statement, "x << 5 >> 5 ^ y == z & A"))
    println(ExprParser.parseAll(ExprParser.statement, "x << 5 >> 5 ^ y == z & A && B | z || false"))
    println(ExprParser.parseAll(ExprParser.statement, "0xfe"))
    println(ExprParser.parseAll(ExprParser.statement, "0x80"))
    println(ExprParser.parseAll(ExprParser.statement, "0b1010110"))
    println(ExprParser.parseAll(ExprParser.statement, "0b1010110 & 0b11110000"))
    println(ExprParser.parseAll(ExprParser.statement, "~0xff"))
    println(ExprParser.parseAll(ExprParser.statement, "!!!!!!!!!!true || ~~~~~~~~~~0xff"))
    println(ExprParser.parseAll(ExprParser.statement, "!true != false"))
    println(ExprParser.parseAll(ExprParser.statement, "hoge = false"))
    println(ExprParser.parseAll(ExprParser.statement, "foo => x * x"))
    println(ExprParser.parseAll(ExprParser.statement, "(a,b,c) => d = a + b < c == true"))
    println(ExprParser.parseAll(ExprParser.statement, "(a,b,c) => sin(a) + cos(b) + tan(c)"))
    println(ExprParser.parseAll(ExprParser.statement, "(x,y,r) => pow(x,2) + pow(y,2) <= pow(r,2)"))
    println(ExprParser.parseAll(ExprParser.statement, "x + y = 5"))
    println(ExprParser.parseAll(ExprParser.statement, "z * x + y = 5 * 3 + 2"))
    println(ExprParser.parseAll(ExprParser.statement, "z * (x + y) = 5 * 3 + 2"))
    println(ExprParser.parseAll(ExprParser.statement, "z * (x + y) = 5 * (3 + 2)"))
    println(ExprParser.parseAll(ExprParser.statement, "(x,y,z) => z * (x + y) = 5 * (3 + 2)"))
    println(ExprParser.parseAll(ExprParser.varDefState, "var func = (x,y,z) => z * (x + y) = 5 * (3 + 2)"))
    println(ExprParser.parseAll(ExprParser.varDefState, "int x = 5"))
    println(ExprParser.parseAll(ExprParser.varDefState, "int x = (hoge) => true"))
    println(ExprParser.parseAll(ExprParser.varDefState, "int x = sin(45)"))
    println(ExprParser.parseAll(ExprParser.varDefState, "int[] x = sin(45)"))
    println(ExprParser.parseAll(ExprParser.varDefState, "char c = hoge()"))
    println(ExprParser.parseAll(ExprParser.charNumber,"\'h\'"))
    println(ExprParser.parseAll(ExprParser.stringLiteral,"\"tako ika\""))
    println(ExprParser.parseAll(ExprParser.quoteExprLiteral,
      """
        |<[
        |    int x = 5
        |    int y = x * x +x == x >> x
        |    print(x,y)
        |]>
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |int x = 'r'
        |int y = 3
        |int z = x * (y + 1)
        |print('a')
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.ifState,
      """
        |if(x == 5) {
        |    x = "if statement"
        |    print(x)
        |}
      """.stripMargin))


    println(ExprParser.parseAll(ExprParser.ifState,
      """
        |if(x == 5) {
        |    x = "if statement"
        |    print(x)
        |} else if(x > 5) {
        |    print("else if statement")
        |} else if(x < 5) {
        |    print("else if statement 2")
        |} else {
        |    print("else statement")
        |}
      """.stripMargin))

    println(ExprParser.parseAll(ExprParser.forState,
      """
        |for(int x = 0  ; x < 5 && x > 0 ; ++x){
        |    print("hi" + x)
        |}
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.forState,
      """
        |for(int x = 0,int y = 0 ; x < 10 && y < 10 ; ++x,++y){
        |    print("hi" + x)
        |}
      """.stripMargin))

    println(ExprParser.parseAll(ExprParser.whileState,
      """
        |while(true) {
        |   print("a")
        |}
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |while(true) {
        |   if(true) {
        |       for(int i = 0 ; i < 10 ; ++i) {
        |           print("count = " + i)//comment out
        |       }
        |   }
        |}
      """.stripMargin))

    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |do {
        |    char c = read();//read from user
        |    print("input = " + c);
        |} while(c != 'q');
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |do {
        |    char c = read();//read from user
        |    repeat(10) {
        |        print("input = " + c);
        |    }
        |} while(c != 'q');
      """.stripMargin))

    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |repeat(i ; 30) {
        |    int j = i + 1
        |    if(j % 15 == 0) {
        |        print("fizzbuzz")
        |    } else if(j % 5 == 0) {
        |        print("buzz")
        |    } else if(j % 3 == 0) {
        |        print("fizz")
        |    } else {
        |        print(j)
        |    }
        |}
      """.stripMargin))
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

    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |int b = toInt(read())
        |int e = toInt(read())
        |repeat(i ; b -> e) {
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
    println(ExprParser.parseAll(ExprParser.comment,"// hogehoge".stripMargin))
    println(ExprParser.parseAll(ExprParser.comment,"/* hogehoge */".stripMargin))
    println(ExprParser.parseAll(ExprParser.comment,"/* hoge * hoge / hoge = hoge */".stripMargin))
    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |def fact = (n) => {
        |    if(n < 1) { 1 } else { fact(n-1) }
        |}
        |fact(10)
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.allCaseState,
      """
        |case('a') {
        |    print("A")
        |    break
        |}
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.allCaseState,
      """
        |default{
        |    break
        |}
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.allCaseState,
      """
        |case('a'){
        |   print("A")
        |}
      """.stripMargin))
    println(ExprParser.parseAll(ExprParser.allStatements,
      """
        |char c = input()
        |switch(c) {
        |    case('a'){
        |        print("A")
        |        break
        |    } case('b') {
        |        print("B")
        |        break
        |    } case ('c') {
        |        print("C")
        |        break
        |    } default {
        |        break
        |    }
        |}
      """.stripMargin))
  }
} 