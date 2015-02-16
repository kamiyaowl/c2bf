/**
 * Created by kamiya on 2015/02/16.
 */
package kamiya.parse.brainfuck

import scala.util.parsing.combinator.RegexParsers

class BrainfuckProcessor {
  var pt : Int = 0
  var buf : Array[Char] = Array.fill[Char](3000)(0)

  def reset : Unit = { pt = 0; buf = Array.fill[Char](3000)(0) }
  def read : Char = buf(pt)
  def write(c: Char) = buf(pt) = c
  def update(f: Char => Char) = buf(pt) = f(buf(pt))
}
abstract class BrainfuckToken {
  def run(bp: BrainfuckProcessor): Unit
  implicit def toChar(src: Int): Char = src.asInstanceOf[Char]
}
case class Incr() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.update(_ + 1)
}
case class Decr() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.update(_ - 1)
}
case class Left() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.pt -= 1
}
case class Right() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.pt += 1
}
case class In() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.write(readChar)
}
case class Out() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = print(bp.read)
}
case class Loop(tokens: List[BrainfuckToken]) extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = while (bp.read != 0) {
    tokens.foreach(_.run(bp))
  }
}
object BrainfuckParser extends RegexParsers {
  implicit class BrainfuckTokens(val self: List[BrainfuckToken]) {
    def run(bp: BrainfuckProcessor) = self.foreach(_.run(bp))
  }
  def apply(src:String) : ParseResult[List[BrainfuckToken]] = parseAll(bf,src)

  def bf: Parser[List[BrainfuckToken]] = rep(loop | token)
  def token: Parser[BrainfuckToken] = ("+" | "-" | ">" | "<" | "," | ".") ^^ {
    case "+" => Incr()
    case "-" => Decr()
    case "<" => Left()
    case ">" => Right()
    case "," => In()
    case "." => Out()
  }
  def loop: Parser[Loop] = ("[" ~> bf) <~ "]" ^^ { r => Loop(r)}
}