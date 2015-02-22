/**
 * Created by kamiya on 2015/02/16.
 */
package kamiya.parse.brainfuck

import scala.util.parsing.combinator.RegexParsers

class BrainfuckProcessor {
  val memMax : Int = 3000
  var pt : Int = 0
  var buf : Array[Char] = Array.fill[Char](memMax)(0)

  def reset : Unit = { pt = 0; buf = Array.fill[Char](memMax)(0) }
  def memRead : Char = buf(pt)
  def memWrite(c: Char) = buf(pt) = c
  def memUpdate(f: Char => Char) = buf(pt) = f(buf(pt))
  def ptUpdate(f: Int => Int) = pt = f(pt)
}
class BrainfuckDebugger extends BrainfuckProcessor {
  var debugEnable = false
  var skipCount = 0
  def stepRun = debugEnable = true
  def debugPrint(message:String) = {
    skipCount match {
      case 1 => { skipCount = 0 ; debugEnable = true}
      case x if x > 1 =>  skipCount -= 1
      case _ => Unit
    }
    if(debugEnable) memView(10,message)
  }
  override def memRead ={ debugPrint("memRead") ; super.memRead }
  override def memWrite(c:Char) ={  super.memWrite(c) ; debugPrint("after memWrite")  }
  override def memUpdate(f:Char => Char) ={ val r = super.memUpdate(f) ; debugPrint("after memUpdate")  ; r }
  override def ptUpdate(f:Int => Int) ={ val r = super.ptUpdate(f) ;  debugPrint("after ptUpdate"); r }

  def memView(implicit around:Int = 15, message:String = "debug memView", isStop:Boolean = true) : Unit = {
    def b = if(this.pt - around >= 0) this.pt - around else 0
    def e = if(this.pt + around <= this.memMax) this.pt + around else this.memMax

    println
    println(s"### $message ### pt = $pt")
    (b to (e - 1)).map("[" + _.toString.padTo(4,' ') + "]").foreach(print)
    println
    this.buf.slice(b,e).map(_.asInstanceOf[Int].toString.padTo(5,'_') + "|").foreach(print)
    println
    println("______" * this.pt + "^")
    this.buf.slice(b,e).map( _ +  ",").foreach(print)
    println
    if(isStop) {
      //optional tools
      readLine match {
        case "run" => {
          skipCount = 0; debugEnable = false
        }
        case "skip" => {
          skipCount = readLine.toInt; debugEnable = false
        }
        case _ => Unit
      }
    }
  }
  def memStrView(implicit around:Int = 10, message:String = "debug memStrView") : Unit = {
    def b = if(this.pt - around >= 0) this.pt - around else 0
    def e = if(this.pt + around <= this.memMax) this.pt + around else this.memMax

    println(s"$message : pt = $this.pt ($b -> $e)")
    this.buf.slice(b,e).map( _ +  ",").foreach(print)
    println("any input to next...")
    readLine
  }
}
abstract class BrainfuckToken {
  def run(bp: BrainfuckProcessor): Unit
  implicit def toChar(src: Int): Char = src.asInstanceOf[Char]
}
case class Incr() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.memUpdate(_ + 1)
}
case class Decr() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.memUpdate(_ - 1)
}
case class Left() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.ptUpdate(_ - 1)
}
case class Right() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.ptUpdate(_ + 1)
}
case class In() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = bp.memWrite(readChar)
}
case class Out() extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = print(bp.memRead)
}
case class Loop(tokens: List[BrainfuckToken]) extends BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit = while (bp.memRead != 0) {
    tokens.foreach(_.run(bp))
  }
}
abstract class BrainfuckDebugToken extends  BrainfuckToken {
  override def run(bp: BrainfuckProcessor): Unit =
    if(bp.isInstanceOf[BrainfuckDebugger]) this.debug(bp.asInstanceOf[BrainfuckDebugger])
  def debug(bd: BrainfuckDebugger): Unit
}
case class StepRun() extends BrainfuckDebugToken {
  override def debug(bd: BrainfuckDebugger) : Unit = bd.stepRun
}
case class MemView() extends BrainfuckDebugToken {
  override def debug(bd: BrainfuckDebugger) : Unit = bd.memView(isStop = false)
}
case class MemStrView() extends BrainfuckDebugToken {
  override def debug(bd: BrainfuckDebugger) : Unit = bd.memStrView
}

object BrainfuckParser extends RegexParsers {
  implicit class BrainfuckTokens(val self: List[BrainfuckToken]) {
    def run(bp: BrainfuckProcessor) = self.foreach(_.run(bp))
  }
  def apply(src:String) : ParseResult[List[BrainfuckToken]] = parseAll(bf,src)

  def bf: Parser[List[BrainfuckToken]] = rep(loop | token | debugToken)

  def debugToken: Parser[BrainfuckToken] = ("b" | "i" | "c" ) ^^ {
    case "b" => StepRun()
    case "i" => MemView()
    case "c" => MemStrView()
  }

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