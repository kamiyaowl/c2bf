
package kamiya.parse.expr
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by kamiya on 15/02/14.
 */
sealed class Expr

class Statement extends Expr

case class NoOperation() extends Statement

case class SwitchState(cmp:Literal, cases:List[CaseLabel]) extends Statement
class CaseLabel extends Statement
case class CaseState(label:Literal, exprs:List[Expr]) extends CaseLabel
case class DefaultState(exprs:List[Expr]) extends CaseLabel

case class RepeatState(n:Expr, exprs:List[Expr]) extends Statement//TODO: expr -> Literal?
case class VarRepeatState(variable:Variable, n:Expr, exprs:List[Expr]) extends Statement
case class RangeRepeatState(variable:Variable, s:Expr, n:Expr, exprs:List[Expr]) extends Statement

case class WhileState(cmp:Expr, exprs:List[Expr]) extends Statement
case class DoWhileState(cmp:Expr, exprs:List[Expr]) extends Statement

case class ForState(gen:List[Expr], cmp:Expr, adder:List[Expr],exprs:List[Expr]) extends Statement
case class IfState(ifstate:(Expr,List[Expr]), elseiflist:List[(Expr,List[Expr])], elsestate:Option[List[Expr]]) extends Expr
case class DefinitionVariable(typename:String, variable:Variable, statement:Expr) extends Statement
case class DefinitionArray(typename:String, variable:Variable, statement:Expr, size:Option[Int] = None) extends Statement

class Literal extends Expr
case class ConstNumber[+A](num: A) extends Literal
case class BoolLiteral(b: Boolean) extends Literal
case class StringLiteral(c:String) extends Literal
case class Variable(name: String) extends Literal//TODO: literal -> keyword
case class ExprList(args:List[Expr]) extends Literal
case class Func(a: String, args: ExprList) extends Literal
case class QueteExpr(exprs:List[Expr]) extends Literal


class BinaryOp(a: Expr, b: Expr) extends Expr
case class Assign(a: Expr, b: Expr) extends BinaryOp(a, b)
case class AssignBlockFunc(a: Expr, b: List[Expr]) extends Expr
case class AssignFunc(a: Expr, b: Expr) extends BinaryOp(a, b)

case class BitAnd(a: Expr, b: Expr) extends BinaryOp(a, b)
case class BitOr(a: Expr, b: Expr) extends BinaryOp(a, b)
case class BitXor(a: Expr, b: Expr) extends BinaryOp(a, b)
case class BitLeftShift(a: Expr, b: Expr) extends BinaryOp(a, b)
case class BitRightShift(a: Expr, b: Expr) extends BinaryOp(a, b)

case class LogicAnd(a: Expr, b: Expr) extends BinaryOp(a, b)
case class LogicOr(a: Expr, b: Expr) extends BinaryOp(a, b)

case class Over(a: Expr, b: Expr) extends BinaryOp(a, b)
case class AndOver(a: Expr, b: Expr) extends BinaryOp(a, b)
case class Equal(a: Expr, b: Expr) extends BinaryOp(a, b)
case class NotEqual(a: Expr, b: Expr) extends BinaryOp(a, b)
case class AndLess(a: Expr, b: Expr) extends BinaryOp(a, b)
case class Less(a: Expr, b: Expr) extends BinaryOp(a, b)

case class Add(a: Expr, b: Expr) extends BinaryOp(a, b)
case class Sub(a: Expr, b: Expr) extends BinaryOp(a, b)

case class Mul(a: Expr, b: Expr) extends BinaryOp(a, b)
case class Div(a: Expr, b: Expr) extends BinaryOp(a, b)
case class Rest(a: Expr, b: Expr) extends BinaryOp(a, b)

case class Indexer(a:Expr, b:Expr) extends  BinaryOp(a,b)

class UnaryOp(a: Expr) extends Expr
case class Not(a: Expr) extends UnaryOp(a)
case class BitInv(a: Expr) extends UnaryOp(a)
case class Increment(a: Expr) extends UnaryOp(a)
case class Decrement(a: Expr) extends UnaryOp(a)

object ExprParser extends RegexParsers {
  def intNumber = "[0-9]+".r ^^ { r => ConstNumber[Int](r.toInt)}
  def hexNumber = "0x[0-9a-fA-F]+".r ^^ {r => ConstNumber(Integer.parseInt(r.replace("0x",""),16)) }
  def binNumber = "0b[01]+".r ^^ {r => ConstNumber(Integer.parseInt(r.replace("0b",""),2)) }
  def doubleNumber = ("[0-9]+".r <~ ".") ~ "[0-9]+".r ^^ { case a ~ b => ConstNumber[Double](s"$a.$b".toDouble)}
  def charNumber = ("'" ~> ".{1}".r) <~ "'" ^^ {r => ConstNumber[Char](r.head) }
  def numberLiteral = hexNumber | binNumber | doubleNumber | intNumber | charNumber

  def booleanLiteral = ("[Tt]rue".r | "[Ff]alse".r) ^^ { r => BoolLiteral(r.toBoolean) }
  def stringLiteral = ("\"" ~> "(\\.|[^\"])*".r) <~ "\"" ^^ { r => StringLiteral(r) }
  def arrayLiteral = ("{" ~> repsep(statement,",")) <~ "}" ^^{ r => ExprList(r) }
  def quoteExprLiteral = ("<[" ~> allStatements) <~ "]>" ^^ { r => QueteExpr(r) }
  def variable = "[a-zA-Z0-9]+".r ^^ { r => Variable(r)}

  def args = noEmptyArgs | emptyArgs
  def emptyArgs = "(" ~ ")" ^^ { case a ~ b => ExprList(List.empty[Expr])}
  def noEmptyArgs = (("(" ~> statement) ~ rep("," ~> statement)) <~ ")" ^^ { case head ~ tail => ExprList(head :: tail) }

  def function : Parser[Literal] = "[a-zA-Z0-9]+".r ~ args ^^ { case name ~ argv => Func(name,argv) }
  def literal : Parser[Literal] = numberLiteral | stringLiteral | function | booleanLiteral | variable | arrayLiteral

  def assignTerm : Parser[String] = "+=" | "-=" | "*=" | "/=" | "*=" | "/=" | "<<=" | ">>=" | "&=" | "|=" | "^=" | "="
  def bitTerm : Parser[String] = "<<" | ">>" | "&" | "|" | "^"
  def logicTerm : Parser[String] = "&&" | "||"
  def compTerm : Parser[String] = ">=" | ">" | "==" | "!=" | "<=" | "<"
  def exprTerm : Parser[String] = "+" | "-"//TODO: priority "+=" > "+"
  def opTerm: Parser[String] = "*" | "/" | "%"
  def singleTerm : Parser[String] = "!" | "~" | "++" | "--"

  //master
  def apply(src:String) = parseAll(allStatements,src)
  def allStatements : Parser[List[Expr]] = rep(allStatement)//TODO: first comment (rep(comment) ^^ {r => NoOperation()}) |
  def allStatement : Parser[Expr] =
    (switchState | rangeRepeatState | varRepeatState  | repeatState | doWhileState | whileState | forState | ifState | arrDefState | varDefState | statement) <~ rep(";" | comment)
  def comment : Parser[String]= ("//" ~> ".*".r) | (("/*" ~> "^((?!\\*/).)*".r) <~ "*/")
  ////////////////////////////////////////////////////////////////////////////////////
  // extend statements
  def switchState : Parser[SwitchState] =
    (((("switch" ~ "(") ~> literal) <~ ")") <~ "{") ~ rep(allCaseState) <~ "}" ^^ {
      case l ~ cs => SwitchState(l,cs)
    }
  def allCaseState : Parser[CaseLabel] = defaultState | caseState
  def defaultState : Parser[DefaultState] = ("default" ~ "{" ) ~> allStatements <~ "}" ^^ { r => DefaultState(r) }
  def caseState : Parser[CaseState] = (((("case" ~ "(" )~> literal) <~ ")") <~ "{") ~ allStatements <~ "}" ^^ {
    case c ~ ex => CaseState(c,ex)
  }
  def rangeRepeatState : Parser[RangeRepeatState] = (((("repeat" ~ "(") ~> ((variable <~ ";") ~ literal <~ "->") ~ literal)) <~ ")" <~ "{") ~ allStatements <~ "}" ^^ {
    case v ~ s ~ n ~ es => RangeRepeatState(v,s,n,es)
  }
  def varRepeatState : Parser[VarRepeatState] = ((("repeat" ~ "(") ~> (variable <~ ";") ~ literal) <~ ")" <~ "{") ~ allStatements <~ "}" ^^ {
    case v ~ n ~ es => VarRepeatState(v,n,es)
  }
  def repeatState : Parser[RepeatState] = ((("repeat" ~ "(") ~> literal) <~ ")" <~ "{") ~ allStatements <~ "}" ^^ {
    case n ~ es => RepeatState(n,es)
  }

  def doWhileState : Parser[DoWhileState] = ((("do" ~ "{") ~> allStatements) <~ "}") ~ (("while" ~ "(") ~> statement) <~ ")"^^ {
    case es ~ c => DoWhileState(c,es)
  }
  def whileState : Parser[WhileState] = ((("while" ~ "(") ~> statement) <~ ")" <~ "{") ~ allStatements <~ "}" ^^ {
    case c ~ es => WhileState(c,es)
  }
  def forState : Parser[ForState] =
    (((("for" ~ "(") ~> repsep(repsep(varDefState | statement,","), ";")) <~ ")" <~ "{") ~ allStatements) <~ "}" ^^ {
      case List(g, c :: Nil, a) ~ ss => ForState(g,c,a,ss)
    }

  def ifState : Parser[IfState] = implIfState ~ rep(implElseIfState) ~ implElseState.? ^^ {
    case i ~ es ~ e => IfState(i,es,e)
  }
  def implIfState : Parser[(Expr, List[Expr])] = (((("if" ~ "(") ~> statement) <~ ")" <~ "{") ~ allStatements) <~ "}" ^^ {
    case bl ~ sts => (bl, sts)
  }
  def implElseIfState : Parser[(Expr, List[Expr])] = "else" ~> implIfState
  def implElseState : Parser[List[Expr]] = (("else" ~ "{") ~> allStatements) <~ "}"

  ////////////////////////////////////////////////////////////////////////////////////
  // base statements

  def varDefState : Parser[DefinitionVariable] = (("[a-zA-Z0-9\\[\\]]+".r ~ (variable)) <~ "=") ~ statement ^^ {
    case t ~ v ~ s => DefinitionVariable(t,v,s)
  }
  def arrDefState : Parser[DefinitionArray] = (("[a-zA-Z0-9\\[\\]]+".r ~ (indexer)) <~ "=") ~ statement ^^ {
    case t ~ (v:Variable) ~ s => DefinitionArray(t,v,s,None)
    case t ~ Indexer(v:Variable,ConstNumber(i:Int)) ~ s => DefinitionArray(t,v,s,Some(i))
  }
  def statement : Parser[Expr] = assignBlockFunc | assignFunc | brackets | assign


  def assignFunc : Parser[AssignFunc] = ((args | variable) <~ "=>") ~ assign ^^ {
    case a ~ body => AssignFunc(a,body)
  }
  def assignBlockFunc : Parser[AssignBlockFunc] = (((args | variable) <~ "=>") <~ "{") ~  allStatements  <~ "}" ^^ {
    case a ~ body => AssignBlockFunc(a,body)
  }
  def brackets : Parser[Expr] = ("(" ~> assign ) <~ ")"
  def assign : Parser[Expr] = bit ~ rep("=" ~ bit) ^^ {
    case n ~ body => (n /: body){ (s,x) => { x._1 match {
      case "+=" => Assign(s,Add(s,x._2))//TODO: first matched '+'
      case "=" => Assign(s,x._2)
    }}}
  }

  ////////////////////////////////////////////////////////////////////////////////////
  // base arithmetic statements
  def bit : Parser[Expr] = logic ~ rep(bitTerm ~ logic) ^^ {
    case n ~ body => (n /: body){ (s,x) => { x._1 match {
      case "<<" => BitLeftShift(s,x._2)
      case ">>" => BitRightShift(s,x._2)
      case "&" => BitAnd(s,x._2)
      case "|" => BitOr(s,x._2)
      case "^" => BitXor(s,x._2)
    }}}
  }
  def logic : Parser[Expr] = comp ~ rep(logicTerm ~ comp) ^^ {
    case n ~ body => (n /: body){ (s,x) => { x._1 match {
      case "&&" => LogicAnd(s,x._2)
      case "||" => LogicOr(s,x._2)
    }}}
  }
  def comp : Parser[Expr] = expr ~ rep(compTerm ~ expr) ^^ {
    case n ~ body => (n /: body){ (s,x) => { x._1 match {
      case ">" => Over(s, x._2)
      case ">=" => AndOver(s, x._2)
      case "==" => Equal(s,x._2)
      case "!=" => NotEqual(s,x._2)
      case "<=" => AndLess(s,x._2)
      case "<" => Less(s,x._2)
    }}}
  }
  def expr : Parser[Expr] = term ~ rep(exprTerm ~ term) ^^ {
    case n ~ body => (n /: body){ (s,x) => { x._1 match {
      case "+" => Add(s, x._2)
      case "-" => Sub(s, x._2)
    }}}
  }
  def term: Parser[Expr] = unary ~ rep(opTerm ~ unary) ^^ {
    case n ~ body => ((n.asInstanceOf[Expr]) /: body){ (s,x) => { x._1 match {
      case "*" => Mul(s, x._2)
      case "/" => Div(s, x._2)
      case "%" => Rest(s,x._2)
    }}}
  }

  def unary = indexer | single | literal | brackets//brackets is recursive statement
  def indexer : Parser[Expr] = (literal <~ "[") ~ statement.?  <~ "]" ^^ {
      case a~Some(b) => Indexer(a,b)
      case a~_ => a match {
        case Variable(a) => Variable(a)// + "[]")//typename of array
        case _ => throw new IllegalArgumentException
      }
    }
  def single: Parser[Expr] = singleTerm ~ unary ^^ {
    case ("!" ~ literal) => Not(literal)
    case ("~" ~ literal) => BitInv(literal)
    case ("++" ~ literal) => Increment(literal)
    case ("--" ~ literal) => Decrement(literal)
  }
}

