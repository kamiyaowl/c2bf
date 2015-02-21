/**
 * Created by kamiya on 2015/02/16.
 */
package kamiya.parse.expr

object ExprConverter {

  abstract class ILAsm
  case class Load[A](num: A) extends ILAsm
  case class Store[A](num: A) extends ILAsm
  case class LoadLocal(id: String,var typeName:String = "") extends ILAsm
  case class StoreLocal(id: String,var typeName:String = "") extends ILAsm

  case class Call(id: String,var typeName:String = "") extends ILAsm

  abstract class ILAsmOp extends ILAsm
  case class AddOp() extends ILAsmOp
  case class SubOp() extends ILAsmOp
  case class MulOp() extends ILAsmOp
  case class DivOp() extends ILAsmOp
  case class RestOp() extends ILAsmOp
  case class InvOp() extends ILAsmOp
  case class NotOp() extends ILAsmOp

  case class LessOp() extends ILAsmOp
  case class AndLessOp() extends ILAsmOp
  case class NotEqualOp() extends ILAsmOp
  case class EqualOp() extends ILAsmOp
  case class AndOverOp() extends ILAsmOp
  case class OverOp() extends ILAsmOp
  case class LogicOrOp() extends ILAsmOp
  case class LogicAndOp() extends ILAsmOp

  case class BitRightShiftOp() extends ILAsmOp
  case class BitLeftShiftOp() extends ILAsmOp
  case class BitXorOp() extends ILAsmOp
  case class BitOrOp() extends ILAsmOp
  case class BitAndOp() extends ILAsmOp

  abstract class ILAsmControl extends ILAsm

  case class Jump(label:String) extends ILAsmControl
  case class Branch(label:String) extends ILAsmControl//TODO:Label同様に分類が必要？

  class EmbeddedTag(label:String) extends  ILAsmControl

  case class Label(label:String) extends EmbeddedTag(label)

  class LoopLabel(label:String) extends  EmbeddedTag(label)
  case class LoopBeginLabel(label:String) extends  LoopLabel(label)
  case class LoopEndLabel(label:String) extends  LoopLabel(label)

  class SwitchCaseLabel(label:String) extends EmbeddedTag(label)
  case class SwitchEndLabel(label:String) extends  SwitchCaseLabel(label)
  case class CaseLabel(label:String) extends  SwitchCaseLabel(label)

  class IfLabel(label:String) extends  EmbeddedTag(label)
  case class IfStateBegin(label:String) extends  IfLabel(label)
  case class IfBegin(label:String) extends IfLabel(label)
  case class IfEnd(label:String) extends IfLabel(label)
  case class ElseIfBegin(label:String) extends IfLabel(label)
  case class ElseIfEnd(label:String) extends IfLabel(label)
  case class ElseBegin(label:String) extends IfLabel(label)
  case class ElseEnd(label:String) extends IfLabel(label)
  case class IfStateEnd(label:String) extends  IfLabel(label)

  case class IfThroughFlag(label:String) extends IfLabel(label)//IfStateEndでリセット

  implicit class ExprsConvert(val self: List[Expr]) {
    def convert(implicit cs:ConverterStatus) : List[ILAsm] = self flatMap(_.convert)
  }
  object StaticTools {
    private var counter = 0
    def getCount : Int = { counter += 1; counter }
    def anonymousVariable = "___Anonymous_Variable_" + getCount + "___"
    def anonymousTag = "___Anonymous_Tag_" + getCount + "___"
  }
  //副作用のあるゴミ
  class ConverterStatus {
    private var counter = 0
    def getCount : Int = { counter += 1; counter }
    def anonymousVariable = "___Anonymous_Variable_" + getCount + "___"
    def anonymousTag = "___Anonymous_Tag_" + getCount + "___"
  }
  implicit class ExprConvert(val self: Expr) {
    private def binOp(left:Expr,asm:ILAsmOp, right:Expr)(implicit cs:ConverterStatus) = left.convert ++ right.convert ++ List(asm)

    def convert(implicit cs:ConverterStatus) : List[ILAsm] = self match {
      case Definition(t,Variable(name), right) => right.convert ++ List(StoreLocal(name,t))

      //TODO: impl Repeat
      case SwitchState(cmp,cases) =>{
        val anoVar = cs.anonymousVariable
        val endTag = cs.anonymousTag

        val start = cmp.convert ++ List(StoreLocal(anoVar,"unknown"))
        val casesAsm : List[(List[ILAsm],List[ILAsm])] = cases.filter{
          case CaseState(_,_) => true
          case _ => false
        }.map { case CaseState(l, exprs) => {
            val label = cs.anonymousTag
            (List(LoadLocal(anoVar,"unknown")) ++ l.convert ++ List[ILAsm](EqualOp(), Branch(label)), List[ILAsm](CaseLabel(label)) ++ exprs.convert ++ List[ILAsm](Jump(endTag)))
          }
        }
        val defaultAsm : List[ILAsm] = cases.find{
          case DefaultState(_) => true
          case _ => false
        } match {
          case Some(DefaultState(expr)) => expr.convert ++ List(Jump(endTag))
          case _ => List(Jump(endTag))
        }
        start ++ casesAsm.flatMap(_._1) ++ defaultAsm ++ casesAsm.flatMap(_._2) ++ List[ILAsm](SwitchEndLabel(endTag))
      }
      case ForState(gen,cmp,addr,exprs) => {
        val startTag = cs.anonymousTag
        val endTag = cs.anonymousTag
        gen.convert ++ List(LoopBeginLabel(startTag)) ++ cmp.convert ++ List(NotOp(), Branch(endTag)) ++ exprs.convert ++ addr.convert ++ List(Jump(startTag), LoopEndLabel(endTag))
      }
      case WhileState(cmp,exprs) => {
        val startTag = cs.anonymousTag
        val endTag = cs.anonymousTag
        List(LoopBeginLabel(startTag)) ++ cmp.convert ++ List(NotOp(), Branch(endTag)) ++ exprs.convert ++ List(Jump(startTag), LoopEndLabel(endTag))
      }
      case IfState((cmp1,proc1),elses,els) => {
        val iftag = cs.anonymousTag
        val ifbegin = cs.anonymousTag
        val ifEnd = cs.anonymousTag

        val ifasm = List(IfBegin(ifbegin)) ++ cmp1.convert ++ List(NotOp(), Branch(ifEnd)) ++ proc1.convert ++ List(IfThroughFlag(iftag),IfEnd(ifEnd))

        val elseifasm = elses.flatMap{ case (cmp, proc) => {
          val btag = cs.anonymousTag
          val etag = cs.anonymousTag
          List(ElseIfBegin(btag)) ++ cmp.convert ++ List(NotOp(), Branch(etag)) ++ proc.convert ++ List(IfThroughFlag(iftag),ElseIfEnd(etag))
        }}

        val elseasm = els match {
          case Some(proc) => {
            val btag = cs.anonymousTag
            val etag = cs.anonymousTag
            List(ElseBegin(btag)) ++ proc.convert ++ List(ElseEnd(etag))
          }
          case None => List.empty
        }
        List(IfStateBegin(iftag)) ++ ifasm ++ elseifasm ++ elseasm ++ List(IfStateEnd(iftag))
      }
      case ConstNumber(x) => List(Load(x))
      case BoolLiteral(x) => List(Load[Boolean](x))
      case StringLiteral(x) => List(Load[String](x))

      case Decrement(Variable(x)) => List(LoadLocal(x,"unknown"),Load(1),SubOp(),StoreLocal(x,"number"))
      case Increment(Variable(x)) => List(LoadLocal(x,"unknown"),Load(1),AddOp(),StoreLocal(x,"number"))
      case BitInv(x) => x.convert ++ List(InvOp())
      case Not(x) => x.convert ++ List(NotOp())

      case Rest(left,right) => binOp(left,RestOp(),right)
      case Div(left,right) => binOp(left,DivOp(),right)
      case Mul(left,right) => binOp(left,MulOp(),right)
      case Sub(left,right) => binOp(left,SubOp(),right)
      case Add(left,right) => binOp(left,AddOp(),right)

      case Less(left,right) => binOp(left,LessOp(),right)
      case AndLess(left,right) => binOp(left,AndLessOp(),right)
      case NotEqual(left,right) => binOp(left,NotEqualOp(),right)
      case Equal(left,right) => binOp(left,EqualOp(),right)
      case AndOver(left,right) => binOp(left,AndOverOp(),right)
      case Over(left,right) => binOp(left,OverOp(),right)

      case LogicOr(left,right) => binOp(left,LogicOrOp(),right)
      case LogicAnd(left,right) => binOp(left,LogicAndOp(),right)

      case BitRightShift(left,right) => binOp(left,BitRightShiftOp(),right)
      case BitLeftShift(left,right) => binOp(left,BitLeftShiftOp(),right)
      case BitXor(left,right) => binOp(left,BitXorOp(),right)
      case BitOr(left,right) => binOp(left,BitOrOp(),right)
      case BitAnd(left,right) => binOp(left,BitAndOp(),right)

      case Func(name,args) => args.convert ++ List(Call(name,"unknown"))
      case ArgsList(args) => args.convert
      case Assign(Variable(name),right) => right.convert ++ List(StoreLocal(name,"unknown"))
      case Variable(name) => List(LoadLocal(name,"unknown"))//TODO:Def? Read?
    }
  }

}
