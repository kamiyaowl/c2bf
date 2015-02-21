/**
 * Created by kamiya on 2015/02/16.
 */
package kamiya.parse.expr

import kamiya.util.ConsoleColor
import kamiya.util.ConsoleColor._

object ExprConverter {
  abstract class ILAsm

  abstract class ILAsmPopOp extends ILAsm
  abstract class ILAsmUnaryPopOp extends ILAsmPopOp
  abstract class ILAsmBinPopOp extends ILAsmPopOp

  trait ILAsmPushOp extends ILAsm

  abstract class ILAsmAnnotation extends ILAsm

  abstract class Load[A](bin: A) extends ILAsmPushOp

  case class LoadNumber(bin:Int) extends  Load[Int](bin){
    override def toString() = ("LoadNumber(" + bin.toString + ")").green
  }
  case class LoadBoolean(bin:Boolean) extends  Load[Boolean](bin){
    override def toString() = ("LoadBoolean(" + bin.toString + ")").green
  }
  case class LoadString(bin:String) extends  Load[String](bin) {
    override def toString() = ("LoadString(" + bin.toString + ")").green
  }


  case class LoadLocal(id: String,typeName:Option[String] = None) extends ILAsmPushOp {
    override def toString() = ("LoadLocal[" + typeName.toString + "](" + id + ")").cyan
  }
  case class StoreLocal(id: String,typeName:Option[String] = None) extends ILAsmPushOp {
    override def toString() = ("StoreLocal[" + typeName.toString + "](" + id + ")").cyan
  }

  case class LoadLocalArray(id: String,typeName:Option[String] = None) extends ILAsmUnaryPopOp with ILAsmPushOp {
    override def toString() = ("LoadLocalArray[" + typeName.toString + "](" + id + ")").magenta
  }
  case class StoreLocalArray(id: String,typeName:Option[String] = None) extends ILAsmBinPopOp {
    override def toString() = ("StoreLocalArray[" + typeName.toString + "](" + id + ")").magenta
  }

  case class Call(id: String,typeName:Option[String] = None) extends ILAsmPopOp with ILAsmPushOp {
    override def toString() = ("Call[" + typeName.toString + "](" + id + ")").yellow
  }

  case class StackPushAnnotation(n:Int) extends ILAsmAnnotation {
    override def toString() = ("@StackPush(" + n.toString + ")").yellow
  }
  case class BadOperateAnnotation(expr:Expr) extends ILAsmAnnotation {
    override def toString() = ("@BadOperate(" + expr.toString + ")").black.yellow_
  }

  case class AddOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class SubOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class MulOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class DivOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class RestOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class InvOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class NotOp() extends ILAsmBinPopOp with ILAsmPushOp

  case class LessOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class AndLessOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class NotEqualOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class EqualOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class AndOverOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class OverOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class LogicOrOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class LogicAndOp() extends ILAsmBinPopOp with ILAsmPushOp

  case class BitRightShiftOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class BitLeftShiftOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class BitXorOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class BitOrOp() extends ILAsmBinPopOp with ILAsmPushOp
  case class BitAndOp() extends ILAsmBinPopOp with ILAsmPushOp

  abstract class ILAsmControl extends ILAsm

  case class Jump(label:String) extends ILAsmControl
  case class Branch(label:String) extends ILAsmBinPopOp//TODO:Label同様に分類が必要？

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
  //副作用のあるゴミ
  class ConverterStatus {
    private var counter = 0
    def getCount : Int = { counter += 1; counter }
    def anonymousVariable = "$Value" + getCount
    def anonymousTag = "$Tag" + getCount
  }
  implicit class ExprConvert(val self: Expr) {
    private def binOp(left:Expr,asm:ILAsmBinPopOp, right:Expr)(implicit cs:ConverterStatus) = left.convert ++ right.convert ++ List(asm)

    def convert(implicit cs:ConverterStatus) : List[ILAsm] = self match {

      case DefinitionVariable(t,Variable(name), right) => right.convert ++ List(StoreLocal(name,Some(t)))
      case DefinitionArray(t,Variable(name), ExprList(values),size) => {
        val arrSize = if(size.isEmpty) values.size else size.get
        val rightasm = values take(arrSize) flatMap(_.convert)
        val storeasm = (0 to (arrSize - 1)).reverse.flatMap{ n => List(LoadNumber(n),LoadLocalArray(name,Some(t))) }
        rightasm ++ storeasm
      }

      case SwitchState(cmp,cases) =>{
        val anoVar = cs.anonymousVariable
        val endTag = cs.anonymousTag

        val start = cmp.convert ++ List(StoreLocal(anoVar,None))
        val casesAsm : List[(List[ILAsm],List[ILAsm])] = cases.filter{
          case CaseState(_,_) => true
          case _ => false
        }.map { case CaseState(l, exprs) => {
            val label = cs.anonymousTag
            ( List(LoadLocal(anoVar,None)) ++ l.convert ++ List[ILAsm](EqualOp(), Branch(label)),
              List[ILAsm](CaseLabel(label)) ++ exprs.convert ++ List[ILAsm](Jump(endTag)))
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
      case ConstNumber(x:Int) => List(LoadNumber(x))
      case ConstNumber(x:Char) => List(LoadNumber(x))
      case BoolLiteral(x) => List(LoadBoolean(x))
      case StringLiteral(x) => List(LoadString(x))

      case Decrement(Variable(x)) => List(LoadLocal(x,None),LoadNumber(1),SubOp(),StoreLocal(x,Some("Number")))
      case Increment(Variable(x)) => List(LoadLocal(x,None),LoadNumber(1),AddOp(),StoreLocal(x,Some("Number")))
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

      case Func(name,args) => args.convert ++ List(Call(name,None))
      case ExprList(args) => args.convert ++ List(StackPushAnnotation(args.size))

      case Assign(Variable(name),right) => right.convert ++ List(StoreLocal(name,None))
      case Assign(Indexer(Variable(name),indexExpr),right) => right.convert ++ indexExpr.convert ++ List(StoreLocalArray(name,None))
      case Variable(name) => List(LoadLocal(name,None))
      case _ => List(BadOperateAnnotation(self))
    }
  }

}
