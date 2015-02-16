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
  case class IfBegin() extends ILAsmControl
  case class IfEnd() extends ILAsmControl
  case class ElseIfBegin() extends ILAsmControl
  case class ElseIfEnd() extends ILAsmControl
  case class ElseBegin() extends ILAsmControl
  case class ElseEnd() extends ILAsmControl

  case class WhileBegin(label:String) extends ILAsmControl
  case class WhileBranch(label:String) extends ILAsmControl
  case class WhileEnd(label:String) extends ILAsmControl

  case class JmpOp(label:String) extends ILAsmControl
  case class BranchOp(label:String) extends ILAsmControl
  case class Label(label:String) extends ILAsmControl

  implicit class ExprsConvert(val self: List[Expr]) {
    def convert : List[ILAsm] = self flatMap(_.convert)
  }
  object StaticTools {
    private var counter = 0
    def getCount : Int = { counter += 1; counter }
    def anonymousVariable = "___Anonymous_Variable_" + getCount + "___"
    def anonymousTag = "___Anonymous_Tag_" + getCount + "___"
  }
  implicit class ExprConvert(val self: Expr) {
    private def binOp(left:Expr,asm:ILAsmOp, right:Expr) = left.convert ++ right.convert ++ List(asm)
    private def ifElseOp(expr:(Expr,List[Expr]))(b:ILAsmControl, e:ILAsmControl) =
      expr._1.convert ++ List(b) ++ expr._2.convert ++ List(e)

    def convert : List[ILAsm] = self match {
      case Definition(t,Variable(name), right) => right.convert ++ List(StoreLocal(name,t))

      //TODO: impl Repeat
      //TODO: Remove StaticTools
      case SwitchState(cmp,cases) =>{
        val anoVar = StaticTools.anonymousVariable
        val endTag = StaticTools.anonymousTag

        val start = cmp.convert ++ List(StoreLocal(anoVar,"unknown"))
        val casesAsm : List[(List[ILAsm],List[ILAsm])] = cases.filter{
          case CaseState(_,_) => true
          case _ => false
        }.map { case CaseState(l, exprs) => {
          val label = StaticTools.anonymousTag
          (List(LoadLocal(anoVar,"unknown")) ++ l.convert ++ List[ILAsm](BranchOp(label)), List[ILAsm](Label(label)) ++ exprs.convert ++ List[ILAsm](JmpOp(endTag)))
        }
        }
        val defaultAsm : List[ILAsm] = cases.find{
          case DefaultState(_) => true
          case _ => false
        } match {
          case Some(DefaultState(expr)) => expr.convert ++ List(JmpOp(endTag))
          case _ => List.empty[ILAsm]
        }
        start ++ casesAsm.flatMap(_._1) ++ defaultAsm ++ casesAsm.flatMap(_._2) ++ List[ILAsm](Label(endTag))
      }
      case ForState(gen,cmp,addr,exprs) => {
        val startTag = StaticTools.anonymousTag
        val endTag = StaticTools.anonymousTag
        gen.convert ++ List(Label(startTag)) ++ cmp.convert ++ List(NotOp(), BranchOp(endTag)) ++ exprs.convert ++ addr.convert ++ List(JmpOp(startTag), Label(endTag))
      }
      case WhileState(cmp,exprs) => {
        val startTag = StaticTools.anonymousTag
        val endTag = StaticTools.anonymousTag
        List(Label(startTag)) ++ cmp.convert ++ List(NotOp(), BranchOp(endTag)) ++ exprs.convert ++ List(JmpOp(startTag), Label(endTag))
      }
        //TODO:else ifも考慮したendTagジャンプ
      case IfState(head,Nil,None) => ifElseOp(head)(IfBegin(),IfEnd())
      case IfState(head,Nil,Some(el)) => ifElseOp(head)(IfBegin(),IfEnd()) ++ List(ElseBegin()) ++ el.convert ++ List(ElseEnd())
      case IfState(head,els,None) => ifElseOp(head)(IfBegin(),IfEnd()) ++ els.flatMap(ifElseOp(_)(ElseIfBegin(),ElseIfEnd()))
      case IfState(head,els,Some(el)) => ifElseOp(head)(IfBegin(),IfEnd()) ++ els.flatMap(ifElseOp(_)(ElseIfBegin(),ElseIfEnd())) ++ List(ElseBegin()) ++ el.convert ++ List(ElseEnd())

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
