/**
 * Created by kamiya on 2015/02/21.
 */
package kamiya.parse.expr
import kamiya.parse.expr.ExprConverter._

import scala.annotation.tailrec

object CodeGenerator {

  object BrainfuckCodeGenerator {
    case class TypeInfo(size:Int,typeName:Option[String] = None)
    case class Generate(code:String,movePtr:Int = 0,typeInfo:Option[TypeInfo] = None)

    def numberType = Some(TypeInfo(1,Some("Number")))
    
    def reset : String = "[-]"
    def repeat(t:String, n:Int) : String = t * n

    def loadNumber(x:Int) = Generate(reset + repeat("+",x) + ">",1,numberType)
    def loadString(str:String) = Generate(str.flatMap{c => reset + repeat("+",c.asInstanceOf[Int]) + ">\n"}.mkString,str.length,Some(TypeInfo(str.length,Some("String"))))

    def addNumber = Generate("<[<+>-]",-1,numberType)
    def subNumber = Generate("<[<->-]",-1,numberType)
    def mulNumber = Generate(//left,right,[leftCpyCpy],leftCpy
      """
        |<<[>>>+<<<-]>>
        |<[->
          |>[<+<<+>>>-]<
          |[>+<-]
        |<]
        |>>[-]<<
        |""".stripMargin,-1,numberType)

    def ifStateBegin(tag:String) = Generate("+>",1,Some(TypeInfo(1,Some(s"IfStateFlag$tag"))))

    def print(gens:List[Generate]) = gens.lastOption match {
      case Some(Generate(_,_,Some(TypeInfo(size,_)))) =>
        Generate(repeat("<",size) + repeat(".[-]>",size) + repeat("<",size + 1),-(size + 1),None)
    }
    def callStdFunc(name:String)(implicit gens: List[Generate]) = name match {
      case "print" => print(gens)
    }
  }

  import BrainfuckCodeGenerator._
  implicit class GenerateInfos(val self:List[Generate]) {
    def mkCode = self.flatMap(_.code).mkString
    def mkDbgCode(implicit s:String = "i") = self.flatMap(_.code + s).mkString
  }
  implicit class ILAsmAssemblerList(val self:List[ILAsm]) {
    def toBf :  List[Generate] = ILAsmAssembler.toBf(self)(List.empty)
  }
  object ILAsmAssembler {
    @tailrec
    def toBf(asm:List[ILAsm])(implicit generates:List[Generate]) : List[Generate] = asm match {
      case Nil => generates
      case StackPushAnnotation(_)::tail => toBf(tail)(generates)
      case LoadNumber(x)::tail => toBf(tail)(generates :+ loadNumber(x))
      case LoadString(str)::tail => toBf(tail)(generates :+ loadString(str))

      case AddOp()::tail => toBf(tail)(generates :+ addNumber)
      case SubOp()::tail => toBf(tail)(generates :+ subNumber)
      case MulOp()::tail => toBf(tail)(generates :+ mulNumber)
      case Call(name,typeName)::tail => toBf(tail)(generates :+ callStdFunc(name))
    }
  }
}
