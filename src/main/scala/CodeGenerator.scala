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

    def reset : String = "[-]"
    def repeat(t:String, n:Int) : String = t * n

    def loadNumber(x:Int) = Generate(reset + repeat("+",x) + ">",1,Some(TypeInfo(1,Some("Number"))))
    def loadString(str:String) = Generate(str.flatMap{c => reset + repeat("+",c.asInstanceOf[Int]) + ">"}.mkString,str.length,Some(TypeInfo(str.length,Some("String"))))

    def print(gens:List[Generate]) = gens.lastOption match {
      case Some(Generate(_,size,_)) =>
        Generate(repeat("<",size) + repeat(".[-]>",size) + repeat("<",size + 1),-(size + 1))
    }
    def callStdFunc(name:String)(implicit gens: List[Generate]) = name match {
      case "print" => print(gens)
    }
  }

  import BrainfuckCodeGenerator._
  implicit class GenerateInfos(val self:List[Generate]) {
    def mkCode = self.flatMap(_.code).mkString
  }
  implicit class ILAsmAssemblerList(val self:List[ILAsm]) {
    def toBf :  List[Generate] = ILAsmAssembler.toBf(self)(List.empty)
  }
  object ILAsmAssembler {
    def toBf(asm:ILAsm)(implicit generates:List[Generate]) : Option[Generate] = asm match {
      case StackPushAnnotation(_) => None
      case LoadNumber(x) => Some(loadNumber(x))
      case LoadString(str) => Some(loadString(str))
      case Call(name,typeName) => Some(callStdFunc(name))
    }
    @tailrec
    def toBf(asm:List[ILAsm])(implicit generates:List[Generate]) : List[Generate] = asm match {
      case Nil => generates
      case head::tail => {
        val ngen = toBf(head)(generates)
        val gen = if(ngen.isEmpty) generates else generates ++ List(ngen.get)
        toBf(tail)(gen)
      }
    }
  }
}
