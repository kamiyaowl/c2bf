/**
 * Created by kamiya on 2015/02/21.
 */
package kamiya.parse.expr

import java.security.InvalidParameterException

import kamiya.parse.expr.ExprConverter._

object CodeGenerator {
  case class LocalTyper(name:String,pt:Int,size:Int,typeName:Option[String] = None)
  case class TypeInfo(size: Int, typeName: Option[String] = None)
  case class Generate(code: String, movePtr: Int = 0, typeInfo: Option[TypeInfo] = None)

  def numberType = Some(TypeInfo(1, Some("Number")))
  def reset: String = "[-]"
  def rep(t: String, n: Int): String = t * n
  def loadNumber(x: Int) = Generate(reset + rep("+", x) + ">", 1, numberType)
  def loadString(str: String) = Generate(str.flatMap { c => reset + rep("+", c.asInstanceOf[Int]) + ">\n"}.mkString, str.length, Some(TypeInfo(str.length, Some("String"))))
  def addNumber = Generate("<[<+>-]", -1, numberType)//TODO: TypeAnnotation & String Concat
  def subNumber = Generate("<[<->-]", -1, numberType)
  //left,right,[leftCpyCpy],leftCpy
  def mulNumber = Generate("""
      |<<[>>>+<<<-]>>
      |<[->
        |>[<+<<+>>>-]<
        |[>+<-]
      |<]
      |>>[-]<<
      |""".stripMargin,-1, numberType)
  def loadLocal(current:Int,target:Int) : Generate = (target - current) match {
    case 0 => ???
    //.....current......target
    case x if x > 0 => ???
    //..target......current...
    case x if x < 0 => {
      val diff = -x

      Generate(
        rep("<",diff) +
          "[-" +
          rep(">",diff) + "+>+<" + rep("<",diff) +
          "]" +
          rep(">",diff) +
          ">" +
            "[-" + rep("<",diff + 1) + "+" + rep(">",diff + 1) + "]"
        ,1,Some(TypeInfo(1)))
    }
  }
  def loadLocals(current:Int,target:Int,size:Int) : Generate = {
    val cpy = loadLocal(current,target)
    val code = (0 until size).map{i => cpy.code}.foldLeft("")(_ + _)
    Generate(code,size,Some(TypeInfo(size)))
  }

  def ifStateBegin(tag: String) = Generate("+>",1, Some(TypeInfo(1,Some(s"IfStateFlag$tag"))))
  def print(gens:List[Generate]) = gens.lastOption match {
    case Some(Generate(_,_, Some(TypeInfo(size,_)))) =>
      Generate(rep("<",size) + rep(".[-]>",size) + rep("<",size),-size,None)
  }
  def callStdFunc(name: String)(implicit gens: List[Generate]) = name match {
    case "print" => print(gens)
  }

  object ILAsmAssembler {
    implicit class GenerateList(val self: List[Generate]) {
      def mkCode = self.flatMap(_.code).mkString

      def mkDbgCode(implicit s: String = "i") = self.flatMap(_.code + s).mkString
    }

    implicit class ILAsmAssemblerList(val self: List[ILAsm]) {
      def toBf: List[Generate] = ILAsmAssembler.toBf(self)(List.empty,Map.empty)
    }
    //TODO:Memorized pt
    def toBf(asm: List[ILAsm])(implicit generates: List[Generate], typer:Map[String,LocalTyper]): List[Generate] = asm match {
      case Nil => generates

      case StackPushAnnotation(_) :: tail => toBf(tail)
      case SizeAnnotation(_) :: tail => toBf(tail)

      case DefinitionAnnotation(name,defAsm)::tail => {
        val varName = typer.get(name) match {
          //ReDefinition Error
          case Some(lt) => ???
          case None => name
        }
        defAsm.find {
          case SizeAnnotation(_) => true
          case _ => false
        }.headOption match {
          case Some(SizeAnnotation(size)) => {
            val pt = generates.map(_.movePtr).sum
            val gen = toBf(defAsm)(generates, typer)
            toBf(tail)(gen, typer + (varName -> LocalTyper(varName, pt, size)))
          }
          case None => ???
        }
      }
      case LoadNumber(x) :: tail => toBf(tail)(generates :+ loadNumber(x),typer)
      case LoadString(str) :: tail => toBf(tail)(generates :+ loadString(str),typer)
      case StoreLocal(name,t) :: tail => toBf(tail)
      case LoadLocal(name,t)::tail => typer.get(name) match {
        case Some(lt) => toBf(tail)(generates :+ loadLocals(generates.map(_.movePtr).sum,lt.pt,lt.size),typer)
        case None => ???
      }
      case AddOp() :: tail => toBf(tail)(generates :+ addNumber,typer)
      case SubOp() :: tail => toBf(tail)(generates :+ subNumber,typer)
      case MulOp() :: tail => toBf(tail)(generates :+ mulNumber,typer)
      case Call(name, typeName) :: tail => toBf(tail)(generates :+ callStdFunc(name),typer)
    }
  }
}
