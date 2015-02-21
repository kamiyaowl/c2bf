/**
 * Created by kamiya on 2015/02/21.
 */
package kamiya.util

import scala.{Console => C}

object ConsoleColor {
  implicit class ColorString(val self:String) extends AnyVal {
    def black = setColor(C.BLACK)
    def red = setColor(C.RED)
    def green = setColor(C.GREEN)
    def yellow = setColor(C.YELLOW)
    def blue = setColor(C.BLUE)
    def magenta = setColor(C.MAGENTA)
    def cyan = setColor(C.CYAN)
    def white = setColor(C.WHITE)

    def black_ = setColor(C.BLACK_B)
    def red_ = setColor(C.RED_B)
    def green_ = setColor(C.GREEN_B)
    def yellow_ = setColor(C.YELLOW_B)
    def blue_ = setColor(C.BLUE_B)
    def magenta_ = setColor(C.MAGENTA_B)
    def cyan_ = setColor(C.CYAN_B)
    def white_ = setColor(C.WHITE_B)

    def reset = setColor(C.RESET)
    def bold = setColor(C.BOLD)
    def underlined = setColor(C.UNDERLINED)
    def blink = setColor(C.BLINK)
    def reversed = setColor(C.REVERSED)
    def invisible = setColor(C.INVISIBLE)
    private def setColor(x:String) = x +  self + C.RESET
  }
}