package com.meshakbain.part2afp

object PartialFunctions {

  val aFussyFunction_v2 = (x: Int) =>
    x match {
      case 1 => 42
      case 2 => 56
      case 5 => 999
    }

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 5 => 999
  }

  def main(args: Array[String]) = {}
}
