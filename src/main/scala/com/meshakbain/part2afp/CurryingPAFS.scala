package com.meshakbain.part2afp

import com.meshakbain.part1as.Recap.x

object CurryingPAFS {

  val superAdder: Int => Int => Int =
    x => y => x + y

  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedMethod(x: Int)(y: Int) = x + y

  val add7v1 = simpleAddFunction(_, 7)
  val add7v2 = simpleAddMethod(_, 7)
  val add7v3 = curriedMethod(7)

  def format(fmt: String)(n: Double): String = fmt.format(n)

}
