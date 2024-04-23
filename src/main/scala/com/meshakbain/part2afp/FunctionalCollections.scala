package com.meshakbain.part2afp

object FunctionalCollections {
  val aSet: Set[String] = Set("I", "Love", "Scala")
  val setContainsScala = aSet("Scala")

  // Seq extend PartialFunctions[int,A]
  val aSeq = Seq[Int](1, 2, 3, 4)
  val anElement = aSeq(2)

  // Map extends PartialFunction[K, V]
  val aPhonebook = Map(
    "Alice" -> 123456,
    "Bob" -> 987654
  )

}
