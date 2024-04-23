package com.meshakbain.practice

import scala.annotation.tailrec

abstract class LzList[A] {
  def isEmpty: Boolean
  def head: A
  def tail: LzList[A]

  infix def #::(element: A): LzList[A]
  infix def ++(another: => LzList[A]): LzList[A]

  def forEach(f: A => Unit): Unit
  def map[B](f: A => B): LzList[B]
  def flatMap[B](f: A => LzList[B]): LzList[B]
  def filter(predicate: A => Boolean): LzList[A]
  def withFilter(predicate: A => Boolean): LzList[A] = filter(predicate)

  def take(n: Int): LzList[A]
  def toList: List[A] = {
    @tailrec
    def toListTailRec(remaining: LzList[A], acc: List[A]): List[A] = {
      if (remaining.isEmpty) acc.reverse
      else toListTailRec(remaining.tail, remaining.head :: acc)
    }
    toListTailRec(this, List[A]())
  }
  def takeAsList(n: Int): List[A] = take(n).toList
}

object LzList {
  def empty[A]: LzList[A] = LzEmpty()
  def generate[A](start: A)(generator: A => A): LzList[A] =
    new LzCons(
      start,
      generate(generator(start))(generator)
    )
  def from[A](list: List[A]): LzList[A] =
    list.foldLeft(LzList.empty[A])((lzList, elem) => elem #:: lzList)
}

case class LzEmpty[A]() extends LzList[A] {
  def isEmpty: Boolean = true
  def head: A = throw new NoSuchElementException
  def tail: LzList[A] = throw new NoSuchElementException

  infix def #::(element: A): LzList[A] = new LzCons[A](element, this)
  infix def ++(another: => LzList[A]): LzList[A] = another

  def forEach(f: A => Unit): Unit = ()
  def map[B](f: A => B): LzList[B] = LzEmpty[B]()
  def flatMap[B](f: A => LzList[B]): LzList[B] = LzEmpty[B]()
  def filter(predicate: A => Boolean): LzList[A] = this

  def take(n: Int): LzList[A] = {
    if (n != 0)
      throw new RuntimeException(s"Cannot take $n elements from the list")
    else this
  }

}

class LzCons[A](hd: => A, tl: => LzList[A]) extends LzList[A] {

  def isEmpty: Boolean = false
  override lazy val head = hd
  override lazy val tail = tl

  infix def #::(element: A): LzList[A] = new LzCons(element, this)
  infix def ++(another: => LzList[A]): LzList[A] =
    new LzCons(head, tail ++ another)

  def forEach(f: A => Unit): Unit = {
    f(head)
    tail.forEach(f)
  }
  def map[B](f: A => B): LzList[B] = {
    new LzCons(f(head), tail.map(f))
  }
  def flatMap[B](f: A => LzList[B]): LzList[B] = {
    f(head) ++ tail.flatMap(f)
  }
  def filter(predicate: A => Boolean): LzList[A] = {
    if (predicate(head)) new LzCons(head, tail.filter(predicate))
    else tail.filter(predicate)
  }

  def take(n: Int): LzList[A] = {
    if (n <= 0) LzEmpty()
    else if (n == 1) new LzCons(head, LzEmpty())
    else LzCons(head, tail.take(n - 1))
  }
}

object LzListPlayground {
  def main(args: Array[String]) = {
    val naturals = LzList.generate(1)(n => n + 1)
  }
}

// abstract class LzList[A] {
//   def isEmpty: Boolean
//   def head: A
//   def tail: LzList[A]

//   def #::(element: A): LzList[A]
//   def ++(another: LzList[A]): LzList[A]

//   def forEach(f: A => Unit): Unit
//   def map[B](f: A => B): LzList[B]
//   def flatMap[B](f: A => LzList[B]): LzList[B]
//   def filter(predicate: A => Boolean): LzList[A]
//   def withFilter(predicate: A => Boolean): LzList[A] = filter(predicate)

//   def take(n: Int): LzList[A]
//   def takeAsList(n: Int): List[A] = take(n).toList
//   def toList: List[A] = {
//     @tailrec
//     def toListAux[A](remaining: LzList[A], acc: List[A]): List[A] = {
//       if (remaining.isEmpty) acc.reverse
//       else toListAux(remaining.tail, remaining.head :: acc)
//     }
//     toListAux(this, List())
//   }

// }

// case class LzEmpty[A]() extends LzList[A] {
//   def isEmpty: Boolean = true;
//   def head: A = throw new NoSuchElementException
//   def tail: LzList[A] = throw new NoSuchElementException

//   infix def #::(element: A): LzList[A] = LzCons(element, this)
//   infix def ++(another: LzList[A]): LzList[A] = another

//   def forEach(f: A => Unit): Unit = ()
//   def map[B](f: A => B): LzList[B] = LzEmpty[B]()
//   def flatMap[B](f: A => LzList[B]): LzList[B] = LzEmpty[B]()
//   def filter(predicate: A => Boolean): LzList[A] = this

//   def take(n: Int): LzList[A] = {
//     if (n == 0) this
//     else
//       throw new RuntimeException(
//         s"Cannot take $n elements from an empty lazy list"
//       )
//   }

//   override def toString(): String = "_"
// }

// class LzCons[A](hd: => A, tl: => LzList[A]) extends LzList[A] {
//   def isEmpty: Boolean = false;
//   override lazy val head: A = hd;
//   override lazy val tail: LzList[A] = tl

//   def #::(element: A): LzList[A] = new LzCons(element, this)
//   def ++(another: => LzList[A]): LzList[A] =
//     new LzCons(head, tail ++ another)

//   def forEach(f: A => Unit): Unit = {
//     @tailrec
//     def forEachTailRec(lzList: LzList[A]): Unit = {
//       if (lzList.isEmpty) ()
//       else {
//         f(lzList.head)
//         forEachTailRec(lzList.tail)
//       }
//     }
//   }
//   def map[B](f: A => B): LzList[B] = new LzCons(f(head), tail.map(f))

//   def flatMap[B](f: A => LzList[B]): LzList[B] = {
//     f(head) ++ tail.flatMap(f)
//   }
//   def filter(predicate: A => Boolean): LzList[A] = {
//     if (predicate(head)) head #:: tail.filter(predicate)
//     else tail.filter(predicate)
//   }

//   def take(n: Int): LzList[A] = {
//     if (n <= 0) LzEmpty()
//     else if (n == 1) new LzCons(head, LzEmpty())
//     else new LzCons(head, tail.take(n - 1))
//   }

//   override def toString(): String = s"$head" + tail.toString()
// }

// object LzList {
//   def generate[A](start: A)(generator: A => A): LzList[A] = {
//     new LzCons(start, LzList.generate(generator(start))(generator))
//   }
//   def from[A](list: List[A]): LzList[A] = ???
// }

// object LzListPlayground {

//   def main(args: Array[String]) = {
//     val aList = new LzCons(3, new LzCons(2, new LzCons(1, LzEmpty())))
//     val aList2 = 4 #:: aList

//     println(aList2.head)
//     println(aList ++ aList2)

//   }
// }
