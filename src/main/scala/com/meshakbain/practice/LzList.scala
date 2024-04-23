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
    list.reverse.foldLeft(LzList.empty[A])((lzList, elem) => elem #:: lzList)
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
    @tailrec
    def forEachTailRec(lzList: LzList[A]): Unit = {
      if (lzList.isEmpty) ()
      else {
        f(lzList.head)
        forEachTailRec(lzList.tail)
      }
    }

    forEachTailRec(this)

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
    println(naturals.tail.head)
    println(naturals.tail.tail.head)
    println(naturals.tail.tail.tail.head)
    println(naturals.tail.tail.tail.tail.head)

    val first100 = naturals.take(100)
    first100.forEach(println)
    val first50k = naturals.take(50000)
    first50k.forEach(println)
  }
}
