package com.meshakbain.practice

import scala.annotation.tailrec

abstract class FSet[A] extends (A => Boolean) {
  def contains(elem: A): Boolean
  def apply(elem: A): Boolean = contains(elem)

  infix def +(elem: A): FSet[A] 
  infix def ++(anotherSet: FSet[A]) : FSet[A] 

  def map[B]( f: A => B) : FSet[B]
  def flatMap[B]( f: A => FSet[B]): FSet[B]
  def filter(predicate: A => Boolean): FSet[A]
  def forEach(f: A => Unit) : Unit

  infix def -(elem: A): FSet[A]
  infix def --(anotherSet: FSet[A]): FSet[A]
  infix def &(anotherSet: FSet[A]): FSet[A]
}

case class Empty[A]() extends FSet[A] {
  def contains(elem: A): Boolean = false
  
  infix def +(elem: A) = LSet(elem, this)
  infix def ++(anotherSet: FSet[A]): FSet[A] = anotherSet
  
  def map[B](f: A => B): FSet[B] = Empty[B]()
  def flatMap[B](f: A => FSet[B]): FSet[B] = Empty[B]()
  def filter(predicate: A=> Boolean) : FSet[A] = this
  def forEach(f: A => Unit): Unit = ()

  infix def -(elem: A): FSet[A] = this 
  infix def --(anotherSet: FSet[A]): FSet[A] = this 
  infix def &(anotherSet: FSet[A]): FSet[A] = this
}

case class LSet[A](val head: A, tail: FSet[A]) extends FSet[A] {
  def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  infix def +(elem: A): FSet[A] = {
    if (contains(elem)) this 
    else LSet(elem, this)
  }
    
  infix def ++(anotherSet: FSet[A]): FSet[A] = {
    anotherSet ++ tail + head
  }

  def map[B](f: A => B): FSet[B] = {
   tail.map(f) + f(head)
  }


  def flatMap[B](f: A => FSet[B]): FSet[B] = tail.flatMap(f) ++ f(head)
  def filter(predicate: A => Boolean): FSet[A] = {
    if (predicate(head))  tail.filter(predicate) + head
    else tail.filter(predicate)
  }

  def forEach(f: A => Unit): Unit = {
    f(head)
    tail.forEach(f)
  }

  infix def -(elem: A): FSet[A] = {
    if(head == elem) tail
    else tail - elem + head
  }
   
  infix def --(anotherSet: FSet[A]): FSet[A] = filter((x) => !anotherSet(x))
  infix def &(anotherSet: FSet[A]): FSet[A] = filter(anotherSet)
}

object FSet {
  def apply[A](values: A*): FSet[A] = {
    @tailrec
    def buildSet(valuesSeq: Seq[A], acc: FSet[A]): FSet[A] = {
      if(valuesSeq.isEmpty) acc
      else buildSet(valuesSeq.tail, acc + valuesSeq.head)
    }

    buildSet(values, Empty())
  }
}

object FunctiionalSet {

  def main(args: Array[String]) = {
    val first5 = FSet(1,2,3,4,5)
    val someNumbers = FSet(4,5,6,7,8)
    println(first5.contains(5)) // true
    println(first5(6))          // false
    println((first5 + 10).contains(10)) // true
    println(first5.map(_ * 2).contains(10)) // true
    println(first5.map(_ % 2).contains(1))  // true
    println(first5.flatMap(x => FSet(x, x + 1)).contains(7)) // false
  }
}
