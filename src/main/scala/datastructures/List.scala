package datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  // Exercise 3.3
  def setHead[A](a: A, as: List[A]): List[A] = as match {
    case Nil => Cons(a, Nil)
    case Cons(_, tail) => Cons(a, tail)
  }

  /** Exercise 3.4
   * The way this method is implemented now, it'll return Nil for negative n.
   * I think that's fine, though I'd probably throw an error in a real application
   */
  @tailrec
  def drop[A](as: List[A], n: Int): List[A] = as match {
    case Nil => Nil
    case Cons(_, tail) =>
      if (n == 0) as
      else if (n == 1) tail
      else drop(tail, n - 1)
  }

  // Exercise 3.5
  @tailrec
  def dropWhile[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(head, tail) =>
      if (p(head)) dropWhile(tail)(p)
      else as
  }

  def append[A](l1: List[A], l2: List[A]): List[A] = l1 match {
    case Nil => l2
    case Cons(head, tail) => Cons(head, append(tail, l2))
  }

  /**
   * Exercise 3.6
   * This method technically works (?), but has terrible, terrible, terrible performance
   * (object creations quadratic in the length of the list???)
   */
  def init[A](l: List[A]): List[A] = {
    @tailrec
    def go(initialPart: List[A], rest: List[A]): List[A] = rest match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (tail == Nil) initialPart
        else go(append(initialPart, List(head)), tail)
    }

    go(List(), l)
  }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => f(head, foldRight(tail, z)(f))
  }

  def lengthWithoutFold[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, tail) => 1 + lengthWithoutFold(tail)
  }

  // Exercise 3.8
  def expensiveId[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])(Cons(_, _))

  // Exercise 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_: A, lengthUptoNow: Int) => 1 + lengthUptoNow)

  // Exercise 3.10
  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

  // Exercise 3.11
  def sumByFoldLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productByFoldLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthByFoldLeft[A](l: List[A]): Int = foldLeft(l, 0)((b, _) => 1 + b)

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    @tailrec
    def go(rest: List[A], currentHead: List[A]): List[A] = rest match {
      case Nil => currentHead
      case Cons(head, tail) => go(tail, Cons(head, currentHead))
    }

    go(as, Nil)
  }

  def reverseWithFold[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((a, b) => Cons(b, a))

  // Exercise 3.13
  def foldRightByFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((a, b) => f(b, a))

  def foldLeftByFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, z)((a, b) => f(b, a))

  def sumByFoldRightByFoldLeft(ints: List[Int]): Int = foldRightByFoldLeft(ints, 0)(_ + _)

  def sumByFoldLeftByFoldRight(l: List[Int]): Int = foldLeftByFoldRight(l, 0)(_ + _)

  def productByFoldLeftByFoldRight(l: List[Double]): Double = foldLeftByFoldRight(l, 1.0)(_ * _)

  def lengthByFoldLeftByFoldRight[A](l: List[A]): Int = foldLeftByFoldRight(l, 0)((b, _) => 1 + b)

  // Exercise 3.14
  def appendByFoldRight[A](l1: List[A], l2: List[A]): List[A] = foldRightByFoldLeft(l2, l1)(Cons(_, _))

  // Exercise 3.15: Concat a list of lists into a list
  def concat[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil: List[A])(appendByFoldRight)

  // Exercise 3.16
  def plusOne(ints: List[Int]): List[Int] = foldRightByFoldLeft(ints, Nil: List[Int])((a, b) => Cons(a + 1, b))

  // Exercise 3.17
  def allToString(ds: List[Double]): List[String] = foldRightByFoldLeft(ds, Nil: List[String])((d, tail) => Cons(d.toString, tail))

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRightByFoldLeft(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // Exercise 3.19
  def filter[A](as: List[A])(p: A => Boolean): List[A] = foldRightByFoldLeft(as, Nil: List[A])((head, z) => if (p(head)) Cons(head, z) else z)

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRightByFoldLeft(as, Nil: List[B])((head, z) => List.append(f(head), z))

  // Exercise 3.21
  def filterByFlatMap[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as)(a => if (p(a)) List(a) else Nil: List[A])

  // Exercise 3.22
  def addPairs(ints1: List[Int], ints2: List[Int]): List[Int] = (ints1, ints2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairs(t1, t2))
  }


}
