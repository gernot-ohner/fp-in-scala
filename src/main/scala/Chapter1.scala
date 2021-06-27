package dev.ohner

import scala.annotation.tailrec

object Chapter1 {

  def abs(i: Int): Int = if (i < 0) -i else i

  private def formatAbs(i: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(i, abs(i))
  }

  private def formatFactorial(i: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(i, factorial(i))
  }

  def factorial(i: Int): Int = {
    @tailrec
    def go(i: Int, acc: Int): Int = {
      if (i <= 1) acc
      else go(i - 1, acc * i)
    }

    go(i, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def go(count: Int, limit: Int, i: Int, j: Int): Int = {
      if (count < limit - 2) go(count + 1, limit, j, i + j)
      else j
    }

    go(0, n, 0, 1)
  }

  def formatResult(operationName: String, n: Int, f: Int => Int): String = {
    val msg: String = "The %s of %d is %d"
    msg.format(operationName, n, f(n))
  }

  def findFirst[A](arr: List[A], p: A => Boolean): Int = {
    @tailrec
    def loop(i: Int): Int = {
      if (i >= arr.size) -1
      else if (p(arr(i))) i
      else loop(i + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean = {
      if (i >= as.length) true
      else if (ordered(as(i - 1), as(i))) loop(i + 1)
      else false
    }

    if (as.length < 2) true
    else loop(1)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }


  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatFactorial(10))
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 42, factorial))

    val sortedArray = Array(1, 2, 6, 8, 10)
    val unsortedArray = Array(1, 5, 2, 3, 5)
    def comparator: (Int, Int) => Boolean = (i, j) => i < j
    println(isSorted(sortedArray, comparator))
    println(isSorted(unsortedArray, comparator))

    def binaryFunctionAddition(i: Int, j: Int) = i + j
    val curriedAddition = curry(binaryFunctionAddition)
    val add3 = curriedAddition(3)
    val result = add3(5)
    println(s"Result should be 8, is: $result")
    val uncurriedAddition = uncurry(curriedAddition)
    val result2 = uncurriedAddition(10, 15)
    println(s"Result should be 25, is $result2");

    val sqrtOfAbsValue: Double => Double = compose(Math.sqrt, Math.abs)

    val result3 = sqrtOfAbsValue(-25.0)

    println(s"Result should be 5, is: $result3");
  }

}
