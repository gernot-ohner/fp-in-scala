package datastructures

object Datastructures {

  def main(args: Array[String]): Unit = {
    println(patternMatchingExample)

    val integers = List(1, 2, 3, 4, 5)
    Range(-1, 6).map(List.drop(integers, _)).foreach(println)

    val setHeadResult = List.setHead(100, integers)
    println(setHeadResult)

    val dropWhileResult = List.dropWhile(integers)(_ < 3)
    println(dropWhileResult)

    val dropWhileResult2 = List.dropWhile(integers)(_ > 3)
    println(dropWhileResult2)

    val initResult = List.init(integers)
    println(initResult)

    val lengthResult = List.length(integers)
    println(lengthResult)

    val expensiveIdentityResult = List.expensiveId(integers)
    println(expensiveIdentityResult)

    println(List.lengthByFoldLeft(integers))
    println(List.sumByFoldLeft(integers))
    println(List.productByFoldLeft(List(1.0, 2.0, 3.0)))

    val reverseResult = List.reverse(integers)
    println(reverseResult)

    val sumByFoldRightByFoldLeftResult = List.sumByFoldRightByFoldLeft(integers)
    println(sumByFoldRightByFoldLeftResult)
    println(List.lengthByFoldLeftByFoldRight(integers))
    println(List.sumByFoldLeftByFoldRight(integers))
    println(List.productByFoldLeftByFoldRight(List(1.0, 2.0, 3.0)))

    val appendByFoldRightResult = List.appendByFoldRight(integers, integers)
    println(appendByFoldRightResult)

    val concatResult = List.concat(List(integers, integers, integers))
    println(concatResult)

    val plusOneResult = List.plusOne(integers)
    println(plusOneResult)

    val allToStringResult = List.allToString(List(1.0, 3.7, 123.5))
    println(allToStringResult)

    val mapResult = List.map(integers)(_ + 10)
    println(mapResult)

    val filterResult = List.filter(integers)(_ % 2 == 0)
    println(filterResult)

    val biggifyer = (x: Int) => List(x, x, x)
    val flatMapResult = List.flatMap(integers)(biggifyer)
    println(flatMapResult)

    val filterByFlatMapResult = List.filterByFlatMap(integers)(_ % 2 == 0)
    println(filterByFlatMapResult)



  }

  // Exercise 3.1
  private def patternMatchingExample = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    x
  }
}
