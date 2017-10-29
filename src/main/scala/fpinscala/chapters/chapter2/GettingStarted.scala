package fpinscala.chapters.chapter2

/**
  * Created by seung on 2017/08/17.
  */
object GettingStarted {

  def main(args: Array[String]): Unit = {
    println(formatAbs(-48))
    println(formatFactorial(7))
    println(formatResult("factorial", 7, factorial))
    println(PolymorphicFunctions.findFirst(Array("one", "two", "three"), (x: String) => x == "two"))
    println(PolymorphicFunctions.findFirst(Array(2, 7, 11, 21), (x: Int) => x == 11))
    println(PolymorphicFunctions.isSorted(Array(2, 7, 11, 21), (current: Int, next: Int) => current < next))
  }

  def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def abs(n: Int): Int = if (n < 0) -n else n

  def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)

  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d"
    msg.format(name, n, f(n))
  }

}

object PolymorphicFunctions {
  def findFirst[A](as: Array[A], f: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int = {
      if (n >= as.length) -1
      else if (f(as(n))) n
      else loop(n + 1)
    }

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }

    go(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C) : (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

}


