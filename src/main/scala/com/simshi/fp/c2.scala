package com.simshi.fp

/**
  * Created by simshi on 10/20/16.
  */
object c2 {
  def main(args: Array[String]): Unit = {
    println(factorial(5))

    println(fibAtNth(1))
    println(fibAtNth(2))
    println(fibAtNth(3))
    println(fibAtNth(4))
    println(fibAtNth(5))
    println(fibAtNth(6))
    println(fibAtNth(7))

    println("==== binary search")
    println(binarySearch(Array(1, 2, 3, 5, 7, 8), 1))
    println(binarySearch(Array(1, 2, 3, 5, 7, 8), 5))
    println(binarySearch(Array(1, 2, 3, 5, 7, 8), 8))
    println(binarySearch(Array(1, 2, 3, 5, 7, 8), 0))
    println(binarySearch(Array(1, 2, 3, 5, 7, 8), 99))
    println(binarySearch_1(Array(1, 2, 3, 5, 7, 8), 1))
    println(binarySearch_1(Array(1, 2, 3, 5, 7, 8), 5))
    println(binarySearch_1(Array(1, 2, 3, 5, 7, 8), 8))
    println(binarySearch_1(Array(1, 2, 3, 5, 7, 8), 0))
    println(binarySearch_1(Array(1, 2, 3, 5, 7, 8), 99))

    println("==== isSorted")
    def gt(a: Int, b: Int): Boolean = a < b
    println(isSorted(Array(), gt))
    println(isSorted(Array(1), gt))
    println(isSorted(Array(1, 2), gt))
    println(isSorted(Array(1, 2, 3), gt))
    println(isSorted(Array(9, 1, 2, 3), gt))

    println("==== partial")
    def greeting(gr: String, name: String): Unit = {
      println(gr + " " + name)
    }
    val toWhom = partial1("greeting", greeting)
    toWhom("simshi")
    toWhom("star")

    println("==== curry")
    val greetingTo = curry(greeting)("greeting")
    val helloTo = curry(greeting)("hello")
    greetingTo("Simone")
    helloTo("Simone")
  }

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, n * acc)

    go(n, 1)
  }

  def fibAtNth(n: Int): Int = {
    @annotation.tailrec
    def fib(a: Int, b: Int, cur: Int): Int = {
      if (cur >= n) b
      else fib(b, a + b, cur + 1)
    }

    if (n <= 1) 0
    else fib(0, 1, 2)
  }

  // `T: Ordered` doesn't work 'cause Int is not a subclass of Ordered[Int]
  def binarySearch[T <% Ordered[T]](as: Array[T], v: T): Int = {
    @annotation.tailrec
    def go(low: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val mid = (low + high) / 2

        if (v < as(mid)) go(low, mid - 1)
        else if (as(mid) < v) go(mid + 1, high)
        else mid
      }
    }

    go(0, as.length - 1)
  }

  // view bound is deprecated since Scala 2.10, this is the de-sugar version
  def binarySearch_1[T](as: Array[T], v: T)(implicit ord: Ordering[T]): Int = {
    @annotation.tailrec
    def go(low: Int, high: Int): Int = {
      if (low > high) -1
      else {
        val mid = (low + high) / 2

        if (ord.lt(v, as(mid))) go(low, mid - 1)
        else if (ord.lt(as(mid), v)) go(mid + 1, high)
        else mid
      }
    }

    go(0, as.length - 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else ordered(as(n), as(n + 1)) && go(n + 1)
    }

    go(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }
}
