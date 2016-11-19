package com.simshi.fp.chapter3

/**
  * Created by simshi on 11/11/16.
  */
sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](lst: List[A]): List[A] = {
    lst match {
      case Nil => Nil
      case Cons(h, t) => t
    }
  }

  def setHead[A](lst: List[A], h: A): List[A] = {
    Cons(h, tail(lst))
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def drop[A](lst: List[A], n: Int): List[A] = {
    lst match {
      case Nil => Nil
      case _ if n <= 0 => lst
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](lst: List[A])(f: A => Boolean): List[A] = {
    lst match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t)(f)
      case _ => lst
    }
  }

  def concat[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, concat(t, a2))
    }

  def foldRight[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(l: List[Int]) = foldRight(l, 0.0)(_ + _)

  def product2(l: List[Double]) = foldRight(l, 1.0)(_ * _)

  def product3(l: List[Double]) =
    foldRight(l, 1.0)((a, z) => if (a == 0.0) 0 else a * z)

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, len) => len + 1)

  def concatR[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def sumL(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productL(l: List[Int]): Int = foldLeft(l, 1)(_ * _)

  def lengthL[A](l: List[A]): Int = foldLeft(l, 0)((len, _) => len + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  def flatten[A](lst: List[List[A]]): List[A] = foldRight(lst, Nil: List[A])(concatR)

  def addOne(ints: List[Int]): List[Int] =
    foldRight(ints, List[Int]())((a, b) => Cons(a + 1, b))

  def double2String(doubles: List[Double]): List[String] =
    foldRight(doubles, List[String]())((a, b) => Cons(a.toString, b))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, List[B]())((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, List[A]())((a, b) => if (f(a)) Cons(a, b) else b)

  def flatMapViaFlatten[A, B](l: List[A])(f: A => List[B]): List[B] = flatten(map(l)(f))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, List[B]())((a, b) => concatR(f(a), b))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def listAddInt(l1: List[Int], l2: List[Int]): List[Int] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, listAddInt(t1, t2))
    }

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] =
    (l1, l2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

  def beginWith[A](l: List[A], sub: List[A]): Boolean = {
    (l, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 != h2) false else beginWith(t1, t2)
    }
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    l match {
      case Nil => false
      case _ if beginWith(l, sub) => true
      case Cons(_, t) => hasSubsequence(t, sub)
    }
  }

  val example = Cons(1, Cons(2, Cons(3, Nil)))
  val example2 = List(1, 2, 3)
  val total = sum(example)

  val e1 = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }
}
