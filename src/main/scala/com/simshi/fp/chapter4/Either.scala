package com.simshi.fp.chapter4

/**
  * Created by simshi on 11/15/16.
  */
sealed trait Either[+E, +A] {
  /**
    * EXERCISE 7
    * Implement versions of map, flatMap, orElse, and map2 on Either that
    * operate on the Right value.
    */
  def map[B](f: A => B): Either[E, B] =
  this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

  def map2[EE >: E, B, C](eb: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {a <- this; b <- eb} yield f(a, b)


}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  /**
    * EXERCISE 8: Implement sequence and traverse for Either.
    */
  def sequence[E, A](l: List[Either[E, A]]): Either[E, List[A]] = l match {
    case Nil => Right(Nil)
    case x :: xs => x flatMap (a => sequence(xs) map (a :: _))
  }

  def traverse[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = l match {
    case Nil => Right(Nil)
    case x :: xs => f(x) flatMap (b => traverse(xs)(f) map (b :: _))
  }

  def traverse_2[E, A, B](l: List[A])(f: A => Either[E, B]): Either[E, List[B]] = l match {
    case Nil => Right(Nil)
    case x :: xs => (f(x) map2 traverse_2(xs)(f)) (_ :: _)
  }

  def sequenceViaTraverse[E, A](l: List[Either[E, A]]): Either[E, List[A]] = traverse(l)(x => x)

  /**
    * EXERCISE 9
    * In this implementation, map2 is only able to report one error, even if
    * both the name and the age are invalid. What would you need to change in
    * order to report both errors? Would you change map2 or the signature of
    * mkPerson? Or could you create a new data type that captures this
    * requirement better than Either does, with some additional structure? How
    * would orElse, traverse, and sequence behave differently for that data type?
    */
  def map2b[E, A, B, C](e1: Either[E, A], e2: Either[E, B])(f: (A, B) => C): Either[List[E], C] =
  (e1, e2) match {
    case (Right(a), Right(b)) => Right(f(a, b))
    case _ => Left(List(e1, e2) collect { case Left(e) => e })
  }
}
