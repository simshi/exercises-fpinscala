package com.simshi.fp.chapter4

/**
  * Created by simshi on 11/17/16.
  */
sealed trait Validation[+E, +A] {
  def isSuccess: Boolean = this match {
    case Success(_) => true
    case _ => false
  }

  def isFailure: Boolean = !isSuccess

  def map[B](f: A => B): Validation[E, B] = this match {
    case e@Failure(_) => e
    case Success(a) => Success(f(a))
  }

  def orElse[EE >: E, B >: A](b: => Validation[EE, B]): Validation[EE, B] =
    (this, b) match {
      case (a@Success(_), _) => a
      case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
      case (_, b@Success(_)) => b
    }

  def addF[EE >: E, B, C](v: Validation[EE, B])(f: (A, B) => C): Validation[EE, C] =
    (this, v) match {
      case (Failure(e1), Failure(e2)) => Failure(e1 ++ e2)
      case (f@Failure(_), _) => f
      case (Success(a), Success(b)) => Success(f(a, b))
      case (_, f@Failure(_)) => f
    }
}

case class Failure[E, A](errors: List[E]) extends Validation[E, Nothing]

case class Success[E, A](value: A) extends Validation[Nothing, A]

object Validation {
  def sequence[E, A](l: List[Validation[E, A]]): Validation[E, List[A]] = {
    @annotation.tailrec
    def go(lst: List[Validation[E, A]], acc: Validation[E, List[A]]): Validation[E, List[A]] =
      lst match {
        case Nil => acc.map(_.reverse)
        case x :: xs => go(xs, x.addF(acc)(_ :: _))
      }

    go(l, Success(List[A]()))
  }

  def traverse[E, A, B](l: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] = {
    @annotation.tailrec
    def go(lst: List[A], acc: Validation[E, List[B]]): Validation[E, List[B]] = {
      lst match {
        case Nil => acc.map(_.reverse)
        case x :: xs => f(x) match {
          case f@Failure(e) => go(xs, acc match {
            case Failure(es) => Failure(es ++ e)
            case _ => f
          })
          case Success(a) => go(xs, acc match {
            case f@Failure(_) => f
            case Success(as) => Success(a :: as)
          })
        }
      }
    }

    go(l, Success(List[B]()))
  }

  def sequenceViaTraverse[E, A](l: List[Validation[E, A]]): Validation[E, List[A]] = traverse(l)(x => x)

  def traverseR[E, A, B](l: List[A])(f: A => Validation[E, B]): Validation[E, List[B]] =
    l.foldRight(Success(List[B]()): Validation[E, List[B]])((a, acc) => f(a).addF(acc)(_ :: _))

  def sequenceViaTraverseR[E, A](l: List[Validation[E, A]]): Validation[E, List[A]] = traverseR(l)(x => x)
}
