package com.simshi.fp.chapter4

/**
  * Created by simshi on 11/11/16.
  */
sealed trait Option[+A] {
  /**
    * Exercise 1
    * Implement all of the preceding functions on Option. As you implement each
    * function, try to think about what it means and in what situations you’d
    * use it.
    */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def filter(f: A => Boolean): Option[A] = this.flatMap(a => if (f(a)) Some(a) else None)

  def filter_1(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => this
      case _ => None
    }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  /**
    * Exercise 3
    * Write a generic function map2 that combines two Option values using a
    * binary function. If either Option value is None, then the return value is
    * too.
    */
  def map2_0[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {aSome <- a; bSome <- b} yield f(aSome, bSome)

  /**
    * Exercise 5
    * Write a function sequence, that combines a list of Options into one
    * option containing a list of all the Some values in the original list.
    * If the original list contains None even once, the result of the function
    * should be None , otherwise the result should be Some with a list of all
    * the values.
    */
  // clear code but not short-circuit
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]])((a, z) => a match {
      case None => None
      case Some(v) => z.map(v :: _)
    })
  }

  // short-circuit (by recursion)
  def sequence_1[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(x) :: xs => sequence_1(xs).map(x :: _)
  }

  /**
    * EXERCISE 6
    * Implement this function. It is straightforward to do using map and
    * sequence, but try for a more efficient implementation that only looks at
    * the list once. In fact, implement sequence in terms of traverse.
    */
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case x :: xs => f(x) flatMap (b => traverse(xs)(f) map (b :: _))
  }

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}
