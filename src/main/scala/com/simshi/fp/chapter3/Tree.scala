package com.simshi.fp.chapter3

/**
  * Created by simshi on 11/11/16.
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximumInt(tree: Tree[Int]): Int = {
    tree match {
      case Leaf(v) => v
      case Branch(l, r) => maximumInt(l) max maximumInt(r)
    }
  }

  def depth[A](tree: Tree[A]): Int = {
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(b: (B, B) => B): B = {
    tree match {
      case Leaf(v) => f(v)
      case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
    }
  }

  def sizeF[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ + _ + 1)

  def depthF[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(_ max _ + 1)

  def mapF[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}
