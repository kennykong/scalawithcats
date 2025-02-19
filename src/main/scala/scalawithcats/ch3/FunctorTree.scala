package scalawithcats.ch3

import cats.Functor
import cats.implicits.toFunctorOps

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      def map[A, B](tree: Tree[A])(func: A => B): Tree[B] =
        tree match {
          case Branch(left, right) =>
            Branch(map(left)(func), map(right)(func))
          case Leaf(value) =>
            Leaf(func(value))
        }
    }

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)
}

object FunctorTreeMain extends App {

//  Branch(Leaf(10), Leaf(20)).map(_ * 2)

  val res9 = Tree.leaf(100).map(_ * 2)
  // res9: Tree[Int] = Leaf(200)
  println(res9.getClass.getName)
  println(res9)
  val res10 =Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2)
  // res10: Tree[Int] = Branch(Leaf(20), Leaf(40))
  println(res10.getClass.getName)
  println(res10)
}
