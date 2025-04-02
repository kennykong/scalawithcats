package scalawithcats.ch4

import cats.Monad

import scala.annotation.tailrec

import scalawithcats.Utils._

/**
 * 4.10.1
 * Exercise: Branching out Further with Monads
 */


sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  val treeMonad = new Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
        case Leaf(a) => f(a)
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Leaf(value)
      }
  }

  implicit val treeMonad1 = new Monad[Tree] {

    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] =
      fa match {
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
        case Leaf(a) => f(a)
      }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Option[Tree[B]]]): List[Tree[B]] =
        open match {
          case Branch(l, r) :: next => loop(l :: r :: next, None :: closed)
          case Leaf(Left(value)) :: next => loop(f(value) :: next, closed)
          case Leaf(Right(value)) :: next => loop(next, Some(pure(value)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree.map(_ :: acc).getOrElse {
                val left :: right :: tail = acc
                branch(left, right) :: tail
              }
            }
        }

      loop(List(f(a)), Nil).head
    }
  }
}

object FurtherWithMonads extends App {

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  val res5 = Tree.branch(Tree.leaf(100), Tree.leaf(200)).flatMap(x => Tree.branch(Tree.leaf(x - 1), Tree.leaf(x + 1)))
  // res5: Tree[Int] = Branch(Branch(Leaf(99), Leaf(101)),Branch(Leaf(199), Leaf(201)))
  p1(res5)

  val res6 = for {
    a <- Tree.branch(Tree.leaf(100), Tree.leaf(200))
    b <- Tree.branch(Tree.leaf(a - 10), Tree.leaf(a + 10))
    c <- Tree.branch(Tree.leaf(b - 1), Tree.leaf(b + 1))
  } yield c
  // res6: Tree[Int] = Branch(
  // Branch(Branch(Leaf(89), Leaf(91)), Branch(Leaf(109), Leaf(111))),
  // Branch(Branch(Leaf(189), Leaf(191)), Branch(Leaf(209), Leaf(211)))
  // )
  p1(res6)

  p4(res6)

  p5(res6)

}
