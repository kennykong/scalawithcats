package scalawithcats.ch9

import cats.syntax.semigroup._
//import cats.implicits.catsSyntaxSemigroup
import cats.kernel.Monoid

import scalawithcats.Utils.p

object MapReduceExample extends App{

  def foldMap[A, B:Monoid](values: Vector[A])(func: A => B):B =
    values.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  import cats.instances.int._ // for Monoid
  val res1 = foldMap(Vector(1, 2, 3))(identity)
  // res1: Int = 6
  p(res1)
  import cats.instances.string._ // for Monoid
  // Mapping to a String uses the concatenation monoid:
  val res2 = foldMap(Vector(1, 2, 3))(_.toString + "! ")
  // res2: String = "1! 2! 3! "
  p(res2)
}
