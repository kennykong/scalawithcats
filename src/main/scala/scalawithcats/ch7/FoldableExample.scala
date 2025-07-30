package scalawithcats.ch7

import scalawithcats.Utils.p

object FoldableExample extends App {

  def show[A](list: List[A]): String =
    list.foldLeft("nil")((accum, item) => s"$item then $accum")

  def show1[A](list: List[A]): String =
    list.foldRight("nil")((accum, item) => s"$item then $accum")

  val res0 = show(Nil)
  // res0: String = "nil"
  p(res0)
  val res1 = show(List(1, 2, 3))
  // res1: String = "3 then 2 then 1 then nil"
  p(res1)

  val res01 = show1(Nil)
  p(res01)

  val res02 = show1(List(1, 2, 3))
  p(res02)

  val res2 = List(1, 2, 3).foldLeft(0)(_ + _)
  // res2: Int = 6
  p(res2)

  val res3 =List(1, 2, 3).foldRight(0)(_ + _)
  // res3: Int = 6
  p(res3)

  val res6 = List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a)
  // res6: List[Int] = List(3, 2, 1)
  p(res6)

  val res7 = List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a)
  // res7: List[Int] = List(1, 2, 3)
  p(res7)

//  List(1, 2, 3).foldRight(Nil)(_ :: _)
  // error: type mismatch;
  // found : List[Int]
  // required: scala.collection.immutable.Nil.type
  // List(1, 2, 3).foldRight(Nil)(_ :: _)
  //

  def map[A, B](list: List[A])(func: A => B): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) :: accum
    }
  val res9 = map(List(1, 2, 3))(_ * 2)
  // res9: List[Int] = List(2, 4, 6)
  p(res9)

  def flatMap[A, B](list: List[A])(func: A => List[B]): List[B] =
    list.foldRight(List.empty[B]) { (item, accum) =>
      func(item) ::: accum
    }
  val res10 = flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))
  // res10: List[Int] = List(1, 10, 100, 2, 20, 200, 3, 30, 300)
  p(res10)

  def filter[A](list: List[A])(func: A => Boolean): List[A] =
    list.foldRight(List.empty[A]) { (item, accum) =>
      if(func(item)) item :: accum else accum
    }
  val res11 = filter(List(1, 2, 3))(_ % 2 == 1)
  // res11: List[Int] = List(1, 3)
  p(res11)

  import scala.math.Numeric
  def sumWithNumeric[A](list: List[A])
                       (implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)
  val res12 = sumWithNumeric(List(1, 2, 3))
  // res12: Int = 6
  p(res12)

  import cats.Monoid
  def sumWithMonoid[A](list: List[A])
                      (implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)
  import cats.instances.int._ // for Monoid
  val res13 = sumWithMonoid(List(1, 2, 3))
  // res13: Int = 6
  p(res13)

  import cats.Foldable
  import cats.instances.list._ // for Foldable
  val ints = List(1, 2, 3)
  val res14 = Foldable[List].foldLeft(ints, 0)(_ + _)
  // res0: Int = 6
  p(res14)

  import cats.instances.option._ // for Foldable
  val maybeInt = Option(123)
  val res15 = Foldable[Option].foldLeft(maybeInt, 10)(_ * _)
  // res1: Int = 1230
  p(res15)

  //7.1.4.1
  //Folding Right


}
