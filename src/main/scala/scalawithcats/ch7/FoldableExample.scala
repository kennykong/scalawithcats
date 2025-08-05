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


  import cats.Eval
  import cats.Foldable
  def bigData = (1 to 100000).to(LazyList)

  val res16 = bigData.foldRight(0L)(_ + _)
  // java.lang.StackOverflowError ...
  // this version of Scala do not appear SOE
  p(res16)

  import cats.instances.lazyList._ // for Foldable
  val eval: Eval[Long] =
    Foldable[LazyList].
      foldRight(bigData, Eval.now(0L)) { (num, eval) =>
        eval.map(_ + num)
      }
  val res17 = eval.value
  // res3: Long = 5000050000L
  p(res17)

  //7.1.4.2
  //Folding with Monoids

  val res18 = Foldable[Option].nonEmpty(Option(42))
  // res6: Boolean = true
  p(res18)
  val res19 = Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)
  // res7: Option[Int] = Some(2)
  p(res19)

  import cats.instances.int._ // for Monoid
  val res20 = Foldable[List].combineAll(List(1, 2, 3))
  // res8: Int = 6
  p(res20)

  import cats.instances.string._ // for Monoid
  val res21 = Foldable[List].foldMap(List(1, 2, 3))(_.toString)
  // res9: String = "123"
  p(res21)

  import cats.instances.vector._ // for Monoid
  val ints1 = List(Vector(1, 2, 3), Vector(4, 5, 6))
  val res22 = (Foldable[List] compose Foldable[Vector]).combineAll(ints1)
  // res11: Int = 21
  p(res22)

  import cats.syntax.foldable._ // for combineAll and foldMap
  val res23 = List(1, 2, 3).combineAll
  // res12: Int = 6
  p(res23)

  val res24 = List(1, 2, 3).foldMap(_.toString)
  // res13: String = "123"
  p(res24)
}
