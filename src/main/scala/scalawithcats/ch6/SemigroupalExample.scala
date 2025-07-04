package scalawithcats.ch6

import scalawithcats.Utils.p1

object SemigroupalExample extends App {

  // context2 is dependent on value1:
  //  context1.flatMap(value1 => context2)

  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal
  val res1 = Semigroupal[Option].product(Some(123), Some("abc"))
  // res1: Option[(Int, String)] = Some((123, "abc"))
  p1(res1)

  val res2 = Semigroupal[Option].product(None, Some("abc"))
  // res2: Option[Tuple2[Nothing, String]] = None
  p1(res2)
  val res3 = Semigroupal[Option].product(Some(123), None)
  // res3: Option[Tuple2[Int, Nothing]] = None
  p1(res3)

  import cats.instances.option._ // for Semigroupal
  val res4 = Semigroupal.tuple3(Option(1), Option(2), Option(3))
  // res4: Option[(Int, Int, Int)] = Some((1, 2, 3))
  p1(res4)
  val res5 = Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
  // res5: Option[(Int, Int, Int)] = None
  p1(res5)
  val res6 = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
  // res6: Option[Int] = Some(6)
  p1(res6)
  val res7 = Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
  // res7: Option[Int] = None
  p1(res7)

  //  product(a, product(b, c)) == product(product(a, b), c)
}
