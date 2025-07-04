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
}
