package scalawithcats.ch10

import scalawithcats.Utils.p

object KleisliExample extends App{
  // 10.5 kleisli
  import cats.data.Kleisli
  import cats.instances.list._ // for Monad
  val step1: Kleisli[List, Int, Int] =
    Kleisli(x => List(x + 1, x - 1))
  val step2: Kleisli[List, Int, Int] =
    Kleisli(x => List(x, -x))
  val step3: Kleisli[List, Int, Int] =
    Kleisli(x => List(x * 2, x / 2))

  val pipeline = step1 andThen step2 andThen step3
}
