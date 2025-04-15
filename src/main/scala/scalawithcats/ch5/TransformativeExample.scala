package scalawithcats.ch5

import scalawithcats.Utils._

object TransformativeExample extends App {


  import cats.data.OptionT

  type ListOption[A] = OptionT[List, A]

  import cats.instances.list._

  val res1: ListOption[Int] = OptionT(List(Option(10)))

  p1(res1)

  import cats.syntax.applicative._

  val res2: ListOption[Int] = 32.pure[ListOption]

  p5(res2)
  
  val res3 = res1.flatMap { (x: Int) =>
    res2.map { (y: Int) =>
      x + y
    }
  }

  p1(res3)
}
