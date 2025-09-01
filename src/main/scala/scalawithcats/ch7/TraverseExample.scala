package scalawithcats.ch7

import scalawithcats.Utils.p

object TraverseExample extends App {

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  val allUptimes: Future[List[Int]] =
    hostnames.foldLeft(Future(List.empty[Int])) {
      (accum, host) =>
        val uptime = getUptime(host)
        for {
          accum <- accum
          uptime <- uptime
        } yield accum :+ uptime
    }
  val res0 = Await.result(allUptimes, 1.second)
  // res0: List[Int] = List(1020, 960, 840)
  p(res0)

  val allUptimes1: Future[List[Int]] =
    Future.traverse(hostnames)(getUptime)
  val res2 = Await.result(allUptimes1, 1.second)
  // res2: List[Int] = List(1020, 960, 840)
  p(res2)

  def traverse[A, B](values: List[A])
                    (func: A => Future[B]): Future[List[B]] =
    values.foldLeft(Future(List.empty[B])) { (accum, host) =>
      val item = func(host)
      for {
        accum <- accum
        item <- item
      } yield accum :+ item
    }

  import cats.Applicative
  import cats.instances.future._
  // for Applicative
  import cats.syntax.applicative._ // for pure

  List.empty[Int].pure[Future]

  def oldCombine(
                  accum: Future[List[Int]],
                  host: String
                ): Future[List[Int]] = {
    val uptime = getUptime(host)
    for {
      accum <- accum
      uptime <- uptime
    } yield accum :+ uptime
  }

  import cats.syntax.apply._ // for mapN

  // Combining accumulator and hostname using an Applicative:
  def newCombine(accum: Future[List[Int]],
                 host: String): Future[List[Int]] =
    (accum, getUptime(host)).mapN(_ :+ _)

  def listTraverse[F[_] : Applicative, A, B]
  (list: List[A])(func: A => F[B]): F[List[B]] =
    list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
      (accum, func(item)).mapN(_ :+ _)
    }

  def listSequence[F[_] : Applicative, B]
  (list: List[F[B]]): F[List[B]] =
    listTraverse(list)(identity)

  val totalUptime = listTraverse(hostnames)(getUptime)
  val res5 = Await.result(totalUptime, 1.second)
  // res5: List[Int] = List(1020, 960, 840)
  p(res5)

  //7.2.2.1 Exercise: Traversing with Vectors

  import cats.instances.vector._ // for Applicative

  val res6 = listSequence(List(Vector(1, 2), Vector(3, 4)))
  p(res6)
  val res7 = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
  p(res7)

  import cats.instances.option._ // for Applicative

  def process(inputs: List[Int]) =
    listTraverse(inputs)(n => if (n % 2 == 0) Some(n) else None)

  val res8 = process(List(2, 4, 6))
  p(res8)
  val res9 = process(List(1, 2, 3))
  p(res9)

  //7.2.2.3 Exercise: Traversing with Validated

  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type ErrorsOr[A] = Validated[List[String], A]

  def process1(inputs: List[Int]): ErrorsOr[List[Int]] =
    listTraverse(inputs) { n =>
      if (n % 2 == 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(List(s"$n is not even"))
      }
    }

  val res10 = process(List(2, 4, 6))
  p(res10)
  val res11 = process(List(1, 2, 3))
  p(res11)

  //7.2.3
  //Traverse in Cats

  /*
  package cats
  trait Traverse[F[_]] {
    def traverse[G[_]: Applicative, A, B]
    (inputs: F[A])(func: A => G[B]): G[F[B]]
    def sequence[G[_]: Applicative, B]
    (inputs: F[G[B]]): G[F[B]] =
      traverse(inputs)(identity)
  }
  */

  import cats.Traverse
  import cats.instances.future._ // for Applicative
  import cats.instances.list._
  // for Traverse
  val totalUptime2: Future[List[Int]] =
    Traverse[List].traverse(hostnames)(getUptime)
  val res12 = Await.result(totalUptime2, 1.second)
  // res0: List[Int] = List(1020, 960, 840)
  p(res12)
  val numbers = List(Future(1), Future(2), Future(3))
  val numbers2: Future[List[Int]] =
    Traverse[List].sequence(numbers)
  val res13 = Await.result(numbers2, 1.second)
  // res1: List[Int] = List(1, 2, 3)
  p(res13)

  import cats.syntax.traverse._ // for sequence and traverse
  val res14 = Await.result(hostnames.traverse(getUptime), 1.second)
  // res2: List[Int] = List(1020, 960, 840)
  p(res14)
  val res15 = Await.result(numbers.sequence, 1.second)
  // res3: List[Int] = List(1, 2, 3)
  p(res15)
}
