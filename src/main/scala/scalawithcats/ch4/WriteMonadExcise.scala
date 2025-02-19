package scalawithcats.ch4

import scalawithcats.Utils.{p1, p3}
import WriteMonadExample.slowly

case object WriteMonadExcise extends App {

  import cats.data.Writer
  import cats.instances.vector._
  import cats.syntax.applicative._ // for pure

  type Logged[A] = Writer[Vector[String], A]
  val res11 = 42.pure[Logged]
  // res11: Logged[Int] = WriterT((Vector(), 42))
  p1(res11)
  import cats.syntax.writer._ // for tell
  val res12 = Vector("Message").tell
  // res12: Writer[Vector[String], Unit] = WriterT((Vector("Message"),
//  ()))
  p1(res12)

  import cats.instances.vector._ // for Monoid

  val res13 = 41.pure[Logged].map(_ + 1)
  // res13: cats.data.WriterT[cats.package.Id, Vector[String], Int] =
//  WriterT(
  // (Vector(), 42)
  // )
  p3(res13)

  def factorial(n: Int): Logged[Int] =
    for {
      ans <-
        if (n == 0) {
          1.pure[Logged]
        } else {
          slowly(factorial(n - 1).map(_ * n))
        }
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  val (log, res) = factorial(5).run
  // log: Vector[String] = Vector(
  // "fact 0 1",
  // "fact 1 1",
  // "fact 2 2",
  // "fact 3 6",
  // "fact 4 24",
  // "fact 5 120"
  // )
  // res: Int = 120
  p1(log, res)

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  val res21 = Await.result(
    Future
      .sequence(
        Vector(
          Future(factorial(5)),
          Future(factorial(5))
        )
      )
      .map(_.map(_.written)),
    5.seconds
  )
  // res: scala.collection.immutable.Vector[cats.Id[Vector[String]]] =
  // Vector(
  // Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact
//  5
//  120
//  ),
  // Vector(fact 0 1, fact 1 1, fact 2 2, fact 3 6, fact 4 24, fact
//  5
//  120
//  )
  // )
  p1(res21)
}
