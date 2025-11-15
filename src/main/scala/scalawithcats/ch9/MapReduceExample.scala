package scalawithcats.ch9

import cats.syntax.semigroup._
//import cats.implicits.catsSyntaxSemigroup
import cats.kernel.Monoid

import scalawithcats.Utils.p

object MapReduceExample extends App {

  def foldMap[A, B: Monoid](values: Vector[A])(func: A => B): B =
    values.map(func).foldLeft(Monoid[B].empty)(_ |+| _)

  def foldMap1[A, B: Monoid](as: Vector[A])(func: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| func(_))

  import cats.instances.int._ // for Monoid

  val res1 = foldMap(Vector(1, 2, 3))(identity)
  // res1: Int = 6
  p(res1)

  val res101 = foldMap1(Vector(1, 2, 3))(identity)
  p(res101)

  import cats.instances.string._ // for Monoid

  // Mapping to a String uses the concatenation monoid:
  val res2 = foldMap(Vector(1, 2, 3))(_.toString + "! ")
  // res2: String = "1! 2! 3! "
  p(res2)

  val res21 = foldMap1(Vector(1, 2, 3))(_.toString + "! ")
  p(res21)

  // Mapping over a String to produce a String:
  val res3 = foldMap("Hello world!".toVector)(_.toString.toUpperCase)
  p(res3)
  // res3: String = "HELLO WORLD!"

  val res31 = foldMap1("Hello world!".toVector)(_.toString.toUpperCase)
  p(res31)

  import scala.concurrent.Future
  import scala.concurrent.ExecutionContext.Implicits.global

  val future1 = Future {
    (1 to 100).toList.foldLeft(0)(_ + _)
  }
  // future1: Future[Int] = Future(Success(5050))
  p(future1)

  val future2 = Future {
    (100 to 200).toList.foldLeft(0)(_ + _)
  }
  Thread.sleep(1000)
  // future2: Future[Int] = Future(Success(15150))
  p(future2)

  val future3 = future1.map(_.toString)
  // future3: Future[String] = Future(Success(5050))
  p(future3)
  val future4 = for {
    a <- future1
    b <- future2
  } yield a + b
  Thread.sleep(1000)
  // future4: Future[Int] = Future(Success(20200))
  p(future4)

  val res6 = Future.sequence(List(Future(1), Future(2), Future(3)))
  // res6: Future[List[Int]] = Future(Success(List(1, 2, 3)))
  p(res6)

  import cats.instances.future._ // for Applicative
  import cats.instances.list._
  // for Traverse
  import cats.syntax.traverse._

  // for sequence
  val res7 = List(Future(1), Future(2), Future(3)).sequence
  // res7: Future[List[Int]] = Future(Success(List(1, 2, 3)))
  p(res7)

  import scala.concurrent._
  import scala.concurrent.duration._

  val res8 = Await.result(Future(1), 1.second) // wait for the result
  // res8: Int = 1
  p(res8)

  import cats.{Monad, Monoid}
  import cats.instances.int._
  // for Monoid
  import cats.instances.future._ // for Monad and Monoid

  val res9 = Monad[Future].pure(42)
  p(res9)
  val res10 = Monoid[Future[Int]].combine(Future(1), Future(2))
  p(res10)

  val res11 = Runtime.getRuntime.availableProcessors
  // res11: Int = 2
  p(res11)

  val res12 = (1 to 10).toList.grouped(3).toList
  // res12: List[List[Int]] = List(
  // List(1, 2, 3),
  // List(4, 5, 6),
  // List(7, 8, 9),
  // List(10)
  // )
  p(res12)


  import cats.Monoid
  import cats.instances.int._
  // for Monoid
  import cats.instances.future._ // for Applicative and Monad
  import cats.instances.vector._ // for Foldable and Traverse
  import cats.syntax.foldable._
  // for combineAll and foldMap
  import cats.syntax.traverse._
  // for traverse
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  def parallelFoldMap[A, B: Monoid]
  (values: Vector[A])
  (func: A => B): Future[B] = {
    val numCores
    = Runtime.getRuntime.availableProcessors
    val groupSize = (1.0 * values.size / numCores).ceil.toInt
    values
      .grouped(groupSize)
      .toVector
      .traverse(group => Future(group.foldMap(func)))
      .map(_.combineAll)
  }
0
  val future: Future[Int] =
    parallelFoldMap((1 to 1000).toVector)(_ * 1000)
  val res18 = Await.result(future, 1.second)
  // res18: Int = 500500000
  p(res18)
}
