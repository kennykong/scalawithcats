package scalawithcats.ch4

import cats.Id
import cats.data.WriterT
import scalawithcats.Utils._

object WriteMonadExample extends App {
  import cats.data.Writer
  import cats.instances.vector._ // for Monoid
  val w = Writer(
    Vector(
      "It was the best of times",
      "it was the worst of times"
    ),
    1859
  )
  // res0: cats.data.WriterT[cats.package.Id, Vector[String], Int] = WriterT(
  // (Vector("It was the best of times", "it was the worst of times"), 1859)
  // )
  p2(w)

  import cats.instances.vector._ // for Monoid
  import cats.syntax.applicative._ // for pure
  type Writer[W, A] = WriterT[Id, W, A]
  type Logged[A] = Writer[Vector[String], A]
  val res1 = 123.pure[Logged]
  // res1: Logged[Int] = WriterT((Vector(), 123))
  p1(res1)

  import cats.syntax.writer._ // for tell
  val res2 = Vector("msg1", "msg2", "msg3").tell
  // res2: Writer[Vector[String], Unit] = WriterT(
  // (Vector("msg1", "msg2", "msg3"), ())
  // )
  p1(res2)

  import cats.syntax.writer._ // for writer

  val a = Writer(Vector("msg1", "msg2", "msg3"), 123)
  // a: cats.data.WriterT[cats.package.Id, Vector[String], Int] =
//  WriterT(
  // (Vector("msg1", "msg2", "msg3"), 123)
  // )
  p3(a)
  val b = 123.writer(Vector("msg1", "msg2", "msg3"))
  // b: Writer[Vector[String], Int] = WriterT(
  // (Vector("msg1", "msg2", "msg3"), 123)
  // )
  p1(b)

  val aResult: Int = a.value
  // aResult: Int = 123
  p1(aResult)
  val aLog: Vector[String] = a.written
  // aLog: Vector[String] = Vector("msg1", "msg2", "msg3")
  p1(aLog)
  val (log, result) = b.run
  // log: Vector[String] = Vector("msg1", "msg2", "msg3")
  // result: Int = 123
  p(log, result)

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b
  // writer1: cats.data.WriterT[cats.package.Id, Vector[String], Int] =
//  WriterT(
  // (Vector("a", "b", "c", "x", "y", "z"), 42)
  // )
  val res3 = writer1.run
  // res3: (Vector[String], Int) = (Vector("a", "b", "c", "x", "y", "z")
//    , 42)
  p1(res3)

  val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
  // writer2: cats.data.WriterT[cats.package.Id, Vector[String], Int] =
//  WriterT(
  // (Vector("A", "B", "C", "X", "Y", "Z"), 42)
  // )
  val res4 = writer2.run
  // res4: (Vector[String], Int) = (Vector("A", "B", "C", "X", "Y", "Z")
//  , 42
//  )
  p1(res4)

  val writer3 = writer1.bimap(
    log => log.map(_.toUpperCase),
    res => res * 100
  )
  // writer3: cats.data.WriterT[cats.package.Id, Vector[String], Int] =
//  WriterT(
  // (Vector("A", "B", "C", "X", "Y", "Z"), 4200)
  // )
  val res5 = writer3.run
  // res5: (Vector[String], Int) = (Vector("A", "B", "C", "X", "Y", "Z")
//    , 4200)
  p1(res5)
  val writer4 = writer1.mapBoth { (log, res) =>
    val log2 = log.map(_ + "!")
    val res2 = res * 1000
    (log2, res2)
  }
  // writer4: cats.data.WriterT[cats.package.Id, Vector[String], Int] =
//  WriterT(
  // (Vector("a!", "b!", "c!", "x!", "y!", "z!"), 42000)
  // )
  val res6 = writer4.run
  // res6: (Vector[String], Int) = (
  // Vector("a!", "b!", "c!", "x!", "y!", "z!"),
  // 42000
  // )
  p1(res6)

  val writer5 = writer1.reset
  // writer5: cats.data.WriterT[cats.package.Id, Vector[String], Int] =
//  WriterT(
  // (Vector(), 42)
  // )
  val res7 = writer5.run
  // res7: (Vector[String], Int) = (Vector(), 42)
  p1(res7)
  val writer6 = writer1.swap
  // writer6: cats.data.WriterT[cats.package.Id, Int, Vector[String]] =
//  WriterT(
  // (42, Vector("a", "b", "c", "x", "y", "z"))
  // )
  val res8 = writer6.run
  // res8: (Int, Vector[String]) = (42, Vector("a", "b", "c", "x", "y",
//  "z"
//  ) )
  p(res8)

  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
    println(s"fact $n $ans")
    ans
  }

  val res9 = factorial(5)
  // fact 0 1
  // fact 1 1
  // fact 2 2
  // fact 3 6
  // fact 4 24
  // fact 5 120
  // res9: Int = 120
  p1(res9)

  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits._
  import scala.concurrent.duration._

  val res10 = Await.result(
    Future.sequence(
      Vector(
        Future(factorial(5)),
        Future(factorial(5))
      )
    ),
    5.seconds
  )
  // fact 0 1
  // fact 0 1
  // fact 1 1
  // fact 1 1
  // fact 2 2
  // fact 2 2
  // fact 3 6
  // fact 3 6
  // fact 4 24
  // fact 4 24
  // fact 5 120
  // fact 5 120
  // res: scala.collection.immutable.Vector[Int] =
  // Vector(120, 120)

  p1(res10)

}
