package scalawithcats.ch3

import cats.Functor

case class FunctorExample() {

}

object FunctorExample {


}

object FunctorExMain extends App {

  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for map

  val func1: Int => Double =
    (x: Int) => x.toDouble
  val func2: Double => Double =
    (y: Double) => y * 2
  val res1 = (func1 map func2)(1) // composition using map
  // res3: Double = 2.0 // composition using map
  val res2 = (func1 andThen func2)(1) // composition using andThen
  // res4: Double = 2.0 // composition using andThen
  val res3 = func2(func1(1)) // composition written out by hand
  // res5: Double = 2.0

  println(res1, res2, res3)

  val res4 = List(1, 2, 3).
    map(n => n + 1).
    map(n => n * 2).
    map(n => s"${n}!")
  // res1: List[String] = List("4!", "6!", "8!")
  println(res4)


  import scala.concurrent.{Future, Await}
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val future: Future[String] =
    Future(123).
      map(n => n + 1).
      map(n => n * 2).
      map(n => s"${n}!")
  Await.result(future, 1.second)
  // res2: String = "248!"

  println(future)

  val func =
    ((x: Int) => x.toDouble).
      map(x => x + 1).
      map(x => x * 2).
      map(x => s"${x}!")
  val res6 = func(123)

  println(res6)

  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for map

  val func11 = (a: Int) => a + 1
  val func12 = (a: Int) => a * 2
  val func13 = (a: Int) => s"${a}!"
  val func14 = func11.map(func12).map(func13)
  println(func14(123))
  // res3: String = "248!"

  def doMath[F[_]](start: F[Int])
                  (implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  import cats.instances.option._ // for Functor
  import cats.instances.list._ // for Functor

  println(doMath(Option(20)))
  // res4: Option[Int] = Some(22)
  println(doMath(List(1, 2, 3)))
  // res5: List[Int] = List(3, 4, 5)

  implicit val optionFunctor: Functor[Option] =
    new Functor[Option] {
      def map[A, B](value: Option[A])(func: A => B): Option[B] =
        value.map(func)
    }

  import scala.concurrent.{Future, ExecutionContext}

/**
  implicit def futureFunctor(implicit ec: ExecutionContext): Functor[Future] =
    new Functor[Future] {
      def map[A, B](value: Future[A])(func: A => B): Future[B] =
        value.map(func)
    }

  // We write this:
  Functor[Future]
  // The compiler expands to this first:
  Functor[Future](futureFunctor)
  // And then to this:
  Functor[Future](futureFunctor(executionContext))

 */
}
