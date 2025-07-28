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

  // There is only one law for Semigroupal: the product method must be associative.
  //  product(a, product(b, c)) == product(product(a, b), c)

  //6.2 Apply Syntax

  import cats.instances.option._ // for Semigroupal
  import cats.syntax.apply._
  // for tupled and mapN

  val res8 = (Option(123), Option("abc")).tupled
  // res8: Option[(Int, String)] = Some((123, "abc"))
  p1(res8)

  val res9 = (Option(123), Option("abc"), Option(true)).tupled
  // res9: Option[(Int, String, Boolean)] = Some((123, "abc", true))
  p1(res9)

  final case class Cat(name: String, born: Int, color: String)

  val res10 = (
    Option("Garfield"),
    Option(1978),
    Option("Orange & black")
  ).mapN(Cat.apply)
  // res10: Option[Cat] = Some(Cat("Garfield", 1978, "Orange & black"))
  p1(res10)

  val add: (Int, Int) => Int = (a, b) => a + b
  // add: (Int, Int) => Int = <function2>
  p1(add)
  //  (Option(1), Option(2), Option(3)).mapN(add)
  // error: ':' expected but '(' found.
  // Option("Garfield"),
  // error: identifier expected but '}' found.
  //  (Option("cats"), Option(true)).mapN(add)
  // error: ':' expected but '(' found.
  // Option("Garfield"),
  // error: identifier expected but '}' found.

  //6.2.1
  //Fancy Functors and Apply Syntax

  import cats.Monoid
  import cats.instances.int._
  // for Monoid
  import cats.instances.invariant._
  // for Semigroupal
  import cats.instances.list._
  // for Monoid
  import cats.instances.string._
  // for Monoid
  import cats.syntax.apply._

  // for imapN
  final case class Cat1(name: String,
                        yearOfBirth: Int,
                        favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat1 =
    Cat1.apply
  val catToTuple: Cat1 => (String, Int, List[String]) =
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)
  implicit val catMonoid: Monoid[Cat1] = (
    Monoid[String],
    Monoid[Int],
    Monoid[List[String]]
  ).imapN(tupleToCat)(catToTuple)

  import cats.syntax.semigroup._ // for |+|

  val garfield = Cat1("Garfield", 1978, List("Lasagne"))
  val heathcliff = Cat1("Heathcliff", 1988, List("Junk Food"))
  val res14 = garfield |+| heathcliff
  // res14: Cat = Cat("GarfieldHeathcliff", 3966, List("Lasagne", "Junk Food"))
  p1(res14)

  //6.3
  //Semigroupal Applied to Different Types

  import cats.Semigroupal
  import cats.instances.future._ // for Semigroupal
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))
  val res00 = Await.result(futurePair, 1.second)
  // res0: (String, Int) = ("Hello", 123)
  p1(res00)

  import cats.syntax.apply._ // for mapN

  case class Cat2(
                   name: String,
                   yearOfBirth: Int,
                   favoriteFoods: List[String])

  val futureCat = (
    Future("Garfield"),
    Future(1978),
    Future(List("Lasagne"))
  ).mapN(Cat2.apply)
  val res01 = Await.result(futureCat, 1.second)
  // res1: Cat = Cat("Garfield", 1978, List("Lasagne"))
  p1(res01)

  import cats.Semigroupal
  import cats.instances.list._ // for Semigroupal

  val res02 = Semigroupal[List].product(List(1, 2), List(3, 4))
  // res2: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))
  p1(res02)

  import cats.instances.either._ // for Semigroupal

  type ErrorOr[A] = Either[Vector[String], A]
  val res03 = Semigroupal[ErrorOr].product(
    Left(Vector("Error 1")),
    Left(Vector("Error 2"))
  )
  // res3: ErrorOr[Tuple2[Nothing, Nothing]] = Left(Vector("Error 1"))
  p1(res03)

  //6.3.1
  //Semigroupal Applied to Monads

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatmap

  def product[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a =>
      fb.map(b =>
        (a, b)
      )
    )

  val a = Future("Future 1")
  val b = Future("Future 2")
  val res04 =
    for {
      x <- a
      y <- b
    } yield (x, y)

  p1(res04)

  val res05 = Semigroupal[List].product(List(1, 2), List(3, 4))
  // res5: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))
  p1(res05)

  val res06 = (List(1, 2), List(3, 4)).tupled
  // res6: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))
  p1(res06)
}
