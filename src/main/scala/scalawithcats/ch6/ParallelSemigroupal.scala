package scalawithcats.ch6

import scalawithcats.Utils.p1

object ParallelSemigroupal extends App {
  import cats.Semigroupal
  import cats.instances.either._ // for Semigroupal
  type ErrorOr[A] = Either[Vector[String], A]
  val error1: ErrorOr[Int] = Left(Vector("Error 1"))
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))
  val res0 = Semigroupal[ErrorOr].product(error1, error2)
  // res0: ErrorOr[(Int, Int)] = Left(Vector("Error 1"))
  p1(res0)

  import cats.syntax.apply._ // for tupled
  import cats.instances.vector._ // for Semigroup on Vector
  val res1 = (error1, error2).tupled
  // res1: ErrorOr[(Int, Int)] = Left(Vector("Error 1"))
  p1(res1)

  import cats.syntax.parallel._ // for parTupled
  val res2 = (error1, error2).parTupled
  // res2: ErrorOr[(Int, Int)] = Left(Vector("Error 1", "Error 2"))
  p1(res2)

  import cats.instances.list._ // for Semigroup on List
  type ErrorOrList[A] = Either[List[String], A]
  val errStr1: ErrorOrList[Int] = Left(List("error 1"))
  val errStr2: ErrorOrList[Int] = Left(List("error 2"))
  val res3 = (errStr1, errStr2).parTupled
  // res3: ErrorOrList[(Int, Int)] = Left(List("error 1", "error 2"))
  p1(res3)

  val success1: ErrorOr[Int] = Right(1)
  val success2: ErrorOr[Int] = Right(2)
  val addTwo = (x: Int, y: Int) => x + y
  val res4 = (error1, error2).parMapN(addTwo)
  // res4: ErrorOr[Int] = Left(Vector("Error 1", "Error 2"))
  p1(res4)
  val res5 = (success1, success2).parMapN(addTwo)
  // res5: ErrorOr[Int] = Right(3)
  p1(res5)

  import cats.arrow.FunctionK
  object optionToList extends FunctionK[Option, List] {
    def apply[A](fa: Option[A]): List[A] =
      fa match {
        case None
        => List.empty[A]
        case Some(a) => List(a)
      }
  }
  val res6 = optionToList(Some(1))
  // res6: List[Int] = List(1)
  p1(res6)
  val res7 = optionToList(None)
  // res7: List[Nothing] = List()
  p1(res7)


  import cats.instances.list._
  val res8 = (List(1, 2), List(3, 4)).tupled
  // res8: List[(Int, Int)] = List((1, 3), (1, 4), (2, 3), (2, 4))
  p1(res8)
  val res9 =(List(1, 2), List(3, 4)).parTupled
  // res9: List[(Int, Int)] = List((1, 3), (2, 4))
  p1(res9)
}
