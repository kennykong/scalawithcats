package scalawithcats.ch4

import cats.implicits.toFunctorOps
import scalawithcats.Utils._

import scala.Int

object MonadExample1 extends App {

  // 4.5.1

  import cats.MonadError
  import cats.instances.either._ // for MonadError

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  val success = monadError.pure(42)
  // success: ErrorOr[Int] = Right(42)
  p(success)
  p1(success)
  p2(success)
  p3(success)
  println()

  val failure = monadError.raiseError("Badness")
  // failure: ErrorOr[Nothing] = Left("Badness")
  p(failure)
  p1(failure)
  p2(failure)
  p3(failure)
  println()

  val res21 = monadError.handleErrorWith(failure) {
    case "Badness" =>
      monadError.pure("It's ok")
    case _ =>
      monadError.raiseError("It's not ok")
  }
  // res0: ErrorOr[String] = Right("It's ok")
  p(res21)

  val res22 = monadError.handleError(failure.as(1)) {
    case "Badness" => 42
    case _         => -1
  }
  // res1: ErrorOr[Int] = Right(42)
  p(res22)

  val res23 = monadError.ensure(success)("Number too low!")(_ > 1000)
  // res2: ErrorOr[Int] = Left("Number too low!")
  p(res23)
  import cats.syntax.applicative._ // for pure
  import cats.syntax.applicativeError._ // for raiseError etc
  import cats.syntax.monadError._ // for ensure

  val success1 = 42.pure[ErrorOr]
  // success: ErrorOr[Int] = Right(42)
  val failure1 = "Badness".raiseError[ErrorOr, Int]
  // failure: ErrorOr[Int] = Left("Badness")
  val res24 = failure1.handleErrorWith {
    case "Badness" =>
      256.pure
    case _ =>
      ("It's not ok").raiseError
  }
  // res4: ErrorOr[Int] = Right(256)
  p(res24)
  val res25 = success1.ensure("Number to low!")(_ > 1000)
  // res5: ErrorOr[Int] = Left("Number to low!")
  p(res25)

  import scala.util.Try
  import cats.instances.try_._ // for MonadError

  val exn: Throwable =
    new RuntimeException("It's all gone wrong")
  val res26 = exn.raiseError[Try, Int]
  // res6: Try[Int] = Failure(java.lang.RuntimeException: It's all gone wrong)
  p(res26)

  def validateAdult[F[_]](age: Int)(implicit
      me: MonadError[F, Throwable]
  ): F[Int] = {

    if (age >= 18) age.pure[F]
    else
      new IllegalArgumentException("Age must be greater than or equal to 18")
        .raiseError[F, Int]
  }

  val res27 = validateAdult[Try](18)
  // res7: Try[Int] = Success(18)
  p(res27)
  val res28 = validateAdult[Try](8)
  // res8: Try[Int] = Failure(
  // java.lang.IllegalArgumentException: Age must be greater than or
//  equal to 18
  // )
  p(res28)
  type ExceptionOr[A] = Either[Throwable, A]
  val res29 = validateAdult[ExceptionOr](-1)
  // res9: ExceptionOr[Int] = Left(
  // java.lang.IllegalArgumentException: Age must be greater than or
//  equal to 18
  // )
  p(res29)
  p1(res29)
  p2(res29)
}
