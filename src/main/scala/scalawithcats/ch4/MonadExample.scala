package scalawithcats.ch4

import cats.Monad
import cats.implicits.{catsSyntaxEitherId, toBifunctorOps}
import scalawithcats.Utils.p

trait MonadExample

object MainMonadExample extends App {
  import cats.instances.future._ // for Monad
  import scala.concurrent._
  import scala.concurrent.duration._

  import scala.concurrent.ExecutionContext.Implicits.global

  val fm = Monad[Future]

  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))
  val res = Await.result(future, 1.second)
  // res4: Int = 3
  println(res)

  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad
  import cats.syntax.applicative._ // for pure

  val res1 = 1.pure[Option]
  // res5: Option[Int] = Some(1)
  val res2 = 1.pure[List]
  // res6: List[Int] = List(1)
  println(res1, res2)

  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x * x + y * y))

  import cats.instances.option._ // for Monad
  import cats.instances.list._ // for Monad

  val res3 = sumSquare(Option(3), Option(4))
  // res7: Option[Int] = Some(25)
  val res4 = sumSquare(List(1, 2, 3), List(4, 5))
  // res8: List[Int] = List(17, 26, 20, 29, 25, 34)
  println(res3, res4)

  def sumSquare1[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x * x + y * y

  val res5 = sumSquare1(Option(3), Option(4))
  // res10: Option[Int] = Some(25)
  val res6 = sumSquare1(List(1, 2, 3), List(4, 5))
  // res11: List[Int] = List(17, 26, 20, 29, 25, 34)
  println(res5, res6)

  import cats.Id
//  sumSquare(3, 4)
  p(sumSquare(3: Id[Int], 4: Id[Int]))

//  def countPositive(nums: List[Int]) =
//    nums.foldLeft(Right(0)) { (accumulator, num) =>
//      if (num > 0) {
//        accumulator.map(_ + 1)
//      } else {
//        Left("Negative. Stopping!")
//      }
//    }

  def countPositive(nums: List[Int]) =
    nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
      if (num > 0) {
        accumulator.map(_ + 1)
      } else {
        Left("Negative. Stopping!")
      }
    }
  val res7 = countPositive(List(1, 2, 3))
  // res5: Either[String, Int] = Right(3)
  val res8 = countPositive(List(1, -2, 3))
  // res6: Either[String, Int] = Left("Negative. Stopping!")
  p(res7, res8)

//  Either.catchOnly[NumberFormatException]("foo".toInt)
  // res7: Either[NumberFormatException, Int] = Left(
  // java.lang.NumberFormatException: For input string: "foo"
  // )
//  Either.catchNonFatal(sys.error("Badness"))
  // res8: Either[Throwable, Nothing] = Left(java.lang.RuntimeException:
//  Badness
//  )

//  import cats.syntax.either._



  val res10 = "foo".asLeft[Int].leftMap(_.reverse)
  // res16: Either[String, Int] = Left("oof")
  val res11 = 6.asRight[String].bimap(_.reverse, _ * 7)
  // res17: Either[String, Int] = Right(42)
  val res12 = "bar".asLeft[Int].bimap(_.reverse, _ * 7)
  // res18: Either[String, Int] = Left("rab")
  p(res10)
  p(res11)
  p(res12)

  val res13 = for {
    a <- 1.asRight[String]
    b <- 0.asRight[String]
    c <-
      if (b == 0) "DIV0".asLeft[Int]
      else (a / b).asRight[String]
  } yield c * 100
  // res21: Either[String, Int] = Left("DIV0")

  p(res13)

  //4.4.4
  object wrapper {
    sealed trait LoginError extends Product with Serializable

    final case class UserNotFound(username: String)
      extends LoginError

    final case class PasswordIncorrect(username: String)
      extends LoginError

    case object UnexpectedError extends LoginError
  };

  import wrapper._

  case class User(username: String, password: String)

  type LoginResult = Either[LoginError, User]

  // Choose error-handling behaviour based on type:
  def handleError(error: LoginError): Unit =
    error match {
      case UserNotFound(u) =>
        println(s"User not found: $u")
      case PasswordIncorrect(u) =>
        println(s"Password incorrect: $u")
      case UnexpectedError =>
        println(s"Unexpected error")
    }

  val result1: LoginResult = User("dave", "passw0rd").asRight
  p(result1)
  // result1: LoginResult = Right(User("dave", "passw0rd"))
  val result2: LoginResult = UserNotFound("dave").asLeft
  // result2: LoginResult = Left(UserNotFound("dave"))
  p(result2)

  result1.fold(handleError, println)
  // User(dave,passw0rd)
  result2.fold(handleError, println)
  // User not found: dave



}
