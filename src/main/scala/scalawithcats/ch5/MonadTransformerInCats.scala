package scalawithcats.ch5

import cats.Id
import cats.data.{OptionT, ReaderT, StateT, WriterT}
//import cats.implicits.catsSyntaxApplicativeId
import scalawithcats.Utils._

object MonadTransformerInCats extends App {

  //5.3.2

  type ListOption[A] = OptionT[List, A]

  // Alias Either to a type constructor with one parameter:
  type ErrorOr[A] = Either[String, A]
  // Build our final monad stack using OptionT:
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  import cats.syntax.applicative._
  import cats.instances.either._ // for Monad

  val a = 10.pure[ErrorOrOption]
  // a: ErrorOrOption[Int] = OptionT(Right(Some(10)))
  p1(a)
  val b = 32.pure[ErrorOrOption]
  // b: ErrorOrOption[Int] = OptionT(Right(Some(32)))
  p1(b)
  val c = a.flatMap(x => b.map(y => x + y))
  // c: OptionT[ErrorOr, Int] = OptionT(Right(Some(42)))
  p1(c)

  import scala.concurrent.Future
  import cats.data.{EitherT, OptionT}

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  import cats.instances.future._ // for Monad
  import scala.concurrent.Await
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val futureEitherOr: FutureEitherOption[Int] =
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b
  p1(futureEitherOr)

  //5.3.3

  // Create using apply:
  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  // errorStack1: OptionT[ErrorOr, Int] = OptionT(Right(Some(10)))
  p1(errorStack1)
  // Create using pure:
  val errorStack2 = 32.pure[ErrorOrOption]
  // errorStack2: ErrorOrOption[Int] = OptionT(Right(Some(32)))
  p1(errorStack2)

  // Extracting the untransformed monad stack:
  val res4 = errorStack1.value
  // res4: ErrorOr[Option[Int]] = Right(Some(10))
  p1(res4)
  // Mapping over the Either in the stack:
  val res5 =errorStack2.value.map(_.getOrElse(-1))
  // res5: Either[String, Int] = Right(32)
  p1(res5)

  p1(futureEitherOr)
  // res6: FutureEitherOption[Int] = OptionT(
  // EitherT(Future(Success(Right(Some(42)))))
  // )
  val intermediate = futureEitherOr.value
  // intermediate: FutureEither[Option[Int]] = EitherT(
  // Future(Success(Right(Some(42))))
  // )
  p1(intermediate)
  val stack = intermediate.value
  // stack: Future[Either[String, Option[Int]]] = Future(Success(Right(Some(42))))
  p1(stack)
  val res7 = Await.result(stack, 1.second)
  // res7: Either[String, Option[Int]] = Right(Some(42))
  p1(res7)

  //5.3.4
  type Reader[E, A] = ReaderT[Id, E, A] // = Kleisli[Id, E, A]
  type Writer[W, A] = WriterT[Id, W, A]
  type State[S, A] = StateT[Id, S, A]

  import cats.data.Writer
  type Logged[A] = Writer[List[String], A]
  // Methods generally return untransformed stacks:
  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(num) => Writer(List(s"Read $str"), Some(num))
      case None
      => Writer(List(s"Failed on $str"), None)
    }
  // Consumers use monad transformers locally to simplify composition:
  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT
    val result = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c
    result.value
  }
  // This approach doesn't force OptionT on other users' code:
  val result1 = addAll("1", "2", "3")
  // result1: Logged[Option[Int]] = WriterT(
  // (List("Read 1", "Read 2", "Read 3"), Some(6))
  // )
  p1(result1)
  val result2 = addAll("1", "a", "3")
  // result2: Logged[Option[Int]] = WriterT(
  // (List("Read 1", "Failed on a"), None)
  // )
  p1(result2)
}
