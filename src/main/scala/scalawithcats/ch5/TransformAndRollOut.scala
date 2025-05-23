package scalawithcats.ch5

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scalawithcats.Utils._

object TransformAndRollOut extends App {

  //  type Response[A] = Future[Either[String, A]]


  import cats.data.EitherT
  import scala.concurrent.Future

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  import cats.instances.future._ // for Monad
  import scala.concurrent.ExecutionContext.Implicits.global

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(value) => EitherT.right(Future(value))
      case None => EitherT.left(Future(s"$autobot unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield {
      (power1 + power2) > 15
    }

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match {
      case Left(msg) =>
        s"Common error:$msg"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge!"
    }
  }

  val res13 = tacticalReport("Jazz", "Bumblebee")
  p1(res13)
  // res13: String = "Jazz and Bumblebee need a recharge."
  val res14 = tacticalReport("Bumblebee", "Hot Rod")
  p1(res14)
  // res14: String = "Bumblebee and Hot Rod are ready to roll out!"
  val res15 = tacticalReport("Jazz", "Ironhide")
  p1(res15)
  // res15: String = "Comms error: Ironhide unreachable"

}
