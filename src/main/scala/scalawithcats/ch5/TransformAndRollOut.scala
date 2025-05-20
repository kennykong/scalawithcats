package scalawithcats.ch5

import scala.concurrent.Future

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
    ???

  def tacticalReport(ally1: String, ally2: String): String =
    ???

}
