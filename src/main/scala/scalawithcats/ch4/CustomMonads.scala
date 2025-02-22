package scalawithcats.ch4


import scala.annotation.tailrec
import scalawithcats.Utils._

//import scala.collection.IterableOnce

object CustomMonads extends App {

  import cats.Monad

  val optionMonad = new Monad[Option] {

    override def pure[A](opt: A): Option[A] = Some(opt)

    override def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] = opt flatMap fn

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
      f(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(f)
        case Some(Right(b)) => Some(b)
      }
  }

  import cats.syntax.flatMap._ // For flatMap

  def retry[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    f(start).flatMap { a =>
      retry(a)(f)
    }

  import cats.instances.option._

  /**
   * compatible optionMonad
   */
  implicit val optionMonad1 = new Monad[IterableOnce] {

    override def pure[A](x: A): IterableOnce[A] = Some(x)

    override def flatMap[A, B](fa: IterableOnce[A])(f: A => IterableOnce[B]): IterableOnce[B] = fa flatMap f

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => IterableOnce[Either[A, B]]): IterableOnce[B] =
      f(a) match {
        case None => None
        case Some(Left(a1)) => tailRecM(a1)(f)
        case Some(Right(b)) => Some(b)
      }
  }

  val res1 = retry(100)(a => if (a == 0) None else Some(a - 1))
  // res1: Option[Int] = None
  p1(res1)

//  retry(100000)(a => if (a == 0) None else Some(a - 1))
  // KABLOOIE!!!! StackOverflowError

  import cats.syntax.functor._ // for map

  def retryTailRecM[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start) { a =>
      f(a).map(a2 => Left(a2))
    }

  val res2 = retryTailRecM(100000)(a => if(a == 0) None else Some(a - 1))
  // res2: Option[Int] = None
  p1(res2)
}
