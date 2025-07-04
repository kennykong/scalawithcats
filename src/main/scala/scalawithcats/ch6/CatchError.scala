package scalawithcats.ch6

import scalawithcats.Utils._

object CatchError extends App {

  import cats.syntax.either._ // for catchOnly

  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt).
      leftMap(_ => s"Couldn't read $str")

  val res0 = for {
    a <- parseInt("a")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield (a + b + c)
  // res0: Either[String, Int] = Left("Couldn't read a")
  p1(res0)


}
