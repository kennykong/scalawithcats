package scalawithcats.ch1

import cats.Show
//import cats.instances._

trait ShowExample

object MainShow extends App {

  import cats.instances.int._ // for Show
  private val showInt = Show.apply[Int]
  println(showInt)


  import cats.instances.string._ // for Show

//  val showInt1: Show[Int] = Show.apply[Int]
  private val showString: Show[String] = Show.apply[String]
//  val showCat: Show[Cat] = Show.apply[Cat]
  private val intAsString: String = showInt.show(123)
  // intAsString: String = "123"
  private val stringAsString: String = showString.show("abc")
  // stringAsString: String = "abc"

  println(intAsString)
  println(stringAsString)

}

object MainShow1 extends App {
  import cats._
  import cats.implicits._

  import java.util.Date

  implicit val dateShow: Show[Date] =
    new Show[Date] {
      def show(date: Date): String =
        s"${date.getTime}ms since the epoch."
    }
  println(new Date().show)

}
