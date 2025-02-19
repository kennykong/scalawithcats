package scalawithcats.ch1

import cats.{Eq, Show}
import cats.implicits.{catsSyntaxEq, toShow}
import cats.instances.int._
import cats.instances.option._

import java.util.Date
import cats.instances.long._ // for Eq

import cats.syntax.show._ // for show

final case class EqExample() {

}

object EqExample {
  implicit val catShow: Show[CatEq] = Show.show[CatEq] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }
}

object MainEq extends App {

  //
//  List(1, 2, 3).map(Option(_)).filter(item => item == 1)
  (Some(1) : Option[Int]) === (None : Option[Int])

  Option(1) === Option.empty[Int]

  import cats.syntax.option._
  1.some === none[Int]
  // res10: Boolean = false
  1.some =!= none[Int]
  // res11: Boolean = true


  implicit val dateEq: Eq[Date] = Eq.instance[Date] { (date1, date2) =>
    date1.getTime === date2.getTime
  }
  val x = new Date() // now
  Thread.sleep(1)
  val y = new Date() // a bit later than now
  println(x === x)
  // res12: Boolean = true
  println(x === y)
  // res13: Boolean = false


}


final case class CatEq(name: String, age: Int, color: String)

object MainEq1 extends App {
  val cat1 = CatEq("Garfield", 38, "orange and black")
  val cat2 = CatEq("Heathcliff", 33, "orange and black")
  implicit val dateEq: Eq[CatEq] = Eq.instance[CatEq] { (cat1, cat2) =>
    (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat2.color)
  }
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[CatEq]
//  implicit val dateEq: Eq[Date] = Eq.instance[Date] { (date1, date2) =>
//    date1.getTime === date2.getTime
//  }
  println(cat1.toString)
  println(cat2.toString)
  println(cat1 === cat2)
  println(cat1 =!= cat2)
  println(optionCat1 === optionCat2)
  println(optionCat1 =!= optionCat2)
}
