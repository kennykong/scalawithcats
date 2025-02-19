package scalawithcats.ch1

import cats.Show
import cats.instances.int._ // for Show
import cats.instances.string._ // for Show
import cats.syntax.show._ // for show

final case class CatShow(name: String, age: Int, color: String)

object CatShow {
  implicit val catShow: Show[CatShow] = Show.show[CatShow] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }
}

object MainShow2 extends App {
  println(CatShow("Garfield", 38, "ginger and black").show)
  // Garfield is a 38 year-old ginger and black cat.
}