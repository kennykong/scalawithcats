package scalawithcats.ch1

import cats.effect.{IO, IOApp}

trait Printable[A] {
  def format(value: A): String
}

object PrintableInstances {
  implicit val stringPrinter: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        value
    }

  implicit val intPrinter: Printable[Int] =
    new Printable[Int] {
      def format(value: Int): String =
        value.toString
    }
}

object Printable {
  implicit class PrinterOps[A](value: A) {
    def format(implicit p: Printable[A]): String = {
      p.format(value)
    }

    def print(implicit p: Printable[A]): Unit = {
      println(p.format(value))
    }
  }
}

case class Cat(name: String, age: Int, color: String)
object Cat {

  implicit val catPrinter: Printable[Cat] =
    new Printable[Cat] {
      def format(value: Cat): String = {
        val name = value.name
        val age = value.age
        val color = value.color
        s"$name is a $age year-old $color cat."
      }
    }
}

object Main3 extends IOApp.Simple {

  def run: IO[Unit] = {
    // 2.
    import Printable._
    import PrintableInstances._

    val r1 = Cat("Name", 3, "Color").format
    IO.println(r1)

//    import java.util.Date
//    val r2 =new Date().print
//    IO.println(r2)
  }
}
