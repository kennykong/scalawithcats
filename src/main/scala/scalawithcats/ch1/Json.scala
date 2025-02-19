package scalawithcats.ch1

import cats.effect.{IO, IOApp}

// Define a very simple JSON AST
sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double)
class JsNumber1(get: Double)

final case object JsNull extends Json
// The "serialize to JSON" behaviour is encoded in this trait
trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)
object JsonWriterInstances {
  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }
  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(
          Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          )
        )
    }
  // etc...
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

object Main1 extends App {

  // 1.
  import JsonWriterInstances._

  private val r = Json.toJson(Person("Dave", "dave@example.com"))
  println(r)

}

object Main2 extends IOApp.Simple {

  def run: IO[Unit] = {
    // 2.
    import JsonWriterInstances._
    import JsonSyntax._

    val r1 = Person("Dave1", "dave1@example.com").toJson
    IO.println(r1)

    // 3.
    import JsonWriterInstances._
    val r2 = implicitly[JsonWriter[String]]
    IO.println(r2)


  }
}
