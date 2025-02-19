package scalawithcats.ch4

import cats.implicits.catsSyntaxApplicativeId
import scalawithcats.Utils.p1

object ReaderMonadExample extends App {

  import cats.data.Reader

  final case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)
  // catName: Reader[Cat, String] = Kleisli(<function1>)
  p1(catName)

  val res1 = catName.run(Cat("Garfield", "lasagne"))
  // res1: cats.package.Id[String] = "Garfield"
  p1(res1)

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello ${name}")
  val res2 = greetKitty.run(Cat("Heathcliff", "junk food"))
  // res2: cats.package.Id[String] = "Hello Heathcliff"
  p1(res2)

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")
  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."
  val res3 = greetAndFeed(Cat("Garfield", "lasagne"))
  // res3: cats.package.Id[String] = "Hello Garfield. Have a nice bowl
//  of lasagne
//  ."
  p1(res3)
  val res4 = greetAndFeed(Cat("Heathcliff", "junk food"))
  // res4: cats.package.Id[String] = "Hello Heathcliff. Have a nice bowl
//  of junk food.
//  "
  p1(res4)

  final case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String]
  )

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      username <- findUsername(userId)
      login <- username.map { username =>
        checkPassword(username, password)
      } getOrElse {
        false.pure[DbReader]
      }
    } yield login

  val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")

  val passwords = Map(
    "dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")
  val db = Db(users, passwords)
  p1(db)
  val res7 = checkLogin(1, "zerocool").run(db)
  // res7: cats.package.Id[Boolean] = true
  p1(res7)
  val res8 = checkLogin(4, "davinci").run(db)
  // res8: cats.package.Id[Boolean] = false
  p1(res8)


}
