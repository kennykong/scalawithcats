package scalawithcats.ch10


import scalawithcats.Utils.p

object DataValidationExample extends App {

  import cats.Semigroup
  import cats.instances.list._
  // for Semigroup
  import cats.syntax.semigroup._ // for |+|

  val semigroup = Semigroup[List[String]]
  // Combination using methods on Semigroup
  val res3 = semigroup.combine(List("Badness"), List("More badness"))
  p(res3)
  // res3: List[String] = List("Badness", "More badness")
  // Combination using Semigroup syntax
  val res4 = List("Oh noes") |+| List("Fail happened")
  // res4: List[String] = List("Oh noes", "Fail happened")
  p(res4)

  import cats.Semigroup
  import cats.syntax.either._
  // for asLeft and asRight
  import cats.syntax.semigroup._ // for |+|

  final case class CheckF[E, A](func: A => Either[E, A]) {
    def apply(a: A): Either[E, A] =
      func(a)

    def and(that: CheckF[E, A])
           (implicit s: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this (a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (Left(e), Right(_)) => e.asLeft
          case (Right(_), Left(e)) => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      }
  }

  import cats.instances.list._ // for Semigroup

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }
  val check: CheckF[List[String], Int] =
    a and b

  val res5 = check(5)
  p(res5)
  // res5: Either[List[String], Int] = Left(List("Must be < -2"))
  val res6 = check(0)
  p(res6)
  // res6: Either[List[String], Int] = Left(List("Must be > 2", "Must be < -2"))

  val a1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)

  val b1: CheckF[Nothing, Int] =
    CheckF(v => v.asRight)

  // error:
  //  val check1 = a1 and b1
  // error: could not find implicit value for parameter s: cats.
  // Semigroup[Nothing]
  // a and b
  // ^^^^^^^

  //first ADT Check
  sealed trait Check[E, A] {

    import Check._

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          (left(a), right(a)) match {
            case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
            case (Left(e), Right(_)) => e.asLeft
            case (Right(_), Left(e)) => e.asLeft
            case (Right(_), Right(_)) => a.asRight
          }
      }
  }

  object Check {
    final case class And[E, A](left: Check[E, A],
                               right: Check[E, A]) extends Check[E, A]

    final case class Pure[E, A](func: A => Either[E, A]) extends Check[E, A]

    def pure[E, A](f: A => Either[E, A]): Check[E, A] =
      Pure(f)
  }


  val a2: Check[List[String], Int] =
    Check.pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }
  val b2: Check[List[String], Int] =
    Check.pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }
  val check2: Check[List[String], Int] =
    a2 and b2


  //Second ADT Check

  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.apply._

  // for mapN
  sealed trait Check2[E, A] {

    import Check2._

    def and(that: Check2[E, A]): Check2[E, A] =
      And(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)
      }
  }

  object Check2 {
    final case class And[E, A](left: Check2[E, A],
                               right: Check2[E, A]) extends Check2[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Check2[E, A]
  }

  //3rd ADT Check

  import cats.Semigroup
  import cats.data.Validated
  import cats.syntax.semigroup._ // for |+|
  import cats.syntax.apply._
  // for mapN
  import cats.data.Validated._

  // for Valid and Invalid
  sealed trait Check3[E, A] {

    import Check3._

    def and(that: Check3[E, A]): Check3[E, A] =
      And(this, that)

    def or(that: Check3[E, A]): Check3[E, A] =
      Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(func) =>
          func(a)
        case And(left, right) =>
          (left(a), right(a)).mapN((_, _) => a)
        case Or(left, right) =>
          left(a) match {
            case Valid(a) => Valid(a)
            case Invalid(e1) =>
              right(a) match {
                case Valid(a) => Valid(a)
                case Invalid(e2) => Invalid(e1 |+| e2)
              }
          }
      }
  }

  object Check3 {
    final case class And[E, A](left: Check3[E, A],
                               right: Check3[E, A]) extends Check3[E, A]

    final case class Or[E, A](left: Check3[E, A],
                              right: Check3[E, A]) extends Check3[E, A]

    final case class Pure[E, A](func: A => Validated[E, A]) extends Check3[E, A]
  }

}
