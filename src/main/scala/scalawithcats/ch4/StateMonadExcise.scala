package scalawithcats.ch4

import cats.implicits.catsSyntaxApplicativeId
import scalawithcats.Utils._

/**
 * 4.9.3 Exercise: Post‐Order Calculator
 */
object StateMonadExcise extends App {

  import cats.data.State

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] =
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }


  //  Thinking code:
  //    State[List[Int], Int] { oldStack =>
  //      val newStack = someTransformation(oldStack)
  //      val result = someCalculation
  //      (newStack, result)
  //    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case b :: a :: tail =>
        val ans = func(a, b)
        (ans :: tail, ans)
      case _ => sys.error("Calculate Fail!")
    }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]){(a,b) =>
      a.flatMap(_ => evalOne(b))
    }

  val res10 = evalOne("42").runA(Nil).value
  // res10: Int = 42
  p1(res10)

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans
  // program: cats.data.IndexedStateT[cats.Eval, List[Int], List[Int], Int] = cats.data.IndexedStateT@4449effe
  p1(program)
  val res11 = program.runA(Nil).value
  // res11: Int = 3
  p1(res11)

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  // multistageProgram: CalcState[Int] = cats.data.IndexedStateT@7759956e
  p1(multistageProgram)
  val res13 = multistageProgram.runA(Nil).value
  // res13: Int = 9
  p1(res13)

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans
  // biggerProgram: cats.data.IndexedStateT[cats.Eval, List[Int], List[Int], Int] = cats.data.IndexedStateT@2c2c50d1
  p1(biggerProgram)
  val res14 = biggerProgram.runA(Nil).value
  // res14: Int = 21
  p1(res14)
}