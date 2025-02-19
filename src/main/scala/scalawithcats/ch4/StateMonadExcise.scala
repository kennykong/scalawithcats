package scalawithcats.ch4

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

  val res10 = evalOne("42").runA(Nil).value
  // res10: Int = 42
  p1(res10)
}