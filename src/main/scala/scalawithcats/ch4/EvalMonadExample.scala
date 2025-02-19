package scalawithcats.ch4

import scalawithcats.Utils.p

object EvalMonadExample extends App {
  import cats.Eval
  val now = Eval.now(math.random + 1000)
  // now: Eval[Double] = Now(1000.7009661848473)
  val always = Eval.always(math.random + 3000)
  // always: Eval[Double] = cats.Always@2a4e7955
  val later = Eval.later(math.random + 2000)
  // later: Eval[Double] = cats.Later@7684da18
  val res1 = now.value
  // res6: Double = 1000.7009661848473
  p(res1)
  val res2 = always.value
  // res7: Double = 3000.5158510235524
  p(res2)
  val res3 = later.value
  // res8: Double = 2000.6995448328964
  p(res3)

  val x = Eval.now {
    println("Computing X")
    math.random
  }
  // Computing X
  // x: Eval[Double] = Now(0.6969571260771719)
  val x1 = x.value // first access
  // res10: Double = 0.6969571260771719 // first access
  p(x1)
  val x2 = x.value // second access
  // res11: Double = 0.6969571260771719
  p(x2)

  val y = Eval.always {
    println("Computing Y")
    math.random
  }
  // y: Eval[Double] = cats.Always@6d355284
  val y1 = y.value // first access
  // Computing Y
  // res12: Double = 0.8575236846076497 // first access
  p(y1)
  val y2 = y.value // second access
  // Computing Y
  // res13: Double = 0.15716382484701563
  p(y2)

  val z = Eval.later {
    println("Computing Z")
    math.random
  }
  // z: Eval[Double] = cats.Later@3429dabc
  val z1 = z.value // first access
  // Computing Z
  // res14: Double = 0.5149108999064906 // first access
  p(z1)
  val z2 = z.value // second access
  // res15: Double = 0.5149108999064906
  p(z2)

  val greeting = Eval
    .always {
      println("Step 1"); "Hello"
    }
    .map { str => println("Step 2"); s"$str world" }
  // greeting: Eval[String] = cats.Eval$$anon$4@496b9f25
  val greeting1 = greeting.value
  // Step 1
  // Step 2
  // res16: String = "Hello world"
  p(greeting1)

  val ans = for {
    a <- Eval.now {
      println("Calculating A"); 40
    }
    b <- Eval.always {
      println("Calculating B"); 2
    }
  } yield {
    println("Adding A and B")
    a + b
  }
  // Calculating A
  // ans: Eval[Int] = cats.Eval$$anon$4@6e0e633
  val ans1 = ans.value // first access
  // Calculating B
  // Adding A and B
  // res17: Int = 42 // first access
  p(ans1)
  val ans2 = ans.value // second access
  // Calculating B
  // Adding A and B
  // res18: Int = 42
  p(ans2)

  val saying = Eval
    .always {
      println("Step 1"); "The cat"
    }
    .map { str => println("Step 2"); s"$str sat on" }
    .memoize
    .map { str => println("Step 3"); s"$str the mat" }
  // saying: Eval[String] = cats.Eval$$anon$4@77e677ee
  val saying1 = saying.value // first access
  // Step 1
  // Step 2
  // Step 3
  // res19: String = "The cat sat on the mat" // first access
  p(saying1)
  val saying2 = saying.value // second access
  // Step 3
  // res20: String = "The cat sat on the mat"
  p(saying2)

  def factorial(n: BigInt): BigInt =
    if (n == 1) n else n * factorial(n - 1)

//  factorial(50000)
  // java.lang.StackOverflowError
  // ...

  def factorial1(n: BigInt): Eval[BigInt] =
    if (n == 1) {
      Eval.now(n)
    } else {
      factorial1(n - 1).map(_ * n)
    }

//  factorial1(50000).value
  // java.lang.StackOverflowError
  // ...

  def factorial2(n: BigInt): Eval[BigInt] =
    if(n == 1) {
      Eval.now(n)
    } else {
      Eval.defer(factorial2(n - 1).map(_ * n))
    }
  val factorial21 = factorial2(50000).value
  // res: A very big value
//  p(factorial21)

  import cats.Eval

  def foldRightEval[A, B](as: List[A], acc: Eval[B])
                         (fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail =>
        Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
      case Nil =>
        acc
    }

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc)) { (a, b) =>
      b.map(fn(a, _))
    }.value

  val res24 = foldRight((1 to 100000).toList, 0L)(_ + _)
  // res24: Long = 5000050000L

  p(res24)
}
