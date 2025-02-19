package scalawithcats.ch4

import scalawithcats.Utils.p1

object StateMonadExample extends App {

  import cats.data.State

  val a = State[Int, String] { state =>
    (state, s"The state is $state")
  }
  // Get the state and the result:
  val (state, result) = a.run(10).value
  // state: Int = 10
  // result: String = "The state is 10"
  p1(state, result)
  // Get the state, ignore the result:
  val justTheState = a.runS(10).value
  // justTheState: Int = 10
  // Get the result, ignore the state:
  p1(justTheState)
  val justTheResult = a.runA(10).value
  // justTheResult: String = "The state is 10"
  p1(justTheResult)

  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }
  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }
  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)
  val (state1, result1) = both.run(20).value
  // state: Int = 42
  // result: (String, String) = ("Result of step1: 21", "Result of step2: 42")
  p1(state1, result1)

  val getDemo = State.get[Int]
  // getDemo: State[Int, Int] = cats.data.IndexedStateT@796af713
  val res1 = getDemo.run(10).value
  p1(getDemo)
  p1(res1)
  // res1: (Int, Int) = (10, 10)
  val setDemo = State.set[Int](30)
  // setDemo: State[Int, Unit] = cats.data.IndexedStateT@f9e66fa
  val res2 = setDemo.run(10).value
  p1(setDemo)
  p1(res2)
  // res2: (Int, Unit) = (30, ())
  val pureDemo = State.pure[Int, String]("Result")
  // pureDemo: State[Int, String] = cats.data.IndexedStateT@439e3ee4
  val res3 = pureDemo.run(10).value
  p1(pureDemo)
  p1(res3)
  // res3: (Int, String) = (10, "Result")
  val inspectDemo = State.inspect[Int, String](x => s"${x}!")
  // inspectDemo: State[Int, String] = cats.data.IndexedStateT@77263be4
  val res4 = inspectDemo.run(10).value
  p1(inspectDemo)
  p1(res4)
  // res4: (Int, String) = (10, "10!")
  val modifyDemo = State.modify[Int](_ + 1)
  // modifyDemo: State[Int, Unit] = cats.data.IndexedStateT@44ddcbfc
  val res5 = modifyDemo.run(10).value
  p1(modifyDemo)
  p1(res5)
  // res5: (Int, Unit) = (11, ())

  import cats.data.State
  import State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)
  // program: State[Int, (Int, Int, Int)] = cats.data.
//  IndexedStateT @42c9d44a
  p1(program)
  val (state2, result2) = program.run(1).value
  // state: Int = 3
  // result: (Int, Int, Int) = (1, 2, 3000)
  p1(state2, result2)

}
