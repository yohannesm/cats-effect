package com.rockthejvm.playground.part2Effects

import scala.concurrent.Future
import scala.io.StdIn

object Effects {

  val printSomething: Unit    = println("Cats Effect")
  val printSomething_v2: Unit = ()

  //Effect Types
  /*
   * Properties:
   * - Type signature describes the kind of calculation that will be performed
   * - Type signature describes the VALUE that will be calculated
   * - When side effects are needed, effect construction should be separate from effect execution
   *
   * */

  //example: Option (is an effect type)
  // - describes a possibly absent value
  // - computes a value of type A, if it exists
  // - side effects are not needed.
  val anOption: Option[Int] = Option(42)

  /*
   * example: Future (not an effect type).
   * - describes an asynchronous computation that will be performed at some point in the future
   * - computes a value of type A, if it's successful.
   * - side effect is required (allocating/scheduling a thread), execution is NOT separate from construction
   * */
  import scala.concurrent.ExecutionContext.Implicits.global
  // can't separate the construction and execution. Will be executed here right away for Future
  val aFuture: Future[Int] = Future(42)

  /* last example
   * example: MyIO data type from Monad lesson - IT IS an effect type
   * - describes any computation that might produce side effects
   * - calculates a value of type A, if it's successful
   * - side effects are required for the evaluation of () => A [Zero (arg) lambda that produces an A]
   *    - So YES, the creation of MyIO does not produce the side effects on construction
   * */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO { () =>
        f(unsafeRun()).unsafeRun()
      }
  }

  val anIO: MyIO[Int] = MyIO(() => {
    println("I'm writing something...")
    42
  })

  /*
   * Exercise
   * 1. an IO which returns the current time of the System
   * 2. an IO Which measures the duration of a computation (hint: use ex 1)
   * 3. An IO which prints something to the console.
   * 4. An IO which reads a line (a string) from the std input
   * */

  //1
  val clock: MyIO[Long] = MyIO(() => {
    val currentMillis = System.currentTimeMillis()
    currentMillis
  })

  //2
  def measure2[A](computation: MyIO[A]): MyIO[Long] = MyIO(() => {
    val startTime: Long = clock.unsafeRun();
    computation.unsafeRun()
    val endTime: Long = clock.unsafeRun();
    endTime - startTime
  })

  def measure[A](computation: MyIO[A]): MyIO[Long] = for {
    startTime  <- clock
    _          <- computation
    finishTime <- clock
  } yield finishTime - startTime

  /* BREAKING down the map, flatmap chain on for comprehension above
   * clock.flatMap(startTime => computation.flatMap(_ => clock.map(finishTime => finishTime - startTime)))
   *
   * clock.map(finishTime => finishTime - startTime) = MyIO(() => System.currentTimeMillis() - startTime)
   * => clock.flatMap(startTime => computation.flatMap(_ => MyIO(() => System.currentTimeMillis() - startTime)))
   *
   * computation.flatMap(lambda) = MyIO(() => lambda(__COMP__).unsafeRun())
   *                             = MyIO(() => MyIO(() => System.currentTimeMillis() - startTime)).unsafeRun())
   *                             = MyIO(() => System.currentTimeMillis_after_computation() - startTime)
   *
   * => clock.flatMap(startTime => MyIO(() => System.currentTimeMillis_after_computation() - startTime))
   * = MyIO(() => lambda(clock.unsafeRun()).unsafeRun())
   * = MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis_at_start())
   * */

  def TestTimeIO(): Unit = {
    val test: MyIO[Long] = measure2(MyIO(() => Thread.sleep(1000)))
    println(test.unsafeRun())
  }
  //3
  def putStrLn(toPrint: String): MyIO[Unit] = MyIO(() => {
    println(toPrint)
  })

  //4
  val read: MyIO[String] = MyIO(() => StdIn.readLine())

  def testConsole(): Unit = {
    val program: MyIO[Unit] = for {
      line1 <- read
      line2 <- read
      _     <- putStrLn(line1 + line2)
    } yield ()

    program.unsafeRun()
  }

  def main(args: Array[String]): Unit = {
    anIO.unsafeRun()
    clock.unsafeRun()
    putStrLn("I am the king of the world").unsafeRun()
    TestTimeIO()
//    testConsole()
  }

}
