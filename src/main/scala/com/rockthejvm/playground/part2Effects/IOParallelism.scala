package com.rockthejvm.playground.part2Effects

import cats.Parallel
import cats.effect.IO.Par
import cats.effect.{IO, IOApp, ParallelF}

object IOParallelism extends IOApp.Simple {

  val aniIO: IO[String]    = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO: IO[String] = IO(s"[${Thread.currentThread().getName}] Kamran")

  private val composedIO: IO[String] = for {
    ani    <- aniIO
    kamran <- kamranIO
  } yield s"$ani and $kamran love Rock the JVM"

  import com.rockthejvm.playground.utils._
  import cats.syntax.apply._
  val meaningOfLife: IO[Int] = IO.delay(42).debug
  val favLang: IO[String]    = IO.delay("Scala").debug
  val goalInLife: IO[String] =
    (meaningOfLife.debug, favLang.debug).mapN((num, string) => s"my goal in life is $num and $string")

  val parIO1: IO.Par[Int]    = Parallel[IO].parallel(meaningOfLife.debug)
  val par102: IO.Par[String] = Parallel[IO].parallel(favLang.debug)
  import cats.effect.implicits._
  val goalInLifeParallel: Par[String] = (parIO1, par102).mapN((num, string) => s"my goal in life is $num and $string")
  // turn back to sequential
  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)

  // shorthand:
  import cats.syntax.parallel._
  val goalInLife_v3: IO[String] =
    (meaningOfLife.debug, favLang.debug).parMapN((num, string) => s"my goal in life is $num and $string")

  //regarding failure:
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!"))
  //compose success + failure
//  val parallelWithFailure: IO.Par[String] = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)
  //compose failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure"))
  val twoFailures: IO[String]    = (aFailure.debug, anotherFailure.debug).parMapN(_ + _)
  //the first effect gives the failure of the result
  val twoFailuresDelayed: IO[String] = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = {
    composedIO.map(println)
//    goalInLife.map(println)
//    goalInLife_v2.map(println)
//    goalInLife_v2.debug.void
//    goalInLife_v3.debug.void
//    aFailure.debug
//    twoFailures.debug
//    twoFailuresDelayed.debug.void

  }
}
