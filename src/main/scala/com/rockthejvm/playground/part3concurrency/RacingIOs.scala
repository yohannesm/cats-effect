package com.rockthejvm.playground.part3concurrency

import cats.effect
import cats.effect.{Fiber, IO, IOApp, Outcome}
import com.rockthejvm.playground.utils.DebugWrapper

import scala.concurrent.duration._

object RacingIOs extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"starting computation: $value").debug >>
        IO.sleep(duration) >>
        IO(s"computation for $value: done") >>
        IO(value)
    ).onCancel(IO(s"computation CANCELED for $value").debug.void)

  def testRace() = {
    val meaningOfLife                  = runWithSleep(42, 1.second)
    val favLang                        = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)
    /*
     * - both IOs run on separate fibers
     * - the first one to finish will complete the result
     * - the loser will be canceled
     * */
    first.flatMap {
      case Left(mol)   => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"fav language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favLang       = runWithSleep("Scala", 2.seconds)
    val raceResult: IO[
      Either[
        (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
        (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String])  // (loser fiber, winner result)
      ]
    ] =
      IO.racePair(meaningOfLife, favLang)

    raceResult.flatMap {
      case Left((outMol, fibLang)) =>
        fibLang.cancel >> IO("MOL won").debug >> IO(outMol).debug
      case Right((fibMol, outLang)) => fibMol.cancel >> IO("Language won").debug >> IO(outLang).debug
    }
  }

  /*
   * Exercises:
   * 1 - implement a timeout pattern with race
   * add IO.raiseError if it exceeds the duration
   * 2 - a method to return a LOSING effect from a race (hint: use racePair)
   * 3 - implement race in terms of racePair
   * */
  //1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeoutEffect = IO.sleep(duration)
    val result        = IO.race(io, timeoutEffect)
    result.flatMap {
      case Left(value) => IO(value)
      case Right(_)    => IO.raiseError(new RuntimeException("Computation timed out"))
    }
  }
//    IO.race(IO.sleep(duration) >> IO.raiseError(throw new RuntimeException("Computation timed out")), io)

  val importantTask  = IO.sleep(2.seconds) >> IO(42).debug
  val testTimeout_v2 = importantTask.timeout(1.seconds)
  //2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB)) =>
        fibB.join.flatMap {
          case Outcome.Succeeded(resultEffect) => resultEffect.map(result => Right(result))
          case Outcome.Errored(e)              => IO.raiseError(e)
          case Outcome.Canceled()              => IO.raiseError(new RuntimeException("Loser cancelled"))
        }
      case Right((fibA, _)) =>
        fibA.join.flatMap {
          case Outcome.Succeeded(resultEffect) => resultEffect.map(result => Left(result))
          case Outcome.Errored(e)              => IO.raiseError(e)
          case Outcome.Canceled()              => IO.raiseError(new RuntimeException("Loser cancelled"))
        }
    }
//    IO.racePair(ioa, iob).flatMap {
//      case Left((outputL, fiberR)) => {
//        val res = for {
//          result <- fiberR.join
//        } yield result
//        res.flatMap {
//          case Outcome.Succeeded(fa) => fa
//          case Outcome.Errored(e)    =>
//          case Outcome.Canceled()    => ???
//        }
//      }
//      case Right((fiberL, outputR)) => ???
//    }

  //3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) =>
        outA match {
          case Outcome.Succeeded(effectA) => fibB.cancel >> effectA.map(a => Left(a))
          case Outcome.Errored(e)         => fibB.cancel >> IO.raiseError(e)
          case Outcome.Canceled() =>
            fibB.join.flatMap {
              case Outcome.Succeeded(effectB) => effectB.map(b => Right(b))
              case Outcome.Errored(e)         => IO.raiseError(e)
              case Outcome.Canceled()         => IO.raiseError(new RuntimeException("Both computations cancelled"))
            }
        }
      case Right((fibA, outB)) =>
        outB match {
          case Outcome.Succeeded(effectB) => fibA.cancel >> effectB.map(b => Right(b))
          case Outcome.Errored(e)         => fibA.cancel >> IO.raiseError(e)
          case Outcome.Canceled() =>
            fibA.join.flatMap {
              case Outcome.Succeeded(effectA) => effectA.map(a => Left(a))
              case Outcome.Errored(e)         => IO.raiseError(e)
              case Outcome.Canceled()         => IO.raiseError(new RuntimeException("Both computations cancelled"))
            }
        }
    }

  def testUnrace() = {
    val meaningOfLife                  = runWithSleep(42, 1.second)
    val favLang                        = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = unrace(meaningOfLife, favLang)
    /*
     * - both IOs run on separate fibers
     * - the first one to finish will complete the result
     * - the loser will be canceled
     * */
    first.flatMap {
      case Left(mol)   => IO(s"Meaning of life won: $mol")
      case Right(lang) => IO(s"fav language won: $lang")
    }
  }

  override def run: IO[Unit] = {
//    testRace().debug.void
//    testRacePair().void
//    timeout(IO.sleep(2.seconds) >> IO(42).debug, 3.seconds).debug.void
    testUnrace.debug.void
  }
}
