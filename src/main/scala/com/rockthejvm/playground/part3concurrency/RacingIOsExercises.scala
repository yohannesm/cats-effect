package com.rockthejvm.playground.part3concurrency

import cats.effect
import cats.effect.{Fiber, IO, IOApp, Outcome}
import com.rockthejvm.playground.utils.DebugWrapper

import scala.concurrent.duration._

object RacingIOsExercises extends IOApp.Simple {

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
    val timeoutIO  = IO.sleep(duration)
    val raceResult = IO.race(timeoutIO, io)
    raceResult.flatMap {
      case Left(_)   => IO.raiseError(new RuntimeException("IO timed out"))
      case Right(fa) => IO.pure(fa)
    }
  }

  //2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fibB))  => fibB.joinWithNever.map(b => Right(b))
      case Right((fibA, _)) => fibA.joinWithNever.map(a => Left(a))
    }

  //3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outA, fibB)) =>
        outA match {
          case Outcome.Succeeded(fa) => fibB.cancel >> fa.map(a => Left(a))
          case Outcome.Errored(e)    => IO.raiseError(e)
          case Outcome.Canceled()    => fibB.joinWithNever.map(b => Right(b))
        }
      case Right((fibA, outB)) =>
        outB match {
          case Outcome.Succeeded(fb) => fibA.cancel >> fb.map(b => Right(b))
          case Outcome.Errored(e)    => IO.raiseError(e)
          case Outcome.Canceled()    => fibA.joinWithNever.map(a => Left(a))
        }
    }

  def testUnrace() = {
    val meaningOfLife = runWithSleep(42, 1.second).start.map(_.cancel)
    val favLang       = runWithSleep("Scala", 2.seconds).start.map(_.cancel)
    val first         = unrace(meaningOfLife, favLang)
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
    testUnrace().debug.void
  }
}
