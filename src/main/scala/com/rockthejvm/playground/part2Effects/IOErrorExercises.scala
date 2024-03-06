package com.rockthejvm.playground.part2Effects

import cats.effect.IO

import java.util.concurrent.CompletableFuture
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

import cats.implicits._
import cats._

object IOErrorExercises {

  //IO: pure , delay, defer
  //create failed effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("a runtime failure"))
  val aFailure: IO[Int]       = IO.raiseError(new RuntimeException("a proper fail"))

  //handle exceptions
  val dealWithIt = aFailure.handleErrorWith { case _: RuntimeException =>
    IO.delay(println("I'm still here"))
  //add more cases
  }

  //turn into an Either
  //use attempt
  val effectsAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  //redeem: transform the failure and success in one go
  val resultAsString: IO[String] = aFailure.redeem(ex => s"FAIL: $ex", value => s"SUCCESS: $value")
  // redeemWith. Like flatMap with redeem
  val resultsAsEffect: IO[Unit] =
    aFailure.redeemWith(ex => IO(println(s"FAIL: $ex")), value => IO(println(s"SUCCESS: $value")))

  /*
   *
   * Exercises
   * */

  //1 - construct potentially failed IOs from standard data types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case Some(value) => IO.delay(value)
    case None        => IO.raiseError(ifEmpty)
  }

  def try2IO[A](aTry: Try[A]): IO[A] = aTry match {
    case Failure(exception) => IO.raiseError(exception)
    case Success(value)     => IO.delay(value)
  }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = anEither match {
    case Left(ex)     => IO.raiseError(ex)
    case Right(value) => IO { value }
  }

  //2 - handleError, handleErrorWith
  // use redeem
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, _ => io)

  // you can use this from IO std lib
  //  aFailure.handleErrorWith()

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    //    aFailedCompute.unsafeRunSync()
    //    aFailure.unsafeRunSync()
    //    dealWithIt.unsafeRunSync()

    println(resultAsString.unsafeRunSync())
    resultsAsEffect.unsafeRunSync()

    //these conversions are available from IO CE library

    //    IO.fromTry[A](t: Try[A])
    //    IO.fromEither[A](e: Either[Throwable, A])
    //    IO.fromFuture[A](fut: IO[Future[A]])
    //    IO.fromOption[A](o: Option[A])(orElse: => Throwable)
    //    IO.fromCompletableFuture[A](fut: IO[CompletableFuture[A]])

  }
}
