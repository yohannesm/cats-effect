package com.rockthejvm.playground.part2Effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IOErrorHandling {

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
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = {
    option match {
      case Some(value) => IO(value)
      case None        => IO.raiseError(ifEmpty)
    }
  }

  def try2IO[A](aTry: Try[A]): IO[A] = {
    aTry match {
      case Success(value)     => IO(value)
      case Failure(exception) => IO.raiseError(exception)
    }
  }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = {
    anEither match {
      case Right(value) => IO(value)
      case Left(ex)     => IO.raiseError(ex)
    }
  }

  //2 - handleError, handleErrorWith
  // use redeem
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] =
    io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] =
    io.redeemWith(handler, x => IO(x))

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
//    aFailedCompute.unsafeRunSync()
//    aFailure.unsafeRunSync()
//    dealWithIt.unsafeRunSync()

    println(resultAsString.unsafeRunSync())
    resultsAsEffect.unsafeRunSync()

  }
}
