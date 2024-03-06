package com.rockthejvm.playground.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.playground.utils.DebugWrapper

import scala.concurrent.duration._

object CancellingIOs extends IOApp.Simple {
  /*
   * Cancelling IOs
   * - fib.cancel
   * - IO.race & other APIs
   * - manual cancellation (IO.canceled)
   * */

  val chainOfIOs: IO[Int] = IO("waiting").debug >> IO.canceled >> IO(42).debug

  // uncancelable
  // example: online store, payment processor
  //payment process must NOT be canceled
  val specialPaymentSystem = {
    IO("payment running, don't cancel me...").debug >>
      IO.sleep(1.second) >> IO("Payment completed.").debug
  }.onCancel(IO("MEGA CANCEL OF DOOM!").debug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _   <- IO.sleep(500.millis) >> fib.cancel
    _   <- fib.join
  } yield ()

  val atomicPayment    = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  val atomicPayment_v2 = specialPaymentSystem.uncancelable          // same as above

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _   <- IO.sleep(500.millis) >> IO("attempting cancellation...").debug >> fib.cancel
    _   <- fib.join
  } yield ()

  /*
   * The uncancelable API is more complex and more general.
   * It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
   * The poll object can be used to mark sections within the returned effect which CAN BE CANCELED
   * */

  /*
   * Example: Authentication service. Has two parts:
   * - input password: can be cancelled , because otherwise we might block indefinitely on user input
   * - verify password: CAN'T be cancelled once it's started
   * */

  val inputPassword =
    IO("Input password: ").debug >> IO("typing password").debug >> IO.sleep(2.seconds) >> IO("RockTheJVM1!")

  val verifyPassword = (pw: String) => IO("verifying...").debug >> IO.sleep(2.seconds) >> IO(pw == "RockTheJVM1!")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(
        IO("Auth timed out. Try again later").debug.void
      ) //this will be cancelable. being "unmasked"
      verified <- verifyPassword(pw) //this is NOT cancelable
      _ <-
        if (verified) IO("Auth successful").debug //this is NOT cancelable
        else IO("Auth failed").debug
    } yield ()
  }

  val authProgram = for {
    authFib <- authFlow.start
    _       <- IO.sleep(3.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _       <- authFib.join
  } yield ()

  /*
   * Uncancelable calls are MASKS which suppress cancellation
   * Poll calls are "gaps opened" in the uncancelable region. "unmasking those region"
   * */

  /*
   * Exercises
   * */
  //1
  val cancelBeforeMol = IO.canceled >> IO(42).debug
  val uncancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).debug)
  //uncancelable will eliminate ALL cancel points

  //2
  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _       <- IO.sleep(3.seconds) >> IO("Authentication timed out, attempting cancel...").debug >> authFib.cancel
    _       <- authFib.join
  } yield ()

  //3
  def threeStepProgram() = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").debug) >> IO.sleep(1.second) >> IO("cancelable end").debug >>
        IO("uncancelable").debug >> IO.sleep(1.second) >> IO("uncancelable end").debug >>
        poll(IO("second cancelable").debug) >> IO.sleep(1.second) >> IO("second cancelable end").debug
    }

    for {
      fib <- sequence.start
      _   <- IO.sleep(1500.millis) >> IO("CANCELING").debug >> fib.cancel
      _   <- fib.join
    } yield ()

  }

  override def run: IO[Unit] = {
//    cancellationOfDoom
//    noCancellationOfDoom
//    authFlow
//    authProgram //play around with the timing of inputPassword
//    cancelBeforeMol.void
//    uncancelableMol.void
//    invincibleAuthProgram
    threeStepProgram()
  }
}
