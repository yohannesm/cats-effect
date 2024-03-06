package com.rockthejvm.playground.part3concurrency

import cats.effect.IO.{IOCont, Uncancelable}
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, FiberIO, IO, IOApp}
import com.rockthejvm.playground.utils._

import scala.concurrent.duration._

object Fibers extends IOApp.Simple {

  val meaningOfLife: IO[Int] = IO.pure(42)
  val favLang: IO[String]    = IO.pure("Scala")

  def simpleIOComposition(): IO[Unit] = for {
    _ <- meaningOfLife.debug
    _ <- favLang.debug
  } yield ()

  //introduce the fiber primitive
  def createFiber: Fiber[IO, Throwable, String] = ??? //almost impossible to create fibers manually. use cats API

  //the fiber is not actually started, but the fiber allocation is wrapped in another effect(Another IO)
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  def differentThreadIOs(): IO[Unit] = for {
    _ <- aFiber
    _ <- favLang.debug
  } yield ()

  //joining a fiber
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib    <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result
  /*
   * what's happening above.
   * IO[ResultType of fib.join]
   * fib.join = Outcome[ IO, Throwable, A] has IO[A] inside it if successful
   * possible outcomes:
   * - success with an IO
   * - failure with an Exception
   * - cancelled
   * */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Succeeded(effect) => effect
    case Errored(_)        => IO(0)
    case Canceled()        => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib    <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result

  def testCancel(): IO[Outcome[IO, Throwable, String]] = {
    val task                        = IO("starting").debug >> IO.sleep(1.second) >> IO("done").debug
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled!").debug.void)

    for {
      fib    <- taskWithCancellationHandler.start //on a separate thread
      _      <- IO.sleep(500.millis) >> IO("cancelling").debug
      _      <- fib.cancel
      result <- fib.join
    } yield result
  }

  /** Exercises:
    * 1. Write a function that runs an IO on another thread, and , depending on the result of the fiber:
    *     - return the result in an IO
    *     - if errored or cancelled, return a failed IO
    *
    * 2. Write a function that takes two IOs, runs them on a different fibers and returns an IO with a tuple
    *    containing both results
    *    - if both IOs complete successfully, tuple their results
    *    - if the first IO returns an error, raise that error (ignoring the second IO's result)
    *    - if the first IO doesn't error but the second IO returns an error, raise that error.
    *    - if one (or both) canceled, raise a RuntimeException
    *
    * 3. Write a function that adds a timeout to an IO:
    *    - IO runs on a fiber
    *    - if the timeout duration passes, then the fiber is canceled
    *    - the method returns an IO[A] which contains
    *       - the original value if the computation is successful before the timeout signal
    *       - the exception if the computation is failed before the timeout signal
    *       - a RuntimeException if it times out (i.e. cancelled by the timeout).
    */

  //1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val ioOutcome: IO[Outcome[IO, Throwable, A]] = for {
      fib    <- io.start
      result <- fib.join
    } yield result
    ioOutcome.flatMap {
      case Succeeded(fa) => fa
      case Errored(e)    => IO.raiseError(e)
      case Canceled()    => IO.raiseError(new RuntimeException("IO Cancelled"))
    }
  }

  def testEx1(): IO[Unit] = {
    val aComputation = IO("starting").debug >> IO.sleep(1.second) >> IO("done!").debug >> IO(42).debug
    processResultsFromFiber(aComputation).void
  }

  //2
//  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
//    val runBothIOs = for {
//      fibA <- ioa.start
//      fibB <- iob.start
//      result = (fibA.join, fibB.join)
//    } yield result
//
//    val bleh = runBothIOs.flatMap { case (value1, value2) =>
//      val v1 = value1.flatMap {
//        case Succeeded(a) => a
//        case Errored(e)   => IO.raiseError(e)
//        case Canceled()   => IO.raiseError(new RuntimeException("IO A cancelled"))
//      }
//      val v2 = value2.flatMap {
//        case Succeeded(b) => b
//        case Errored(e)   => IO.raiseError(e)
//        case Canceled()   => IO.raiseError(new RuntimeException("IO B cancelled"))
//      }
//      IO { (v1, v2) }
//    }
//    bleh
//  }
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val result: IO[(Outcome[IO, Throwable, A], Outcome[IO, Throwable, B])] = for {
      fibA    <- ioa.start
      fibB    <- iob.start
      resultA <- fibA.join
      resultB <- fibB.join
    } yield (resultA, resultB)

    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) =>
        for {
          a <- fa
          b <- fb
        } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _               => IO.raiseError(new RuntimeException("IO was cancelled")) //cancelled case.
    }
  }

  def testEx2(): IO[Unit] = {
    val firstIO  = IO.sleep((2.seconds)) >> IO(1).debug
    val secondIO = IO.sleep((3.seconds)) >> IO(2).debug
    tupleIOs(firstIO, secondIO).debug.void
  }

  //3
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computeIOs: IO[Outcome[IO, Throwable, A]] = for {
      fib <- io.start
      _   <- IO.sleep(duration) >> fib.cancel
//      _      <- (IO.sleep(duration) >> fib.cancel).start //careful on doing this. Fibers can leak out resources
      //^running this on another thread(fiber). We won't wait for the whole `duration` to finish this method
      result <- fib.join
    } yield result
    computeIOs.flatMap {
      case Succeeded(fa) => fa
      case Errored(e)    => IO.raiseError(e)
      case Canceled()    => IO.raiseError(new RuntimeException("Computation timed out"))
    }
  }

  def testEx3(): IO[Unit] = {
    val computation = IO("starting").debug >> IO.sleep(1.second) >> IO("done!").debug >> IO(42)
    timeout(computation, 2.seconds).debug.void
//    timeout(computation, 500.millis).debug.void
    //^will errored out with timeout message
  }

  override def run: IO[Unit] = {
//    simpleIOComposition()
//    differentThreadIOs()
//    runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
//      .debug.void
//    throwOnAnotherThread().debug.void
//    testCancel().debug.void
//    (testEx1())
//    testEx2()
    testEx3()
  }
}
