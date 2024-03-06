package com.rockthejvm.playground.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.playground.utils.DebugWrapper

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object BlockingIOs extends IOApp.Simple {

  val someSleeps = for {
    _ <- IO.sleep(1.second).debug // SEMANTIC BLOCKING
    _ <- IO.sleep(1.second).debug
  } yield ()

  //really blocking IOs
  val aBlockingIO = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    42
  } //will evaluate on a thread from ANOTHER thread pool specific for blocking calls

  //yielding
  val iosOnManyThreads = for {
    _ <- IO("first").debug
    _ <- IO.cede // a signal to yield control over the thread - equivalent to IO.shift
    _ <- IO("second").debug // the rest of this effect may run on another thread
    _ <- IO.cede
    _ <- IO("third").debug
  } yield ()

  def testThousandEffectsSwitch() = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec)
  }

  //blocking calls & IO.sleep yield control over the calling thread automatically

  override def run: IO[Unit] = {
//    someSleeps
//    aBlockingIO.void
//    iosOnManyThreads
    testThousandEffectsSwitch().void
  }
}
