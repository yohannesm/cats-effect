package com.rockthejvm.playground.part2Effects

import cats.Traverse
import cats.effect.{IO, IOApp}

import scala.concurrent.Future
import scala.util.Random

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("I quite like CE", "Scala is great", "looking forward to some awesome stuff")
  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    //Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  import cats.instances.list._
  val listTraverse: Traverse[List] = Traverse[List]
  def traverseFutures(): Unit = {
    //traverse
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    //^^ this stores all the result in one go
    singleFuture.foreach(println)
  }

  import com.rockthejvm.playground.utils._
  //traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]]      = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO) //List[String] | IO[Int]

  //parallel traversal
  import cats.syntax.parallel._
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /** Exercises
    */

  //hint: use Traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    Traverse[List].traverse(listOfIOs)(identity) // List[A] | IO[A] . IO { }

  //hard version
  def sequence_v2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(identity)

  //parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(identity)

  //hard version
  def parSequence_v2[F[_]: Traverse, A](contextOfIOs: F[IO[A]]): IO[F[A]] =
    contextOfIOs.parTraverse(identity)

  //existing sequence API
  val singleIO_v2: IO[List[Int]] = listTraverse.sequence(ios)

  //parallel sequencing
  val parallelSingleIO_v2: IO[List[Int]] = parSequence(ios) // from the exercise
  val parallelSingleIO_v3: IO[List[Int]] = ios.parSequence  //extension method from Parallel syntax package

  override def run: IO[Unit] = {
//    singleIO.map(_.sum).debug.void
    parallelSingleIO_v3.map(_.sum).debug.void
  }
}
