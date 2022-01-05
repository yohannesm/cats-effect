package com.rockthejvm.playground.part2Effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  //IO
  val ourFirstIO: IO[Int] = IO.pure(42)
  val aDelayedIO: IO[Int] = IO.delay({
    println("I'm producing an integer")
    54
  })

  // don't use pure, because it'll be evaluated eagerly
//  val shouldNotDoThis: IO[Int] = IO.pure {
//    println("I'm producing an integer")
//    43
//  }

  val aDelayedIO_v2: IO[Int] = IO { // in this case apply == delay
    println("I'm also producing an integer")
    66
  }

  //map + flatmap
  val improvedMeaningOfLife = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife  = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _     <- IO.delay(println(line1 + line2))
  } yield ()

  //mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife: IO[Int] = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)
  def smallProgram_v2(): IO[Unit] =
    (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /** Exercises
    */

  // 1 - sequence two IOs and take the result of the LAST one
  // hint: use flatMap
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    ioa.flatMap(_ => iob)
  }

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    ioa *> iob // *> is "andThen" operator
  }

  def sequenceTakeLast_v3[A, B](ioa: IO[A], iob: IO[B]): IO[B] = {
    ioa >> iob // >> is "andThen" by name operator
  }

  // 2 - sequence two IOS and take the result of the FIRST one
  // hint: use flatMap
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
    iob.flatMap(_ => ioa)
  }

  // 3 - repeat an IO effect forever
  // hint: Use flatMap and recursion
  def forever[A](io: IO[A]): IO[A] = ???

  // 4 - convert an IO to a different type
  //hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] = ???

  // 5 - discard value inside an IO, just return Unit
  //hint: use map
  def asUnit[A](ioa: IO[A]): IO[Unit] = ???

  // 6 - use/fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] = ???

  // 7 (hard) - write a fibonacci function IO that does NOT crash on recursion
  // hint: use recursion, ignore expontential time complexity and use flatMap heavily
  def fibonacci(n: Int): IO[BigInt] = ???

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // "Platform" to run IO data structure
    // "end of the world"

//    println(aDelayedIO.unsafeRunSync())
//    println(smallProgram().unsafeRunSync())
    println(smallProgram_v2().unsafeRunSync())
  }

}
