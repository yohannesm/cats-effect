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
    ioa.flatMap(a => iob.map(_ => a))
  }

  def sequenceTakeFirst_v2[A, B](ioa: IO[A], iob: IO[B]): IO[A] = {
    ioa <* iob
  }

  // 3 - repeat an IO effect forever
  // hint: Use flatMap and recursion
  def forever[A](io: IO[A]): IO[A] = {
    io.flatMap(_ => forever(io))
  }

  def forever_v2[A](io: IO[A]): IO[A] = {
    io >> forever_v2(io)
  }

  def forever_v3[A](io: IO[A]): IO[A] = {
    io *> forever_v3(io)
  }

  def forever_v4[A](io: IO[A]): IO[A] = {
    io.foreverM //with tail recursion
  }

  // 4 - convert an IO to a different type
  //hint: use map
  def convert[A, B](ioa: IO[A], value: B): IO[B] = {
    ioa.map(_ => value)
  }

  def convert_v2[A, B](ioa: IO[A], value: B): IO[B] = {
    ioa.as(value)
  }

  // 5 - discard value inside an IO, just return Unit
  //hint: use map
  def asUnit[A](ioa: IO[A]): IO[Unit] = {
    ioa.map(_ => ())
  }

  def asUnit_v2[A](ioa: IO[A]): IO[Unit] = {
    ioa.as(()) // discourage - don't use this. confusing to fellow programmer. () is overloaded
  }

  def asUnit_v3[A](ioa: IO[A]): IO[Unit] = {
    ioa.void //same as above, and more readable
  }

  // 6 - use/fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] = {
    if (n <= 0) IO { 0 }
    else
      for {
        lastNumber <- IO(n)
        prevSum    <- sumIO(n - 1)
      } yield prevSum + lastNumber
  }

  // 7 (hard) - write a fibonacci function IO that does NOT crash on recursion
  // hint: use recursion, ignore expontential time complexity and use flatMap heavily
  def plainFibonacci(n: Int): Int =
    if (n <= 0) 0
    else if (n == 1) 1
    else plainFibonacci(n - 1) + plainFibonacci(n - 2)

  def fibonacci(n: Int): IO[BigInt] =
    if (n < 2) IO(1)
    else
      for {
        last     <- IO(fibonacci(n - 1)).flatMap(identity) //the func inside flatMap are identical between these 2
        previous <- IO(fibonacci(n - 2)).flatMap(x => x)
        //another ways to write these
//        last2 <- IO.defer(fibonacci(n - 1))
//        last3 <- IO(fibonacci(n - 1)).flatten
      } yield last + previous

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // "Platform" to run IO data structure
    // "end of the world"

//    println(aDelayedIO.unsafeRunSync())
//    println(smallProgram().unsafeRunSync())
//    println(smallProgram_v2().unsafeRunSync())

//    forever(IO(println("forever!"))).unsafeRunSync()

    //NOTE: This will cause stack overflow because evaluated eagerly
//    forever_v3(IO {
//      println("forever!")
//      Thread.sleep(100)
//    }).unsafeRunSync()

    println(sumIO(20000).unsafeRunSync())
    (1 to 25).foreach(i => println(fibonacci(i).unsafeRunSync()))
  }

}
