package com.rockthejvm.playground.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}
import com.rockthejvm.playground.part3concurrency.Resources.{Connection, openFileScanner, readLineByLine2}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration._

object ResourcesExercises extends IOApp.Simple {

  import com.rockthejvm.playground.utils._

  //use-case: manage a connection lifecycle.
  class Connection(url: String) {
    def open(): IO[String]  = IO(s"opening a connection to $url").debug
    def close(): IO[String] = IO(s"closing a connection to $url").debug
  }

  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open() *> IO.sleep((Int.MaxValue).seconds)).start
    _   <- IO.sleep(1.second) *> fib.cancel
  } yield ()
  //problem: leaking resources. [opening a connection without closing it]

  val correctAsyncFetchUrl: IO[Unit] = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib  <- (conn.open() *> IO.sleep((Int.MaxValue).seconds)).onCancel(conn.close().void).start
    _    <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /*
    bracket pattern: someIO.bracket(useResourceCallback)(releaseResourceCallback)
    bracket is equivalent to try-catches (In pure FP way).
   */
  val bracketFetchUrl: IO[Unit] = IO(new Connection("rockthejvm.com"))
    .bracket(conn => conn.open() *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close().void)

  val bracketProgram: IO[Unit] = for {
    fib <- bracketFetchUrl.start
    _   <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /*
   * Exercise: read the file with bracket pattern
   * - open a Scanner
   * - read the file line by line, every 100 miliseconds
   * - close the scanner
   * - if cancelled/throws error , close the scanner
   * */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLine(scanner: Scanner): IO[Unit] = {
    IO(System.out.println(scanner.nextLine()))
  }

  def readLines(scanner: Scanner): IO[Unit] = {
    readLine(scanner) >> IO.sleep(100.millis) >> readLines(scanner)
  }

  def bracketReadFile(path: String): IO[Unit] = {
    openFileScanner(path).bracket(scanner => readLines(scanner))(scanner => IO(scanner.close()).void)
  }

  def bracketReadFileProgram(path: String): IO[Unit] = for {
    fib    <- bracketReadFile(path).start
    result <- fib.join
  } yield result

  /** Resources
    */

  def connectionFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket { scanner =>
      IO(new Connection(scanner.nextLine())).bracket { conn =>
        conn.open().debug >> IO.never
      }(conn => conn.close().debug.void)
    }(scanner => IO("closing file").debug >> IO(scanner.close()))

  //nesting resources are tedious. (Nested try catches-like)

  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close().void)
  //... at a later part of your code
  val resourceFetchUrl: IO[Unit] = for {
    fib <- connectionResource.use(conn => conn.open() >> IO.never).start
    _   <- IO.sleep(1.second) >> fib.cancel
  } yield ()

  //resources are equivalent to bracket patterns
  val simpleResource: IO[String]          = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string").debug
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").debug.void

  val usingResourceWithBracket  = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)

  /** Exercise: read a text file with one line every 100 millis, using Resource.
    * (refactor the bracket exercise to use Resource)
    */
  val releaseScanner: Scanner => IO[Unit] = scanner => IO("closing file at $path").debug *> IO(scanner.close())

  def readFileResource(path: String): IO[Unit] = {
    val scannerResource = Resource.make(openFileScanner(path))(releaseScanner)
    scannerResource.use(scanner => readLineByLine2(scanner))
  }

  def resourceReadFileProgram(path: String): IO[Unit] = for {
    fib    <- readFileResource(path).start
    result <- fib.join
  } yield result

  def connFromConfResource(path: String): Resource[IO, Connection] =
    Resource
      .make(IO("opening file").debug >> openFileScanner(path))(scanner =>
        IO("closing file").debug >> IO(scanner.close())
      )
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void))

  def connFromConfResourceClean(path: String) = for {
    scanner <- Resource.make(IO("opening file").debug >> openFileScanner(path))(scanner =>
      IO("closing file").debug >> IO(scanner.close())
    )
    conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close().void)
  } yield conn

  val openConnection =
    connFromConfResource("src/main/resources/connection.txt").use(conn => conn.open() >> IO.never)

  val openConnectionClean =
    connFromConfResourceClean("src/main/resources/connection.txt").use(conn => conn.open() >> IO.never)

  val cancelConnection = for {
    fib <- openConnection.start
    _   <- IO.sleep(1.second) >> IO("cancelling!").debug >> fib.cancel
  } yield ()
  //connection + file will close automatically
  val cancelConnectionClean = for {
    fib <- openConnectionClean.start
    _   <- IO.sleep(1.second) >> IO("cancelling!").debug >> fib.cancel
  } yield ()

  //finalizers to regular IOs
  val ioWithFinalizer = IO("some resource").debug.guarantee(IO("freeing resources").debug.void)
  val ioWithFinalizer_v2 = IO("some resources").debug.guaranteeCase {
    case Succeeded(fa) =>
      fa.flatMap(result => IO(s"releasing resource: $result").debug).void
    case Errored(e) => IO("nothing to release").debug.void
    case Canceled() => IO("resource got canceled, releasing what's left").debug.void
  }

  override def run: IO[Unit] = {
    //    asyncFetchUrl.void
    //    bracketProgram.void
//    bracketReadFileProgram("src/main/scala/com/rockthejvm/playground/part3concurrency/Resources.scala").void
    readFileResource("src/main/scala/com/rockthejvm/playground/part3concurrency/Resources.scala").void
  }
}
