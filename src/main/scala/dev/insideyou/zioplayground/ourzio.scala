package dev.insideyou
package zioplayground

import scala.reflect.ClassTag

// scala3 case class final by default
// can be extends if in the same compilation unit => same file
final class ZIO[-R, +E, +A](val run: R => Either[E, A]):
  // E1 -- at least Dog, can be Animal
  def flatMap[R1 <: R, E1 >: E, B](azb: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
    ZIO(r => run(r).fold(ZIO.fail, azb).run(r))

  def map[B](ab: A => B): ZIO[R, E, B] =
    ZIO(r => run(r).map(ab))
    // ZIO(r =>  run(r).fold(e => Left(e), a => Right(ab(a))))

  def catchAll[R1 <: R, E2, A1 >: A](h: E => ZIO[R1, E2, A1]): ZIO[R1, E2, A1] =
    ZIO(r => run(r).fold(h, ZIO.succeed).run(r))

  def mapError[E2](h: E => E2): ZIO[R, E2, A] =
    ZIO(r => run(r).left.map(h))

  def provide(r: => R): ZIO[Any, E, A] =
    ZIO(_ => run(r))

  def provideSome[R0](f: R0 => R): ZIO[R0, E, A] =
    ZIO.accessM(r0 => provide(f(r0)))
    // for
    //   a <- ZIO.accessM[R0](r0 => provide(f(r0)))
    //   // r0 <- ZIO.environment
    //   // a  <- provide(f(r0))
    // yield a

  // R              is Has[ZEnv] & Has[BusinessLogic]
  //             R1 is             Has[BusinessLogic]
  // Has[ZEnv] & R1 is Has[ZEnv] & Has[BusinessLogic]
  def provideCustomLayer[R1 <: Has[?]](r1: => R1)(using ZEnv & R1 => R): ZIO[ZEnv, E, A] =
    provideSome[ZEnv](_.union(r1).asInstanceOf[R])

  def provideCustom[R1 : ClassTag](r1: => R1)(using ZEnv & Has[R1] => R): ZIO[ZEnv, E, A] =
    provideCustomLayer(Has(r1))

object ZIO:
  def succeed[A](a: => A): ZIO[Any, Nothing, A] = ZIO(r => Right(a))

  def fail[E](e: => E): ZIO[Any, E, Nothing] = ZIO(r => Left(e))

  def effect[A](a: => A): ZIO[Any, Throwable, A] =
    ZIO(r => try Right(a) catch Left(_))

  def fromFunction[R, A](run: R => A): ZIO[R, Nothing, A] =
    ZIO(r => Right(run(r)))

  def identity[R]: ZIO[R, Nothing, R] =
    ZIO.fromFunction(Predef.identity)

  inline def environment[R]: ZIO[R, Nothing, R] = identity

  inline def access[R]: AccessPartiallyApplied[R] = AccessPartiallyApplied()

  inline def accessM[R]: AccessMPartiallyApplied[R] = AccessMPartiallyApplied()

  final class AccessPartiallyApplied[R]():
    def apply[A](f: R => A): ZIO[R, Nothing, A] =
      environment[R].map(f)

  final class AccessMPartiallyApplied[R]():
    def apply[E, A](f: R => ZIO[R, E, A]): ZIO[R, E, A] =
      environment.flatMap(f)

  // ! not part of the real ZIO
  inline def read[R]: ZIO[R, Nothing, R] =
    identity


object console:
  type Console = Has[Console.Service]

  object Console:
    trait Service:
      def putStrLn(line: => String): ZIO[Any, Nothing, Unit]
      def getStrLn: ZIO[Any, Nothing, String]

    lazy val live: ZIO[Any, Nothing, Service] =
      ZIO.succeed(make)

    lazy val make: Service =
      new:
        def putStrLn(line: => String) = ZIO.succeed(println(line))
        lazy val getStrLn = ZIO.succeed(scala.io.StdIn.readLine)

  def putStrLn(line: => String): ZIO[Console, Nothing, Unit] =
    ZIO.accessM(_.get.putStrLn(line))

  def getStrLn: ZIO[Console, Nothing, String] =
    ZIO.accessM(_.get.getStrLn)


object Runtime:
  object default:
    def unsafeRunSync[E, A](zio: => ZIO[ZEnv, E, A]): Either[E, A] =
      zio.run(Has(console.Console.make))

type ZEnv = Has[console.Console.Service]


final class Has[A](private val map: Map[String, Any])
object Has:
  def apply[A](a: A)(using tag: ClassTag[A]): Has[A] =
    new Has(Map(tag.toString -> a))

  extension [A <: Has[?]](a: A)
    // @annotation.targetName("union")
    def ++[B <: Has[?]](b: B): A & B =
      union(b)

    infix def union[B <: Has[?]](b: B): A & B =
      new Has(a.map ++ b.map).asInstanceOf[A & B]

    // ! Do NOT change the order of the parameter lists.
    // ! The current order is more type inference friendly.
    // def get[S](using tag: ClassTag[S], view: A <:< Has[S]): S =
    def get[S](using A => Has[S])(using tag: ClassTag[S]): S =
      a.map(tag.toString).asInstanceOf[S]
