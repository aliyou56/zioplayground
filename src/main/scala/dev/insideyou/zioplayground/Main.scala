package dev.insideyou
package zioplayground

// import zio.*

object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  object BusinessLogic:
    trait Service:
      def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[Any, Nothing, Boolean]

    lazy val live: ZIO[Google, Nothing, Service] =
      ZIO.fromFunction(make)

    def make(google: Google): Service =
      new:
        override def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[Any, Nothing, Boolean] =
          google.countPicturesOf(topic).map(_ % 2 == 0)

  def doesGoogleHaveEvenAmountOfPicturesOf(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.doesGoogleHaveEvenAmountOfPicturesOf(topic))

trait Google:
  def countPicturesOf(topic: String): ZIO[Any, Nothing, Int]

object GoogleImpl:
  lazy val live: ZIO[Any, Nothing, Google] =
    ZIO.succeed(make)

  lazy val make: Google =
    new:
      override def countPicturesOf(topic: String): ZIO[Any, Nothing, Int] =
        ZIO.succeed(if topic == "cats" then 1337 else 1338)

object DependencyGraph:
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic.Service] =
    for
      g  <- GoogleImpl.live
      bl <- businessLogic.BusinessLogic.live.provide(g)
    yield bl

  lazy val make: businessLogic.BusinessLogic.Service =
    val g  = GoogleImpl.make
    val bl = businessLogic.BusinessLogic.make(g)

    bl

object Main extends scala.App:
  // Runtime.default.unsafeRunSync(program.provide(DependencyGraph.make))
  Runtime.default.unsafeRunSync(program)

  lazy val program =
    for
      bl <- DependencyGraph.live
      // p <- makeProgram.provide(Has(bl) union Has(console.Service.make))
      // p <- makeProgram.provideSome[Has[ZEnv]](_ union Has(bl))
      // p <- makeProgram.provideCustomLayer(Has(bl))
      // p <- makeProgram.provideCustom(bl)
      p <- makeProgram.provideSome[ZEnv](_ `union` Has(bl))
    yield p

  // def makeProgram(bl: businessLogic.BusinessLogic) =
  lazy val makeProgram =
    for
      // env <- ZIO.environment[console.Console & businessLogic.BusinessLogic]
      // businessLogic <- ZIO.fromFunction[BusinessLogic, BusinessLogic](identity)
      // businessLogic <- ZIO.environment //[BusinessLogic]
      _ <- console.putStrLn("-" * 100)
      cats <- businessLogic.doesGoogleHaveEvenAmountOfPicturesOf("cats")
      _ <- console.putStrLn(cats.toString)

      dogs <- businessLogic.doesGoogleHaveEvenAmountOfPicturesOf("dogs")
      _ <- console.putStrLn(dogs.toString)
      _ <- console.putStrLn("-" * 100)
    yield ()

// extension [R, A](run: R => A)
//   def provide(r: => R): Any => A =
//     _ => run(r)

// def succeed[A](a: => A): Any => A =
//   _ => a





// object Main extends scala.App:
//   val trace = s"[${Service.BLUE}TRACE${Service.RESET}]"

//   println(Runtime.default.unsafeRunSync(program))
//   // override def run(args: List[String]) =
//   //   program.exitCode

//   lazy val program =
//     for
//       _ <- console.putStrLn("─" * 100)

//       _ <- console.putStrLn("What is your name")
//       name <- ZIO.succeed("Aliyou")
//       _ <- console.putStrLn(s"Hello $name")

//       // _ <- ZIO
//       //   .effect(throw RuntimeException("boom"))
//       //   .mapError(_.getMessage)
//         // .catchAll(_ => ZIO.succeed(println(_)))
//       // _ <- ZIO.fail("boom")

//       _ <- console.putStrLn("─" * 100)
//     yield ()





// abstract class Animal
// final class Dog(name: String) extends Animal

// abstract class AnimalShelter[+A]:
//   def adopt(name: String): A

// final class DogShelter extends AnimalShelter[Dog]:
//   override def adopt(name: String): Dog =
//     Dog(name)

// abstract class Vet[-A]:
//   def diagnose(a: A): String

// final class ExperienceVet extends Vet[Animal]:
//   override def diagnose(animal: Animal): String =
//     s"$animal will be fine for sure!"

// // abstract class Vet:
// //   def diagnose: Dog => String
// // final class ExperienceVet extends Vet:
// //   override val diagnose: Animal => String =
// //     animal => s"$animal will be fine for sure!"

// object Main extends scala.App:
//   val shelter: AnimalShelter[Animal] = new DogShelter
//   val animal = shelter.adopt("dog")
//   println(animal)
