package com.navneetgupta.domain

import cats.effect.{ExitCode, IOApp, IO}

object Main extends IOApp {


  trait Console[F[_]] {
    def putStrLn(str: String): F[Unit]

    def readLn(): F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }

  def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)

  def readLn[F[_] : Console](): F[String] = Console[F].readLn()

  def createCard[F[_]: CardServices](): F[OysterCard] = CardServices[F]

  implicit val ConsoleIO = new Console[IO] {
    def putStrLn(line: String): IO[Unit] = IO(println(line))

    def readLn(): IO[String] = IO(scala.io.StdIn.readLine)
  }

  val inputs =
    """
       Please select Options from below Menu
       [1] Create a Card
       [2] Recharge Card
       [3] Get Balance
       [4] Proceed With Journey
    """.stripMargin

  def createCardOption[F[_]: Console](cardServices: CardServices[F]): F[Unit] = for {
    _ <- putStrLn("Please enter the amount default 0")
    amount <- readLn()
    _ <- cardServices.createCard(scala)
  } yield ()
  def loop(n: Int) : IO[ExitCode] = IO.suspend()
}
