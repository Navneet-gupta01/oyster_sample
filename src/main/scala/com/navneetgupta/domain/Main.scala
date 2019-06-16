package com.navneetgupta.domain

import cats.Monad
import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}

object Main { //extends IOApp {


//  trait Console[F[_]] {
//    def putStrLn(str: String): F[Unit]
//
//    def readLn(): F[String]
//  }
//
//  object Console {
//    def apply[F[_]](implicit F: Console[F]): Console[F] = F
//  }
//
//  def putStrLn[F[_] : Console](line: String): F[Unit] = Console[F].putStrLn(line)
//
//  def readLn[F[_] : Console](): F[String] = Console[F].readLn()
//
//  implicit val ConsoleIO = new Console[IO] {
//    def putStrLn(line: String): IO[Unit] = IO(println(line))
//
//    def readLn(): IO[String] = IO(scala.io.StdIn.readLine)
//  }
//  object Validation {
//    def validateDouble(num: String): Option[Double] = scala.util.Try(num.toDouble).toOption
//    def validateLong(num: String): Option[Long] = scala.util.Try(num.toLong).toOption
//  }
//
//  val inputs =
//    """
//       Please select Options from below Menu
//       [1] Create a Card
//       [2] Recharge Card
//       [3] Get Balance
//       [4] Proceed With Journey
//    """.stripMargin
//
//  def createCardOption[F[_]: Console: Monad](cardServices: CardServices[F]): F[Unit] = for {
//    _ <- putStrLn("Please enter the amount default 0")
//    amount <- readLn()
//    amountValidate <- Validation.validateDouble(amount).pure[F]
//    card <- cardServices.createCard(amountValidate)
//    _ <- putStrLn(s"Card Created Successfully, Your Card Number is: ${card.number} and balance is: ${card.balance}")
//  } yield ()
//
//  def getBalance[F[_]: Console: Monad](cardServices: CardServices[F]): F[Unit] =
//    for {
//    _ <- putStrLn("Please enter the card Number")
//    cardNumber <- readLn()
//    _ <- Validation.validateLong(cardNumber).fold(
//      putStrLn("Invalid Card Number."))(number =>
//      cardServices.getBalance(number).flatMap(x => {
//        x match {
//          case Right(balance) => putStrLn(s"Your Card Balance is ${balance}")
//          case Left(_) => putStrLn("Invalid Card Number")
//        }
//      }))
//  } yield ()

//  def addBalance[F[_]: Console: Monad](cardServices: CardServices[F]): F[Unit] =
//    for {
//      _ <- putStrLn("Please enter the card Number")
//      cardNumber <- readLn()
//      _ <- putStrLn("Please enter the amount to add")
//      _ <- Validation.validateLong(cardNumber).fold(
//        putStrLn("Invalid Card Number."))(number =>
//
//    }
//  def loop(n: Int) : IO[ExitCode] = IO.suspend()
}
