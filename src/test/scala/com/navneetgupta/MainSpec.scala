package com.navneetgupta

import cats.Monad
import cats.effect.IO
import com.navneetgupta.domain.{Common, Programs}
import org.scalatest.{Matchers, Outcome, fixture}

class MainSpec extends TestSetup with fixture.FunSpecLike with Matchers {

  case class TestData(input: List[String], output: List[String]) {
    def putStrLn(line: String): (TestData, Unit) =
      (copy(output = line :: output), Unit)

    def readLn(): (TestData, String) =
      (copy(input = input.drop(1)), input.head)

    def showResults = output.reverse.mkString("\n")
  }

  case class TestIO[A](run: TestData => (TestData, A)) {
    self =>
    def map[B](ab: A => B): TestIO[B] = TestIO(t => self.run(t) match {
      case (t, a) => (t, ab(a))
    })

    def flatMap[B](afb: A => TestIO[B]): TestIO[B] = TestIO(t => self.run(t) match {
      case (t, a) => afb(a).run(t)
    })

    def eval(t: TestData): TestData = run(t)._1
  }

  object TestIO {
    def pure[A](a: => A): TestIO[A] = TestIO(t => (t, a))


    implicit val ConsoleTestIO = new Common.Console[TestIO] {
      def putStrLn(line: String): TestIO[Unit] = TestIO(t => t.putStrLn(line))

      def readLn(): TestIO[String] = TestIO(t => t.readLn())
    }
  }
  implicit val testIOMonad: Monad[TestIO] = new Monad[TestIO] {
    override def flatMap[A, B](opt: TestIO[A])(fn: A => TestIO[B]): TestIO[B] = opt.flatMap(fn)

    override def pure[A](opt: A): TestIO[A] = TestIO.pure(opt)

//    @tailrec
    def tailRecM[A, B](a: A)(fn: A => TestIO[Either[A, B]]): TestIO[B] = ???
//      fn(a) match {
//        case None           => None
//        case Some(Left(a1)) => tailRecM(a1)(fn)
//        case Some(Right(b)) => Some(b)
//      }
  }
  def programTest : TestIO[Unit] = Programs.program[TestIO]

  val testData = TestData(
    input = "1" :: "12" :: "43" :: Nil,
    output = Nil
  )
//
  def runTest = programTest.eval(testData).showResults

  override type FixtureParam = TestSetup[IO]

  override def withFixture(test: OneArgTest): Outcome = test(new TestSetup[IO]())


  // should work fine except the card number generated in random
  describe("Program should work") {
    it ("should work fine") {fixture =>
      val resp =
        s"""
          Starting The Program

          ${Programs.inputs}

          Please enter the amount default[0]

          Card Created Successfully, Your Card Number is: 6467167174812780216 and balance is: 12.0

          ${Programs.inputs}

          Invalid Option Selected. Exiting Application !!
        """.stripMargin

      runTest shouldBe resp
    }
  }
}
