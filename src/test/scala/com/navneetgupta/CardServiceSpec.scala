package com.navneetgupta

import cats.effect.IO
import com.navneetgupta.domain._
import org.scalatest.{Matchers, Outcome, fixture}

import scala.util.Random

class CardServiceSpec extends TestSetup with fixture.FunSpecLike with Matchers{
  override type FixtureParam = TestSetup[IO]
  override def withFixture(test: OneArgTest): Outcome = test(new TestSetup[IO]())


  describe("CardServices.CreateCard") {
    it("should be able to create card") {fixture =>
      val createCardResult = fixture.cardServices.createCard(None)
      val oysterCard = createCardResult.unsafeRunSync()
      oysterCard.balance shouldBe 0.0D
      oysterCard.lastBarrier shouldBe None
    }
  }

  describe("CardServices.UpdateBalance") {

    it("should be able to add balance to valid card") { fixture =>
      val getBalance = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(30.0D, card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      getBalance.unsafeRunSync() shouldBe Right(30.0D)
    }

    it("should fail to add balance to Invalid card") { fixture =>
      val invalidCardNumber = (new Random()).nextLong()
      val getBalance = for {
        balance <- fixture.cardServices.updateBalance(35.0D, invalidCardNumber)
      } yield balance

      getBalance.unsafeRunSync() shouldBe Left(CardDoesNotExistError)
    }

  }

  describe("CardServices.GetBalance") {

    it("should be able to get balance from valid card") { fixture =>
      val getBalance = for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(30.0D, card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      getBalance.unsafeRunSync() shouldBe Right(30.0D)
    }

    it("should fail to add balance to Invalid card") { fixture =>
      val invalidCardNumber = (new Random()).nextLong()
      val getBalance = for {
        balance <- fixture.cardServices.updateBalance(35.0D, invalidCardNumber)
      } yield balance

      getBalance.unsafeRunSync() shouldBe Left(CardDoesNotExistError)
    }

    it("should fail to get balance for an invalid card ") {fixture =>
      val invalidCardNumber = (new Random()).nextLong()
      val getBalance = for {
        balance <- fixture.cardServices.getBalance(invalidCardNumber)
      } yield balance

      getBalance.unsafeRunSync() shouldBe Left(CardDoesNotExistError)
    }
  }


  describe("CardServices.createJourney") {
    it("should allow bus journey with card Balance >= 1.8D") {fixture =>
      val getBalance =  for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(1.8D, card.number)
        journey <- fixture.cardServices.createJourney(domain.Barrier("HOL", BusJourney, Direction.CHECK_IN), card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      getBalance.unsafeRunSync() shouldBe Right(0.0D)
    }

    it("should not allow bus journey with Invalid Card") {fixture =>
      val createJourney =  for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(3.0D, card.number)
        journey <- fixture.cardServices.createJourney(domain.Barrier("HOL", BusJourney, Direction.CHECK_IN),233)
      } yield journey

      createJourney.unsafeRunSync() shouldBe Left(CardDoesNotExistError)
    }

    it("should not allow bus journey with card Balance < 1.8D") {fixture =>
      val createJourney =  for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(1.5D, card.number)
        journey <- fixture.cardServices.createJourney(domain.Barrier("HOL", BusJourney, Direction.CHECK_IN), card.number)
      } yield journey

      createJourney.unsafeRunSync() shouldBe Left(MinBalanceError)
    }

    it("should allow tube journey with card Balance >= 3.2D") {fixture =>
      val getBalance =  for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(3.2D, card.number)
        journey <- fixture.cardServices.createJourney(domain.Barrier("HOL", TubeJourney, Direction.CHECK_IN), card.number)
        balance <- fixture.cardServices.getBalance(card.number)
      } yield balance

      getBalance.unsafeRunSync() shouldBe Right(0.0D)
    }

    it("should not allow bus journey with Invalid Card") {fixture =>
      val createJourney =  for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(3.2D, card.number)
        journey <- fixture.cardServices.createJourney(domain.Barrier("HOL", TubeJourney, Direction.CHECK_IN),233)
      } yield journey

      createJourney.unsafeRunSync() shouldBe Left(CardDoesNotExistError)
    }

    it("should not allow bus journey with card Balance < 3.2D") {fixture =>
      val createJourney =  for {
        card <- fixture.cardServices.createCard(None)
        addBalance <- fixture.cardServices.updateBalance(1.5D, card.number)
        journey <- fixture.cardServices.createJourney(domain.Barrier("HOL", TubeJourney, Direction.CHECK_IN), card.number)
      } yield journey

      createJourney.unsafeRunSync() shouldBe Left(MinBalanceError)
    }
  }
}
