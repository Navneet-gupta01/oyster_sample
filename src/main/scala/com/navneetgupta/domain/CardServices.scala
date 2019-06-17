package com.navneetgupta.domain


import java.util.Date

import cats.{Monad}
import cats.data.EitherT
import cats.implicits._

class CardServices[F[_]](cardsRepository: CardsRepository[F],
                         zoneServices: ZoneServices[F]) {

  def createCard(amount: Option[Double]) = cardsRepository.createCard(amount)

  def updateBalance(amountToAdd: Double, cardNumber: Long)(
    implicit M: Monad[F]): F[Either[ValidationError, OysterCard]] = {
    (for {
      card <- EitherT.fromOptionF[F, ValidationError, OysterCard](
        cardsRepository.getCard(cardNumber),
        CardDoesNotExistError)
      updatedCard <- EitherT.fromOptionF[F, ValidationError, OysterCard](
        cardsRepository.updateCard(
          card.copy(balance = card.balance + amountToAdd)),
        BalanceUpdateError)
    } yield updatedCard).value
  }

  def getBalance(cardNumber: Long)(
    implicit M: Monad[F]): F[Either[ValidationError, Double]] =
    (for {
      cardBalance <- EitherT.fromOptionF[F, ValidationError, Double](
        cardsRepository.getCard(cardNumber).fmap(_.map(_.balance)),
        CardDoesNotExistError)
    } yield cardBalance).value

  def createJourney(barrier: Barrier, cardNumber: Long)(implicit M: Monad[F]) =
  {
    println("create Card Services")
    (for {
      card <- EitherT.fromOptionF[F, ValidationError, OysterCard](
        cardsRepository.getCard(cardNumber),
        CardDoesNotExistError)
      crossedBarrier <- barrier.journeyType match {
        case BusJourney  => busJourney(barrier, card)
        case TubeJourney => tubeJourney(barrier, card)
      }
      updateCard <- EitherT.fromOptionF[F, ValidationError, OysterCard](
        cardsRepository.updateCard(
          card.copy(balance = (card.balance - crossedBarrier.fare),
            lastBarrier = crossedBarrier.copy(crossedAt = new Date()).some,
          )),
        CreateJourneyError)
    } yield crossedBarrier).value
  }



  private def tubeJourney(barrier: Barrier, card: OysterCard)(
    implicit M: Monad[F]): EitherT[F, ValidationError, Barrier] =
    for {
      minBalanceValidation        <- EitherT{minBalanceValidation(barrier, card)}
      print = println(minBalanceValidation)
      updatedBarrierWihtFare      <- EitherT{processTubeJourney(minBalanceValidation, card)}
    } yield updatedBarrierWihtFare

  private def busJourney(barrier: Barrier, card: OysterCard)(
    implicit M: Monad[F]): EitherT[F, ValidationError, Barrier] =
    for {
      minBalanceValidation    <- EitherT {minBalanceValidation(barrier, card)}
      print = println(minBalanceValidation)
      updatedBarrierWithFare  <- EitherT{calculateBusFare(minBalanceValidation)}
    } yield updatedBarrierWithFare

  private def minBalanceValidation(barrier: Barrier, card: OysterCard)(
    implicit M: Monad[F]): F[Either[ValidationError, Barrier]] = {
    (CardServices.MIN_BALANCE_FOR_CHECK_IN
      .get(barrier.journeyType)
      .filter(_ <= card.balance)
      .fold({
        Either.left[ValidationError, Barrier](MinBalanceError)})(v => {
        Right(barrier)}))
      .pure[F]
  }

  // If User is trying to continuously IN multiple time, he should not be charger again.
  private def isContinuousCheckIN(lastCheckIn: Date, currentCheckIN: Date): Boolean = {
    (currentCheckIN.getTime-lastCheckIn.getTime) < 5000
  }


  private def processTubeJourney(
                                  barrier: Barrier,
                                  card: OysterCard)(implicit M: Monad[F]): F[Either[ValidationError, Barrier]] = {
    card.lastBarrier.fold(
      if(barrier.direction == Direction.CHECK_IN)
        Either.right[ValidationError, Barrier](barrier.copy(fare = 3.2D)).pure[F]
      else Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[F])(lastBarrier => {
      lastBarrier.journeyType match {
        case BusJourney =>  if(barrier.direction == Direction.CHECK_IN)
          Either.right[ValidationError, Barrier](barrier.copy(fare = 3.2D)).pure[F]
        else
          Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[F]
        case TubeJourney =>
          (lastBarrier.direction, barrier.direction) match {
            case (Direction.CHECK_OUT, Direction.CHECK_OUT) =>
              Either.left[ValidationError, Barrier](BarrierNotCheckedIN).pure[F]
            case (Direction.CHECK_IN, Direction.CHECK_OUT) =>
              calculateMinTubeFare(barrier, lastBarrier).map(Either.right(_))
            case _ =>
              Either.right[ValidationError, Barrier](barrier.copy(fare = 3.2D)).pure[F]
          }
      }
    })
  }

  private def calculateBusFare(barrier: Barrier)(implicit M: Monad[F]): F[Either[ValidationError,Barrier]] =
    Either.right[ValidationError, Barrier](barrier.copy(fare = 1.8D)).pure[F]

  private def calculateMinTubeFare(to: Barrier, from: Barrier)(implicit M: Monad[F]): F[Barrier] = {
    for {
      fromZones <- zoneServices.getZoneByStationCode(from.stationCode)
      toZones <- zoneServices.getZoneByStationCode(to.stationCode)
    } yield {
      //      val minZonesCrossed = zoneServices.getMinNumberOfZonesCrossed(fromZones, toZones)
      // refund the excess fare charged
      val minFare = minFareCalc(fromZones,toZones)
      println(s"calculated MinFare $minFare")
      val barrierC = to.copy(fare = minFare - 3.2D)
      println(s"barrier $barrierC")
      barrierC
    }
  }

  private def minFareCalc(fromZones: List[Int], toZones: List[Int]): Double = {
    println(s"fromZones : $fromZones ,toZones: $toZones")
    fromZones.foldLeft(3.2D)((min, fromZone) =>
      toZones.foldLeft(min)((localMin, toZone) => {
        val fareC = calculateTubeFare(List(fromZone, toZone), toZone - fromZone + 1)
        if (localMin > fareC) fareC else localMin
      }))
  }

  private def calculateTubeFare(crossedZones: List[Int], minNumberOfZonesCrossed: Long) : Double = {
    val cost = (crossedZones.contains(1), minNumberOfZonesCrossed) match {
      case (true, 1) => 2.5D
      case (true, 2) => 3.0D
      case (false, 1) => 2.0D
      case (false, 2) => 2.25D
      case _ => 3.2D
    }
    println(s"zones crossed: $crossedZones, minZonesCrossed : $minNumberOfZonesCrossed, fare: $cost, contains: ${crossedZones.contains(1)}")
    cost
  }
}

object CardServices {
  val MIN_BALANCE_FOR_CHECK_IN = Map(BusJourney -> 1.8D, TubeJourney -> 3.2D)

  def apply[F[_]](cardsRepository: CardsRepository[F],
                  zoneServices: ZoneServices[F]): CardServices[F] = new CardServices(cardsRepository, zoneServices)
}
