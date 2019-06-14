package com.navneetgupta.infra

import com.navneetgupta.domain.{Journey, OysterCard}

trait CardRepository[F[_]] {
  def createCard(amount: Double): F[OysterCard]
  def getBalance(cardNumber: Long): F[Double]
  def updateBalance(cardNumber: Long, cost: Double)
}
class InMemoryCardRepository {

}
