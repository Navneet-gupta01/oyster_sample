package com.navneetgupta.infra

import cats.Monad
import com.navneetgupta.domain.{CardsRepository, OysterCard}
import cats.implicits._
import scala.collection.concurrent.TrieMap
import scala.util.Random


class InMemoryCardsRepositoryInterpreter[F[_]: Monad] extends CardsRepository[F] {
  private val cache = new TrieMap[Long, OysterCard]

  private val random = new Random
  private val defaultAmount = 0.0


  override def createCard(amount: Option[Double]): F[OysterCard] = {
    val cardToSave = OysterCard(random.nextLong, amount.getOrElse(defaultAmount))
    cache.put(cardToSave.number,cardToSave)
    cardToSave.pure[F]

  }

  override def getCard(cardNumber: Long): F[Option[OysterCard]] = cache.get(cardNumber).pure[F]

  override def updateCard(card: OysterCard): F[Option[OysterCard]] = {
    cache.put(card.number, card).pure[F] *> cache.get(card.number).pure[F]
  }

}

object InMemoryCardsRepositoryInterpreter {
  def apply[F[_]: Monad](): InMemoryCardsRepositoryInterpreter[F] = new InMemoryCardsRepositoryInterpreter[F]()
}
