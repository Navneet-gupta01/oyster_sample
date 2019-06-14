package com.navneetgupta.repository

import cats.Applicative

trait ZoneRepository extends Serializable {
  val zoneRepository: ZoneRepository.Service[Any]
}

object ZoneRepository {
  trait Service[R] extends Serializable {
    def getZoneByCode(code: String)
  }
//  class InMemoryZoneRepository[F[_]: Applicative]
}