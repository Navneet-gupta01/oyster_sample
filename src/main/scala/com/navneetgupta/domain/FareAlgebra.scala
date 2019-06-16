package com.navneetgupta.domain

import cats.Monad
import cats.implicits._

/*
*
* Journey
Fare
Anywhere in Zone 1
£2.50
Any one zone outside zone 1
£2.00
Any two zones including zone 1
£3.00
Any two zones excluding zone 1
£2.25
Any three zones
£3.20
Any bus journey
£1.80*/

class FareAlgebraInterpreter[F[_]: Monad] extends FareAlgebra[F] {
  override def calculateBusFare(fromZones: List[Long], toZones: List[Long], minZonesCrossed: Int): F[Long] =
    for {
      crossedZoneOne <- crossedZoneOne(fromZones ++ toZones)
    } yield ()


  private def crossedZoneOne(zones: List[Long]): F[Boolean] = {
    zones.contains(1L).pure[F]
  }



}
