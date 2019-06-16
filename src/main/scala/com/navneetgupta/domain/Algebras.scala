package com.navneetgupta.domain

trait FareAlgebra[F[_]] {
  def calculateBusFare(from: Barrier, to: Barrier): F[Long]
}
