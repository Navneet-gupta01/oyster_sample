package com.navneetgupta.domain

import cats.Monad

object Services {
}

trait CardServices[F[_]: Monad] {}

trait ZonesService[F[_]: Monad]