package com.navneetgupta.domain

import java.util.Date

import cats.data.NonEmptyList


sealed trait JourneyType extends Product with Serializable
case object BusJourney extends JourneyType
case object TubeJourney extends JourneyType

object Direction extends Enumeration {
  type Direction = Value
  val CHECK_IN, CHECK_OUT = Value
}
case class Barrier(stationCode: String, journeyType: JourneyType, direction: Direction.Value, cost: Double)

case class Journey(from: Barrier, to: Option[Barrier], date: Date)
case class OysterCard(number: Long, balance: Double, lastJourney: Journey)

case class Station(stationCode: String, stationName: String, zones: NonEmptyList[Int])