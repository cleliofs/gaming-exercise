package com.company

import scala.collection.mutable.ArrayBuffer

/**
 * Gaming Service
 *
 * Created by clelio on 23/06/15.
 */
sealed trait GamingService {

  def receive(gameEvent: String): Unit

  def allEvents: Seq[GameEvent]

  def lastEvent: Option[GameEvent]

  def lastEvents(n: Int): Seq[GameEvent]

  def lastTeamToScore: Option[Int]

  def lastScoredPoints: Option[Int]

  def timeForLastScoredPoints: Option[Int]

  def latestScore: Option[(Int, Int)]
}


class GamingServiceImpl extends GamingService {

  val EventStreamPattern = """[01]{1}([01]{12})([01]{8})([01]{8})([01]{1})([01]{2})""".r
  var events: collection.mutable.ArrayBuffer[GameEvent] = new ArrayBuffer()

  override def receive(gameEventStr: String): Unit = {
    def isValidEvent(gameEvent: GameEvent): Boolean = {
      val toValidate = (lastEvent, gameEvent.time, 1 to 3 contains gameEvent.pointsScored, gameEvent.pointsScored, gameEvent.whoScored, gameEvent.totalPointsTeam1, gameEvent.totalPointsTeam2)
      toValidate match {
        case (_, _, false, _, _, _, _) => false
        case (None, _, true, points, 0, pointsT1, pointsT2) if pointsT1 == points && pointsT2 == 0 => true
        case (None, _, true, points, 1, pointsT1, pointsT2) if pointsT2 == points && pointsT1 == 0 => true
        case (Some(e), time, _, _, _, _, _) if e.time > time => false
        case (Some(e), _, true, points, 0, pointsT1, pointsT2) if (pointsT1 == e.totalPointsTeam1 + points) && (pointsT2 == e.totalPointsTeam2) => true
        case (Some(e), _, true, points, 1, pointsT1, pointsT2) if (pointsT2 == e.totalPointsTeam2 + points) && (pointsT1 == e.totalPointsTeam1) => true
        case _ => false
      }
    }

    if (gameEventStr.trim() != "") {
      val e: Option[GameEvent] = Integer.parseInt(gameEventStr.drop(2), 16).toBinaryString.reverse.padTo(32, '0').reverse match {
        case EventStreamPattern(time, totalPointsT1, totalPointsT2, whoScored, pointsScored) => {
          val event = GameEvent(Integer.parseInt(time, 2), Integer.parseInt(totalPointsT1, 2), Integer.parseInt(totalPointsT2, 2), Integer.parseInt(whoScored, 2), Integer.parseInt(pointsScored, 2))
          if (isValidEvent(event)) Some(event) else None
        }
      }

      if (e.isDefined) events += e.get
    }
  }

  def allEvents = if (events.isEmpty) Seq() else events

  def lastEvent: Option[GameEvent] = if (events.isEmpty) None else Some(events.last)

  def lastEvents(n: Int): Seq[GameEvent] = if (events.isEmpty || events.size < n) Seq() else events.takeRight(n)

  def lastTeamToScore = if (events.isEmpty) None else Some(events.last.whoScored)

  def lastScoredPoints: Option[Int] = if (events.isEmpty) None else Some(events.last.pointsScored)

  def timeForLastScoredPoints: Option[Int] = if (events.isEmpty) None else Some(events.last.time)

  def latestScore: Option[(Int, Int)] = if (events.isEmpty) None else Some((events.last.totalPointsTeam1, events.last.totalPointsTeam2))
}

