package com.company

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}
import scalaz.Validation._
import scalaz.ValidationNel
import scalaz.syntax.apply._

/**
 * Gaming Service which provides method to gather events from the events streams
 * and also provides methods to query the recoreded events.
 *
 * This service also manages invalid events coming. Those will be discarded properly.
 *
 * Created by clelio on 23/06/15.
 */
sealed trait GamingService {

  /**
   * Receive events from events stream and process them,
   * i.e. parsing, validating and storing them.
   *
   * @param gameEventStr
   */
  def receive(gameEventStr: String): Option[GameEvent]

  /**
   * Returns all recorded valid events. It returns
   * an empty sequence in case non-valid recorded event.
   *
   * @return
   */
  def allEvents: Seq[GameEvent]

  /**
   * Returns the last recorded event, or a Option.None where there is no recorded event.
   *
   * @return
   */
  def lastEvent: Option[GameEvent]

  /**
   * Returns the last 'n' recorded events
   *
   * @param n number of latest events to fetch
   * @return
   */
  def lastEvents(n: Int): Seq[GameEvent]

  /**
   * Returns the last team who has scored.
   *
   * @return 0 for Team 1, or 1 for Team 2,
   *         or a Option.None where there is no recorded event
   */
  def lastTeamToScore: Option[Int]

  /**
   * Returns how many points were scored by last, regardless of which team,
   * or a Option.None where there is no recorded event
   *
   * @return
   */
  def lastScoredPoints: Option[Int]

  /**
   * Returns the time when the last points were scored, regardless of which team,
   * or a Option.None where there is no recorded event
   *
   * @return
   */
  def timeForLastScoredPoints: Option[Int]

  /**
   * Returns the latest match score
   *
   * @return A tuple containing the pointsForTeam1 and pointsForTeam2, (pointsForTeam1, pointsForTeam2),
   *         or a Option.None where there is no recorded event
   */
  def latestScore: Option[(Int, Int)]

  /**
   * Returns all events scored by a certain team
   *
   * @param team 0 for Team 1 or 1 for Team 2
   * @return a list of all events scored by a certain team,
   *         or a Option.None where there is no recorded event
   */
  def eventsByTeam(team: Int): Seq[GameEvent]

  /**
   * Last event scored by team.
   *
   * @param team 0 for Team 1 or 1 for Team 2
   * @return the last event by team, or Option.None where there is no recorded event
   */
  def lastEventScoredByTeam(team: Int): Option[GameEvent]
}

case class InvalidEvent(reason: String)


class GamingServiceImpl extends GamingService {

  private val EventStreamPattern = """[01]{1}([01]{12})([01]{8})([01]{8})([01]{1})([01]{2})""".r

  // visibility to private object level only
  private[this] var events: collection.mutable.ArrayBuffer[GameEvent] = new ArrayBuffer()

  def receive(gameEventStr: String): Option[GameEvent]  = {

    def validateEvent(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = {
      val toValidate = (lastEvent, gameEvent.time, 1 to 3 contains gameEvent.pointsScored, gameEvent.pointsScored, gameEvent.whoScored, gameEvent.totalPointsTeam1, gameEvent.totalPointsTeam2)

      def validatePoints(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (_, _, false, _, _, _, _) => failureNel(InvalidEvent("Points scored outside possible input"))
        case _ => success(true)
      }

      def validateFirstPointsForTeam1NotComputed(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (None, _, true, points, 0, pointsT1, pointsT2) if pointsT1 != points => failureNel(InvalidEvent("First points of the match scored for Team 1 not computed"))
        case _ => success(true)
      }

      def validateFirstPointsForTeam2NotComputed(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (None, _, true, points, 1, pointsT1, pointsT2) if pointsT2 != points => failureNel(InvalidEvent("First points of the match scored for Team 2 not computed"))
        case _ => success(true)
      }

      def validateFirstPointsForTeam1(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (None, _, true, points, 0, pointsT1, pointsT2) if pointsT2 != 0 => failureNel(InvalidEvent("First points of the match scored for Team 1. Invalid score for Team 2"))
        case _ => success(true)
      }

      def validateFirstPointsForTeam2(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (None, _, true, points, 1, pointsT1, pointsT2) if pointsT1 != 0 => failureNel(InvalidEvent("First points of the match scored for Team 2. Invalid score for Team 1"))
        case _ => success(true)
      }

      def validateCurrentEventTiming(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), time, _, _, _, _, _) if e.time > time => failureNel(InvalidEvent("Current event timing is invalid (> last recorded event)"))
        case _ => success(true)
      }

      def validateScoreForTeam1(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), _, true, points, 0, pointsT1, pointsT2) if pointsT1 != e.totalPointsTeam1 + points => failureNel(InvalidEvent("Team 1 score is invalid"))
        case _ => success(true)
      }

      def validateScoreForTeam2(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), _, true, points, 1, pointsT1, pointsT2) if pointsT2 != e.totalPointsTeam2 + points => failureNel(InvalidEvent("Team 2 score is invalid"))
        case _ => success(true)
      }

      def validateRecordedPointsForTeam1(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), _, true, points, 0, pointsT1, pointsT2) if pointsT2 != e.totalPointsTeam2 => failureNel(InvalidEvent("Invalid registered points for Team 2"))
        case _ => success(true)
      }

      def validateRecordedPointsForTeam2(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), _, true, points, 1, pointsT1, pointsT2) if pointsT1 != e.totalPointsTeam1 => failureNel(InvalidEvent("Invalid registered points for Team 2"))
        case _ => success(true)
      }

      val valid = validatePoints(gameEvent) |@|
                  validateFirstPointsForTeam1NotComputed(gameEvent) |@|
                  validateFirstPointsForTeam2NotComputed(gameEvent) |@|
                  validateFirstPointsForTeam1(gameEvent) |@|
                  validateFirstPointsForTeam2(gameEvent) |@|
                  validateCurrentEventTiming(gameEvent) |@|
                  validateScoreForTeam1(gameEvent) |@|
                  validateScoreForTeam2(gameEvent) |@|
                  validateRecordedPointsForTeam1(gameEvent) |@|
                  validateRecordedPointsForTeam2(gameEvent)
      valid { (v1: Boolean, v2: Boolean, v3: Boolean, v4: Boolean, v5: Boolean, v6: Boolean, v7: Boolean, v8: Boolean, v9: Boolean, v10: Boolean) =>
        true
      }
    }

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

    def extractBinaryEventFormat(s: String): Try[String] = Try {
      Integer.parseInt(s.trim().drop(2), 16).toBinaryString.reverse.padTo(32, '0').reverse
    }

    extractBinaryEventFormat(gameEventStr) match {
      case Success(evStr) =>
        val event: Option[GameEvent] =  evStr match {
          case EventStreamPattern(time, totalPointsT1, totalPointsT2, whoScored, pointsScored) => {
            val event = GameEvent(Integer parseInt(time, 2), Integer.parseInt(totalPointsT1, 2), Integer.parseInt(totalPointsT2, 2), Integer.parseInt(whoScored, 2), Integer.parseInt(pointsScored, 2))
//            if (isValidEvent(event)) Some(event) else None
            if (validateEvent(event).isSuccess) Some(event) else None
          }
        }

        if (event.isDefined) events += event.get
        event
      case Failure(e) => None
    }
  }

  def allEvents = if (events.isEmpty) Seq() else events

  def lastEvent: Option[GameEvent] = events.lastOption

  def lastEvents(n: Int): Seq[GameEvent] = if (events.isEmpty || events.size <= n) events else events.takeRight(n)

  def lastTeamToScore = if (events.isEmpty) None else Some(events.last.whoScored)

  def lastScoredPoints: Option[Int] = if (events.isEmpty) None else Some(events.last.pointsScored)

  def timeForLastScoredPoints: Option[Int] = if (events.isEmpty) None else Some(events.last.time)

  def latestScore: Option[(Int, Int)] = if (events.isEmpty) None else Some((events.last.totalPointsTeam1, events.last.totalPointsTeam2))

  def eventsByTeam(team: Int): Seq[GameEvent] = events.filter(_.whoScored==team)

  def lastEventScoredByTeam(team: Int) = eventsByTeam(team).lastOption

}

