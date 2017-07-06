package com.company

import java.lang.Integer.parseInt

import scala.collection.immutable.Seq
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

  def invalidEvents: Seq[InvalidEvent]
}

case class InvalidEvent(event: GameEvent, reason: String)


class GamingServiceImpl extends GamingService {

  private val EventStreamPattern = """[01]{1}([01]{12})([01]{8})([01]{8})([01]{1})([01]{2})""".r

  // visibility to private object level only
  private[this] var events: Seq[GameEvent] = Seq()

  private[this] var _invalidEvents: Seq[InvalidEvent] = Seq()

  def receive(gameEventStr: String): Option[GameEvent] = {

    def validateEvent(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = {
      val toValidate = (lastEvent, gameEvent.time, 1 to 3 contains gameEvent.pointsScored, gameEvent.pointsScored, gameEvent.whoScored, gameEvent.totalPointsTeam1, gameEvent.totalPointsTeam2)

      def validatePoints(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (_, _, false, _, _, _, _) => failureNel(InvalidEvent(gameEvent, "Points scored outside possible input"))
        case _ => success(true)
      }

      def validateFirstPointsForTeam1NotComputed(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (None, _, true, points, 0, pointsT1, pointsT2) if pointsT1 != points => failureNel(InvalidEvent(gameEvent, "First points of the match scored for Team 1 not computed"))
        case _ => success(true)
      }

      def validateFirstPointsForTeam2NotComputed(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (None, _, true, points, 1, pointsT1, pointsT2) if pointsT2 != points => failureNel(InvalidEvent(gameEvent, "First points of the match scored for Team 2 not computed"))
        case _ => success(true)
      }

      def validateFirstPointsForTeam1(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (None, _, true, points, 0, pointsT1, pointsT2) if pointsT2 != 0 => failureNel(InvalidEvent(gameEvent, "First points of the match scored for Team 1. Invalid score for Team 2"))
        case _ => success(true)
      }

      def validateFirstPointsForTeam2(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (None, _, true, points, 1, pointsT1, pointsT2) if pointsT1 != 0 => failureNel(InvalidEvent(gameEvent, "First points of the match scored for Team 2. Invalid score for Team 1"))
        case _ => success(true)
      }

      def validateCurrentEventTiming(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), time, _, _, _, _, _) if e.time > time => failureNel(InvalidEvent(gameEvent, "Current event timing is invalid (> last recorded event)"))
        case _ => success(true)
      }

      def validateScoreForTeam1(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), _, true, points, 0, pointsT1, pointsT2) if pointsT1 != e.totalPointsTeam1 + points => failureNel(InvalidEvent(gameEvent, "Team 1 score is invalid"))
        case _ => success(true)
      }

      def validateScoreForTeam2(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), _, true, points, 1, pointsT1, pointsT2) if pointsT2 != e.totalPointsTeam2 + points => failureNel(InvalidEvent(gameEvent, "Team 2 score is invalid"))
        case _ => success(true)
      }

      def validateRecordedPointsForTeam1(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), _, true, points, 0, pointsT1, pointsT2) if pointsT2 != e.totalPointsTeam2 => failureNel(InvalidEvent(gameEvent, "Invalid registered points for Team 2"))
        case _ => success(true)
      }

      def validateRecordedPointsForTeam2(gameEvent: GameEvent): ValidationNel[InvalidEvent, Boolean] = toValidate match {
        case (Some(e), _, true, points, 1, pointsT1, pointsT2) if pointsT1 != e.totalPointsTeam1 => failureNel(InvalidEvent(gameEvent, "Invalid registered points for Team 2"))
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
        v1 && v2 && v3 && v4 && v5 && v6 && v7 && v8 && v9 && v10
      }
    }

    def extractBinaryEventFormat(s: String): Try[String] = Try {
      parseInt(s.trim().drop(2), 16).toBinaryString.reverse.padTo(32, '0').reverse
    }

    extractBinaryEventFormat(gameEventStr) match {
      case Success(evStr) =>
        val event = evStr match {
          case EventStreamPattern(time, totalPointsT1, totalPointsT2, whoScored, pointsScored) => {

            val event = GameEvent(parseInt(time, 2), parseInt(totalPointsT1, 2), parseInt(totalPointsT2, 2),
              parseInt(whoScored, 2), parseInt(pointsScored, 2))

            val validation: ValidationNel[InvalidEvent, Boolean] = validateEvent(event)

            if (validation.isSuccess)
              Some(event)
            else {
              validation match {
                case scalaz.Failure(failureList) => failureList.foreach(_invalidEvents :+= _)
                case scalaz.Success(_) => ()
              }
              None
            }
          }
        }

        // add to seq of events
        event.foreach(events :+= _)

        event

      case Failure(e) =>
        // TODO adding logging here
        None
    }
  }

  def allEvents = events

  def lastEvent: Option[GameEvent] = events.lastOption

  def lastEvents(n: Int): Seq[GameEvent] = if (events.isEmpty || events.size <= n) events else events.takeRight(n)

  def lastTeamToScore = if (events.isEmpty) None else Some(events.last.whoScored)

  def lastScoredPoints: Option[Int] = if (events.isEmpty) None else Some(events.last.pointsScored)

  def timeForLastScoredPoints: Option[Int] = if (events.isEmpty) None else Some(events.last.time)

  def latestScore: Option[(Int, Int)] = if (events.isEmpty) None else Some((events.last.totalPointsTeam1, events.last.totalPointsTeam2))

  def eventsByTeam(team: Int): Seq[GameEvent] = events.filter(_.whoScored == team)

  def lastEventScoredByTeam(team: Int) = eventsByTeam(team).lastOption

  def invalidEvents: Seq[InvalidEvent] = if (_invalidEvents.isEmpty) Seq() else _invalidEvents

}

