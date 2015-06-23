package main.scala

import scala.io.Source

/**
 * Gaming Service
 *
 * Created by clelio on 23/06/15.
 */
trait GamingService {
  def getEventsFromSource(s: Source): Seq[String] = s.getLines().toSeq

  def getEventsFromStdin: Seq[String] = io.Source.stdin.getLines().toSeq

  def getAllEvents: Seq[GameEvent]

  def lastEvent: Option[GameEvent]

  def lastEvents(n: Int): Seq[GameEvent]

  def printNicely(e: GameEvent): Unit
}

case class GameEvent(time: Int, totalPointsTeam1: Int, totalPointsTeam2: Int, whoScored: Int, pointsScored: Int)

class GamingServiceImpl extends GamingService {

  val EventStreamPattern = """[01]{1}([01]{12})([01]{8})([01]{8})([01]{1})([01]{2})""".r
  var events = Seq[GameEvent]()

  val source = Source.fromFile("src/main/resources/sample2.txt")
  val eventsFromSource: Seq[String] = getEventsFromSource(source)
//  val eventsFromSource: Seq[String] = getEventsFromStdin

  events ++= eventsFromSource.filter(_.trim()!="").map(_.drop(2)).map(Integer.parseInt(_, 16).toBinaryString.reverse.padTo(32, '0').reverse).flatMap {
//    case EventStreamPattern(_, totalPointsT1, totalPointsT2, _, pointsScored) if Integer.parseInt(pointsScored, 2) == 0 => None
    case EventStreamPattern(_, _, _, _, pointsScored) if Integer.parseInt(pointsScored, 2) == 0 => None
    case EventStreamPattern(time, totalPointsT1, totalPointsT2, whoScored, pointsScored) => Some(GameEvent(Integer.parseInt(time, 2), Integer.parseInt(totalPointsT1, 2), Integer.parseInt(totalPointsT2, 2), Integer.parseInt(whoScored, 2), Integer.parseInt(pointsScored, 2)))
    case _ => None
  }

  def getAllEvents = if (events.isEmpty) Seq() else events

  def lastEvent: Option[GameEvent] = if (events.isEmpty) None else Some(events.last)

  def lastEvents(n: Int): Seq[GameEvent] = if (events.isEmpty || events.size < n) Seq() else events.takeRight(n)

  def printNicely(e: GameEvent): Unit = {
    def printTime(time: Int): String = if (time < 60) s"$time secs" else s"${time/60}:${(time%60).toString.padTo(2, '0')}"
    def printPoints(points: Int): String = if (points == 1) s"single point" else s"$points-point shot"
    def printTeam(team: Int): String = if (team == 0) "Team 1" else "Team 2"
    println(s"At ${printTime(e.time)}, a ${printPoints(e.pointsScored)} for ${printTeam(e.whoScored)} was scored - match result: Team-1 ${e.totalPointsTeam1} vs ${e.totalPointsTeam2} Team-2")
  }
}

object GamingServiceApp extends App {

  val gamingService = new GamingServiceImpl

  gamingService.getAllEvents.foreach(gamingService.printNicely)

}



