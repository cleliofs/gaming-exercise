package com.company

/**
 * Game event case class
 *
 * Created by clelio on 24/06/15.
 */
case class GameEvent(time: Int, totalPointsTeam1: Int, totalPointsTeam2: Int, whoScored: Int, pointsScored: Int) {

  override def toString() = {
    def printTime(time: Int) = if (time < 60) s"$time secs" else s"${time/60}:${(time%60).toString.padTo(2, '0')}"
    def printPoints(points: Int) = if (points == 1) s"single point" else s"$points-point shot"
    def printTeam(team: Int) = if (team == 0) "Team 1" else "Team 2"
    s"At ${printTime(time)}, a ${printPoints(pointsScored)} for ${printTeam(whoScored)} was scored - match result: Team-1 $totalPointsTeam1 vs $totalPointsTeam2 Team-2"
  }

}
