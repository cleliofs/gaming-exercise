package com.company

import scala.io.Source

/**
 * Gaming service app to test the game event streams processing from
 * the standard input (i.e. console).
 *
 * Created by clelio on 23/06/15.
 */

object GamingServiceApp2 extends App {
  val gamingService = new GamingServiceImpl

  private val lines: Iterator[String] = Source.stdin.getLines()

  // read from standard input source
  while (true) {
    val event = lines.next()
    gamingService.receive(event)

    println("Show all recorded valid events: ")
    gamingService.allEvents.foreach(println)

    println(s"\nShow last 5 recorded valid event: ")
    gamingService.lastEvents(5).foreach(println)

    val lastEvent = gamingService.lastEvent
    if (lastEvent.isDefined) println(s"\nShow last recorded valid event: ${lastEvent.get}")

    println(s"\nShow all events for Team 1: ")
    val allEventsByTeam1 = gamingService.eventsByTeam(0)
    allEventsByTeam1.foreach(println)
  }

}



