package com.company


import scala.io.Source

/**
 * Gaming service app
 *
 * Created by clelio on 23/06/15.
 */

object GamingServiceApp extends App {
  val gamingService = new GamingServiceImpl

  val source = Source.fromFile("src/main/resources/sample2.txt")

  // push events stream to gaming service
  // we could implement that via an Observable pattern (push based asynchronous mechanism)
  // using Akka Streams instead of a traditional pull based mechanism
  // including nice features, such as back pressure and resilience, i.e. Reactive Streams!
  source.getLines().foreach(gamingService.receive)

  println("Show all recorded valid events: ")
  gamingService.allEvents.foreach(println)

  println(s"\nShow last 5 recorded valid event: ")
  gamingService.lastEvents(5).foreach(println)

  val lastEvent = gamingService.lastEvent
  if (lastEvent.isDefined) println(s"\nShow last recorded valid event: ${lastEvent.get}")

}



