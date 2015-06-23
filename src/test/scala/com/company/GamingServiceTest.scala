package com.company

import org.scalatest.FunSuite

/**
 *
 * Gaming Service test case
 *
 * Created by clelio on 23/06/15.
 */
class GamingServiceTest extends FunSuite {

  test("An empty event should not be recorded") {
    val underTest = new GamingServiceImpl
    val event = ""
    underTest.receive(event)
    val events = underTest.allEvents
    assert(events.size == 0)
  }

  test("The first valid event should be recorded properly") {
    val underTest = new GamingServiceImpl
    val event = "0x801002"
    underTest.receive(event)
    val events = underTest.allEvents
    assert(events.size == 1)
    val lastEvent = underTest.lastEvent
    assert(lastEvent != null)
  }

  test("The second valid event should be recorded properly") {
    val underTest = new GamingServiceImpl
    val event1 = "0x801002"
    underTest.receive(event1)
    assert(underTest.allEvents.size == 1)

    val event2 = "0xf81016"
    underTest.receive(event2)
    assert(underTest.allEvents.size == 2)

    val lastEvent = underTest.lastEvent
    assert(lastEvent != null)
  }

  test("Gaming service receiver should ignore events out of order") {
    val underTest = new GamingServiceImpl
    val event2 = "0xf81016"
    underTest.receive(event2)
    assert(underTest.allEvents.size == 0, event2)
  }

  test("Gaming service should acknowledge all events from sample1.txt") {
    val underTest = new GamingServiceImpl
    val events = io.Source.fromFile("src/test/resources/sample1.txt").getLines()
    events.foreach(underTest.receive)
    assert(underTest.allEvents.size == 28)
  }

  test("The last 5 events from sample1.txt should matched") {
    val underTest = new GamingServiceImpl
    val events = io.Source.fromFile("src/test/resources/sample1.txt").getLines()
    events.foreach(underTest.receive)
    val allEvents = underTest.allEvents
    assert(allEvents.size == 28)
    
    val lastEvents = underTest.lastEvents(5)
    assert(lastEvents.size == 5)

    assert(allEvents.takeRight(5) == lastEvents)
  }

  test("Gaming service should acknowledge only 9 events from sample2.txt") {
    val underTest = new GamingServiceImpl
    val events = io.Source.fromFile("src/test/resources/sample2.txt").getLines()
    events.foreach(underTest.receive)
    assert(underTest.allEvents.size == 9)
  }

  test("For all 9 events from sample2.txt game service should match the latest 5 events") {
    val underTest = new GamingServiceImpl
    val events = io.Source.fromFile("src/test/resources/sample2.txt").getLines()
    events.foreach(underTest.receive)

    val allEvents = underTest.allEvents
    assert(allEvents.size == 9)
    assert(underTest.lastEvents(9) == allEvents)
  }


  test("Game service should ignore nicely a request for latest when it is more than total recorded events") {
    val underTest = new GamingServiceImpl
    val events = io.Source.fromFile("src/test/resources/sample2.txt").getLines()
    events.foreach(underTest.receive)

    val allEvents = underTest.allEvents
    assert(allEvents.size == 9)
    assert(underTest.lastEvents(99) == Seq())
  }

  test("Last team to score in the match for sample2.txt file") {
    val underTest = new GamingServiceImpl
    val events = io.Source.fromFile("src/test/resources/sample2.txt").getLines()
    events.foreach(underTest.receive)
    assert(underTest.allEvents.size == 9)

    assert(underTest.lastTeamToScore.get == 0)
  }

  test("Last scored points in the match for sample2.txt file") {
    val underTest = new GamingServiceImpl
    assert(underTest.lastScoredPoints == None)

    val events = io.Source.fromFile("src/test/resources/sample2.txt").getLines()
    events.foreach(underTest.receive)
    assert(underTest.allEvents.size == 9)

    assert(underTest.lastScoredPoints.get == 1)
  }

  test("At what time through the match was the last point scored in the match for sample2.txt file") {
    val underTest = new GamingServiceImpl
    assert(underTest.timeForLastScoredPoints == None)

    val events = io.Source.fromFile("src/test/resources/sample2.txt").getLines()
    events.foreach(underTest.receive)
    assert(underTest.allEvents.size == 9)

    assert(underTest.timeForLastScoredPoints.get == 168) // "2:48"
  }

  test("What is the latest the score in the match for sample2.txt file") {
    val underTest = new GamingServiceImpl
    assert(underTest.latestScore == None)

    val events = io.Source.fromFile("src/test/resources/sample2.txt").getLines()
    events.foreach(underTest.receive)
    assert(underTest.allEvents.size == 9)

    assert(underTest.latestScore.get == (8, 11)) // Team-1 8 vs 11 Team-2
  }


}
