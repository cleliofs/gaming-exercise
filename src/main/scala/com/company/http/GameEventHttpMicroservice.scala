package com.company.http

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.company.{GameEvent, GamingServiceImpl}
import com.typesafe.config.{Config, ConfigFactory}
import spray.json.DefaultJsonProtocol

import scala.concurrent.{ExecutionContextExecutor, Future}

/**
 * Game event Akka http microservice
 *
 * Created by clelio on 28/06/15.
 */

case class GameEventOutput(events: Seq[String])

trait Protocols extends DefaultJsonProtocol {
  implicit val gameEventOutputFormat = jsonFormat1(GameEventOutput.apply)
  implicit val gameEventFormat = jsonFormat5(GameEvent.apply)
}

trait Service extends Protocols {
  implicit val system: ActorSystem
  implicit def executor: ExecutionContextExecutor
  implicit val materializer: ActorMaterializer

  def config: Config
  val logger: LoggingAdapter

  val gamingService = new GamingServiceImpl

  def recordGameEvent(gameEventStr: String): Future[Either[String, GameEvent]] = {
    gamingService.receive(gameEventStr) match {
      case Some(event) => Future.successful(Right(event))
      case None => Future.successful(Left(s"Event discarded!\n"))
    }

  }

  def fetchGameEvents(): Future[Either[String, GameEventOutput]] = {
    val events = gamingService.allEvents
    val res: GameEventOutput = GameEventOutput(events.map(_.toString()))
    Future.successful(Right(res))
  }

  def fetchLastGameEvent(): Future[Either[String, GameEventOutput]] = {
    val res = gamingService.lastEvent.getOrElse("No recorded events!")
    Future.successful(Right(GameEventOutput(Seq(res.toString))))
  }


//  private val handleAsPartialFunction: PartialFunction[Either, Any] = {
//    case Right(gameEvent) => gameEvent
//    case Left(errorMessage) => BadRequest -> errorMessage
//  }

  val routes: Route = {
    logRequestResult("game-event-http-microservice") {
      path("input" / Segment ~ Slash.?) { event =>
        post {
          complete {
            recordGameEvent(event).map[ToResponseMarshallable] {
              case Right(gameEvent) => gameEvent
              case Left(errorMessage) => BadRequest -> errorMessage
            }
          }
        }
      } ~
      path("events" ~(PathEnd | Slash)) {
        get {
          complete {
            fetchGameEvents().map[ToResponseMarshallable] {
              case Right(gameEvent) => gameEvent
              case Left(errorMessage) => BadRequest -> errorMessage
            }
          }
        }
      } ~
      path("events" / "last" ~ Slash.?) {
        get {
          complete {
            fetchLastGameEvent().map[ToResponseMarshallable] {
              case Right(gameEvent) => gameEvent
              case Left(errorMessage) => BadRequest -> errorMessage
            }
          }
        }
      }
    }
  }
}

object GameEventHttpMicroservice extends App with Service{
  override implicit val system = ActorSystem()
  override implicit def executor: ExecutionContextExecutor = system.dispatcher
  override implicit val materializer: ActorMaterializer = ActorMaterializer()

  override def config: Config = ConfigFactory.load()
  override val logger: LoggingAdapter = Logging(system, getClass)

  Http().bindAndHandle(routes, config.getString("http.interface"), config.getInt("http.port"))
}
