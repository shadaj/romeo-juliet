package me.shadaj.romeojuliet

import akka.actor.{Props, ActorSystem}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.StdIn

import scala.language.postfixOps

object Main extends App {
  val tragedySystem = ActorSystem("tragedy-system")
  val tragedy = tragedySystem.actorOf(Tragedy.props, Tragedy.Name)
  tragedySystem.awaitTermination()
}
